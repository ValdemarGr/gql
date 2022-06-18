package gql

import cats.implicits._
import cats.data._
import io.circe._
import Value._
import cats._

object Types {
  final case class Schema[F[_], Q](
      query: Output.Object[F, Q],
      types: Map[String, ToplevelOutput[F, _]]
  )

  sealed trait Input[A] {
    def decode(value: Value): Either[String, A]
  }

  object Input {
    final case class Arr[A](of: Input[A]) extends Input[Vector[A]] {
      def decode(value: Value): Either[String, Vector[A]] =
        value match {
          case JsonValue(ja) if ja.isArray =>
            ja.asArray.get.traverse(j => of.decode(Value.JsonValue(j)))
          case ArrayValue(v) => v.traverse(of.decode)
          case _             => Left(s"expected array type, get ${value.name}")
        }
    }

    final case class Opt[A](of: Input[A]) extends Input[Option[A]] {
      def decode(value: Value): Either[String, Option[A]] =
        if (value.asJson.isNull) Right(None)
        else of.decode(value).map(Some(_))
    }

    // optimization, use a stack instead of a map since we know the order of decoders
    final case class Object[A](
        name: String,
        fields: NonEmptyList[Object.Field[_]],
        decoder: Map[String, _] => A
    ) extends Input[A] {
      def addField[B](newField: Object.Field[B]): Object[(A, B)] =
        Object(name, newField :: fields, m => (decoder(m), m(newField.name).asInstanceOf[B]))

      def decode(value: Value): Either[String, A] = {
        value match {
          case JsonValue(jo) if jo.isObject =>
            val m = jo.asObject.get.toMap

            fields
              .traverse { field =>
                val res =
                  m
                    .get(field.name)
                    .map(x => field.tpe.decode(JsonValue(x))) match {
                    case Some(outcome) => outcome
                    case None          => field.default.toRight(s"missing field ${field.name} in input object $name")
                  }

                res.map(field.name -> _)
              }
              .map(_.toList.toMap)
              .map(decoder)

          case ObjectValue(xs) =>
            fields
              .traverse { field =>
                val res =
                  xs
                    .get(field.name)
                    .map(field.tpe.decode) match {
                    case Some(outcome) => outcome
                    case None          => field.default.toRight(s"missing field ${field.name} in input object $name")
                  }

                res.map(field.name -> _)
              }
              .map(_.toList.toMap)
              .map(decoder)
          case _ => Left(s"expected object for $name, got ${value.name}")
        }
      }
    }
    object Object {
      final case class Fields[A](
          fields: NonEmptyVector[Object.Field[_]],
          decoder: List[_] => (List[_], A)
      )
      object Fields {
        def apply[A](field: Field[A]): Fields[A] =
          Fields(
            NonEmptyVector.one(field),
            { s => (s.tail, s.head.asInstanceOf[A]) }
          )
      }
      implicit lazy val applyForFields = new Apply[Fields] {
        override def map[A, B](fa: Fields[A])(f: A => B): Fields[B] =
          Fields(fa.fields, fa.decoder andThen { case (s, a) => (s, f(a)) })

        override def ap[A, B](ff: Fields[A => B])(fa: Fields[A]): Fields[B] =
          Fields(
            ff.fields ++: fa.fields,
            { s1 =>
              val (s2, f) = ff.decoder(s1)
              val (s3, a) = fa.decoder(s2)
              (s3, f(a))
            }
          )
      }

      final case class Field[A](
          name: String,
          tpe: Input[A],
          default: Option[A] = None
      )
    }

    final case class Scalar[A](codec: ScalarCodec[A]) extends Input[A] {
      override def decode(value: Value): Either[String, A] =
        codec.decoder.decodeJson(value.asJson).leftMap(_.show)
    }

    final case class Enum[A](codec: EnumCodec[A]) extends Input[A] {
      def decodeString(s: String): Either[String, A] =
        codec.fields.lookup(s) match {
          case Some(a) => Right(a)
          case None    => Left(s"unknown value $s for enum ${codec.name}")
        }

      override def decode(value: Value): Either[String, A] =
        value match {
          case JsonValue(v) if v.isString => decodeString(v.asString.get)
          case EnumValue(s)               => decodeString(s)
          case _                          => Left(s"expected enum ${codec.name}, got ${value.name}")
        }
    }
  }

  sealed trait Output[F[_], +A] {
    def mapK[G[_]](fk: F ~> G): Output[G, A]

    def name: String
  }

  sealed trait ToplevelOutput[F[_], +A] extends Output[F, A]

  object Output {
    final case class Arr[F[_], A](of: Output[F, A]) extends Output[F, Vector[A]] {
      def mapK[G[_]](fk: F ~> G): Output[G, Vector[A]] = Arr(of.mapK(fk))

      def name: String = s"[${of.name}]"
    }

    final case class Opt[F[_], A](of: Output[F, A]) extends Output[F, Option[A]] {
      def mapK[G[_]](fk: F ~> G): Output[G, Option[A]] = Opt(of.mapK(fk))

      def name: String = s"(${of.name} | null)"
    }

    final case class Object[F[_], A](
        name: String,
        fields: NonEmptyList[(String, Object.Field[F, A, _])]
    ) extends Output[F, A]
        with ToplevelOutput[F, A] {
      def mapK[G[_]](fk: F ~> G): Object[G, A] =
        Object(name, fields.map { case (k, v) => k -> v.mapK(fk) })
    }
    object Object {
      sealed trait Resolution[F[_], +A] {
        def mapK[G[_]](fk: F ~> G): Resolution[G, A]
      }
      final case class PureResolution[F[_], +A](value: A) extends Resolution[F, A] {
        override def mapK[G[_]](fk: F ~> G): Resolution[G, A] =
          PureResolution(value)
      }
      final case class DeferredResolution[F[_], A](f: F[A]) extends Resolution[F, A] {
        override def mapK[G[_]](fk: F ~> G): Resolution[G, A] =
          DeferredResolution(fk(f))
      }

      sealed trait Field[F[_], I, T] {
        def output: Eval[Output[F, T]]

        def mapK[G[_]](fk: F ~> G): Field[G, I, T]
      }

      final case class SimpleField[F[_], I, T](
          resolve: I => Resolution[F, T],
          output: Eval[Output[F, T]]
      ) extends Field[F, I, T] {
        def mapK[G[_]](fk: F ~> G): Field[G, I, T] =
          SimpleField(resolve.andThen(_.mapK(fk)), output.map(_.mapK(fk)))
      }

      final case class Arg[A](
          name: String,
          input: Input[A],
          default: Option[A] = None
      )

      final case class Args[A](
          entries: NonEmptyVector[Arg[_]],
          decode: List[_] => (List[_], A)
      )
      object Args {
        def apply[A](entry: Arg[A]): Args[A] =
          Args(NonEmptyVector.one(entry), { s => (s.tail, s.head.asInstanceOf[A]) })
      }

      // pure/point does not make sense for args
      implicit lazy val applyForArgs = new Apply[Args] {
        override def map[A, B](fa: Args[A])(f: A => B): Args[B] =
          fa.copy(decode = fa.decode andThen { case (s, a) => (s, f(a)) })

        override def ap[A, B](ff: Args[A => B])(fa: Args[A]): Args[B] =
          Args(
            ff.entries ++: fa.entries,
            { s1 =>
              val (s2, f) = ff.decode(s1)
              val (s3, a) = fa.decode(s2)
              (s3, f(a))
            }
          )
      }

      final case class ArgField[F[_], I, T, A](
          args: Args[A],
          resolve: (I, A) => Resolution[F, T],
          output: Eval[Output[F, T]]
      ) extends Field[F, I, T] {
        def mapK[G[_]](fk: F ~> G): Field[G, I, T] =
          ArgField[G, I, T, A](
            args,
            (i, a) => resolve(i, a).mapK(fk),
            output.map(_.mapK(fk))
          )
      }
    }

    final case class Union[F[_], A](
        name: String,
        types: NonEmptyList[Object[F, A]]
    ) extends Output[F, A]
        with ToplevelOutput[F, A] {
      def mapK[G[_]](fk: F ~> G): Union[G, A] =
        Union(
          name,
          types.map(_.mapK(fk))
        )
    }

    final case class Scalar[F[_], A](codec: ScalarCodec[A]) extends Output[F, A] with ToplevelOutput[F, A] {
      override def mapK[G[_]](fk: F ~> G): Scalar[G, A] =
        Scalar(codec)

      override def name: String = codec.name
    }

    final case class Enum[F[_], A](codec: EnumCodec[A]) extends Output[F, A] with ToplevelOutput[F, A] {
      override def mapK[G[_]](fk: F ~> G): Output[G, A] =
        Enum(codec)

      lazy val reverseMapping =
        codec.fields.toNel.toList.map { case (k, v) => v -> k }.toMap

      def encode(a: A): Either[String, String] =
        reverseMapping.get(a) match {
          case None        => Left(s"unknown enum value $a for enum type ${codec.name}")
          case Some(value) => Right(value)
        }

      override def name: String = codec.name
    }
  }

  final case class ScalarCodec[A](name: String, encoder: Encoder[A], decoder: Decoder[A])

  final case class EnumCodec[A](name: String, fields: NonEmptyMap[String, A])
}
