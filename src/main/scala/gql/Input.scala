package gql

import cats.implicits._
import cats.data._
import io.circe._
import Value._
import cats._
import scala.reflect.ClassTag

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
