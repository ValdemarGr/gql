package gql

import cats.implicits._
import cats.data._
import io.circe._

object Types {
  sealed trait Input[A] {
    def decode(json: Json): Either[String, A]
  }

  object Input {
    final case class Optional[A](of: Input[A]) extends Input[Option[A]] {
      def decode(json: Json): Either[String, Option[A]] =
        if (json.isNull) Right(None)
        else of.decode(json).map(Some(_))
    }
    final case class Object[A](
        name: String,
        fields: NonEmptyList[Object.Field[_]],
        decoder: Map[String, _] => A
    ) extends Input[A] {
      def addField[B](newField: Object.Field[B]): Object[(A, B)] =
        Object(name, newField :: fields, m => (decoder(m), m(newField.name).asInstanceOf[B]))

      def decode(json: Json): Either[String, A] =
        json.asObject
          .toRight(s"expected object for $name, got ${json.name}")
          .map(_.toMap)
          .flatMap { m =>
            fields
              .traverse { field =>
                val res =
                  m
                    .get(field.name)
                    .map(field.tpe.decode) match {
                    case Some(outcome) => outcome
                    case None          => field.default.toRight(s"missing field ${field.name} in input object $name")
                  }

                res.map(field.name -> _)
              }
          }
          .map(_.toList.toMap)
          .map(decoder)
    }
    object Object {
      final case class Field[A](
          name: String,
          tpe: Input[A],
          default: Option[A] = None
      )
    }
  }

  sealed trait Output[A]

  object Output {}

  final case class Scalar[A](name: String, decoder: Decoder[A]) extends Input[A] {
    def decode(json: Json): Either[String, A] = decoder.decodeJson(json).leftMap(_.show)
  }

  final case class Enum[A](name: String, fields: NonEmptyMap[String, A]) extends Input[A] {
    def decode(json: Json): Either[String, A] =
      json.asString match {
        case None => Left(s"expected type string for enum $name, but got ${json.name}")
        case Some(str) =>
          fields.lookup(str) match {
            case None    => Left(s"failed to find enum value $str for enum $name. Possible values are ${fields.keys.mkString_(",")}")
            case Some(a) => Right(a)
          }
      }
  }
}
