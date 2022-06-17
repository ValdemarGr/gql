package gql

import cats.implicits._
import cats.data._
import io.circe._
import Value._

object Types {
  sealed trait Input[A] {
    def decode(value: Value): Either[String, A]
  }

  object Input {
    final case class Optional[A](of: Input[A]) extends Input[Option[A]] {
      def decode(value: Value): Either[String, Option[A]] =
        if (value.asJson.isNull) Right(None)
        else of.decode(value).map(Some(_))
    }
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
    def decode(value: Value): Either[String, A] = decoder.decodeJson(value.asJson).leftMap(_.show)
  }

  final case class Enum[A](name: String, fields: NonEmptyMap[String, A]) extends Input[A] {
    def decodeString(s: String): Either[String, A] =
      fields.lookup(s) match {
        case Some(a) => Right(a)
        case None    => Left(s"unknown value $s for enum $name")
      }

    def decode(value: Value): Either[String, A] =
      value match {
        case JsonValue(v) if v.isString => decodeString(v.asString.get)
        case EnumValue(s)               => decodeString(s)
        case _                          => Left(s"expected enum $name, got ${value.name}")
      }
  }
}
