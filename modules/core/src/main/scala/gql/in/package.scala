package gql

import cats.implicits._
import cats.data._
import io.circe._
import Value._
import cats._
import scala.reflect.ClassTag

package object in {
  sealed trait Input[+A] {
    def decode(value: Value): Either[String, A]
  }

  sealed trait Toplevel[A] extends Input[A] {
    def name: String
  }

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

  final case class Obj[A](
      name: String,
      fields: Arg[A]
  ) extends Toplevel[A] {
    def decode(value: Value): Either[String, A] = {
      value match {
        case JsonValue(jo) if jo.isObject =>
          val m = jo.asObject.get.toMap

          fields.entries
            .traverse { a =>
              m
                .get(a.name)
                .map(x => a.input.decode(JsonValue(x))) match {
                case Some(outcome) => outcome
                case None          => a.default.toRight(s"missing field ${a.name} in input object $name")
              }
            }
            .map(_.toList)
            .map { xs =>
              val (_, o) = fields.decode(xs.asInstanceOf[List[Any]])
              o
            }
        case ObjectValue(xs) =>
          fields.entries
            .traverse { a =>
              xs
                .get(a.name)
                .map(x => a.input.decode(x)) match {
                case Some(outcome) => outcome
                case None          => a.default.toRight(s"missing field ${a.name} in input object $name")
              }
            }
            .map(_.toList)
            .map { xs =>
              val (_, o) = fields.decode(xs.asInstanceOf[List[Any]])
              o
            }
        case _ => Left(s"expected object for $name, got ${value.name}")
      }
    }
  }

  final case class Scalar[A](name: String, dec: Decoder[A]) extends Toplevel[A] {
    override def decode(value: Value): Either[String, A] =
      dec.decodeJson(value.asJson).leftMap(_.show)
  }

  final case class Enum[A](name: String, fields: NonEmptyMap[String, A]) extends Toplevel[A] {
    def decodeString(s: String): Either[String, A] =
      fields.lookup(s) match {
        case Some(a) => Right(a)
        case None    => Left(s"unknown value $s for enum $name")
      }

    override def decode(value: Value): Either[String, A] =
      value match {
        case JsonValue(v) if v.isString => decodeString(v.asString.get)
        case EnumValue(s)               => decodeString(s)
        case _                          => Left(s"expected enum $name, got ${value.name}")
      }
  }
}
