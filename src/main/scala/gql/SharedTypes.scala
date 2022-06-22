package gql

import cats.implicits._
import cats.data._
import io.circe._
import Value._
import cats._
import scala.reflect.ClassTag

object SharedTypes {
  final case class ScalarCodec[A](name: String, encoder: Encoder[A], decoder: Decoder[A])
  final case class EnumCodec[A](name: String, fields: NonEmptyMap[String, A])
}
