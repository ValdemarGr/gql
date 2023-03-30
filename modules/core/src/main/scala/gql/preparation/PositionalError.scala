package gql.preparation

import gql.Cursor
import cats.parse.Caret
import cats.data._
import cats.implicits._

final case class PositionalError[C](position: Cursor, caret: List[C], message: String)

object PositionalError {
  import io.circe.syntax._
  import io.circe._
  implicit val encoder: Encoder.AsObject[PositionalError[Caret]] = Encoder.AsObject.instance[PositionalError[Caret]] { pe =>
    Map(
      "message" -> Some(pe.message.asJson),
      "locations" -> pe.caret.map(c => Json.obj("line" -> c.line.asJson, "column" -> c.col.asJson)).toNel.map(_.asJson),
      "path" -> NonEmptyChain.fromChain(pe.position.path.map(_.asString)).map(_.asJson)
    ).collect { case (k, Some(v)) => k -> v }.asJsonObject
  }
}
