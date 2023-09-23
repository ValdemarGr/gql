package gql.parser

import cats.parse._
import cats._
import io.circe.syntax._
import io.circe._

final case class ParseError(
    caret: Caret,
    prettyError: Eval[String]
)

object ParseError {
  implicit val encoder: Encoder.AsObject[ParseError] = Encoder.AsObject.instance[ParseError] { err =>
    Map(
      "message" -> "could not parse query".asJson,
      "locations" -> List(Map("line" -> err.caret.line.asJson, "column" -> err.caret.col.asJson)).asJson,
      "error" -> err.prettyError.value.asJson
    ).asJsonObject
  }
}
