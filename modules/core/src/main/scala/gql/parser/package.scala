package gql

import cats.parse._
import cats._
import cats.data._
import cats.implicits._
import io.circe._

package object parser {
  final case class ParseError(
      caret: Caret,
      prettyError: Eval[String]
  ) {
    lazy val asGraphQL = {
      import io.circe.syntax._
      JsonObject(
        "message" -> "could not parse query".asJson,
        "locations" -> Json.arr(Json.obj("line" -> caret.line.asJson, "column" -> caret.col.asJson)),
        "error" -> prettyError.value.asJson
      )
    }
  }

  def parse(str: String): Either[ParseError, NonEmptyList[QueryParser.ExecutableDefinition]] = {
    QueryParser.executableDefinition.rep
      .parseAll(str)
      .leftMap { err =>
        val offset = err.failedAtOffset
        val left = str.take(offset)
        val nls = left.split("\n")
        val line = nls.size
        val col = nls.last.size

        ParseError(
          Caret(line, col, offset),
          Eval.later(ParserUtil.errorMessage(str, err))
        )
      }
  }
}
