package gql

import cats.parse._
import cats._
import cats.data._
import cats.implicits._

package object parser {
  final case class ParseError(
      caret: Caret,
      prettyError: Eval[String]
  )

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
