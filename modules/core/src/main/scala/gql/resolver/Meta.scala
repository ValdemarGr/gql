package gql.resolver

import gql._
import gql.parser.{QueryParser => P}
import gql.interpreter.Cursor

final case class Meta(
    cursor: Cursor,
    alias: Option[String],
    args: Option[P.Arguments],
    variables: PreparedQuery.VariableMap
)
