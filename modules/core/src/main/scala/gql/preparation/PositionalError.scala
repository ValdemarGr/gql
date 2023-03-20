package gql.preparation

import gql.Cursor

final case class PositionalError[C](position: Cursor, caret: List[C], message: String)