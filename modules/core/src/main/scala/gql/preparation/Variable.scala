package gql.preparation

import io.circe._
import gql.parser.{Value => V}
import gql.parser.Const

  final case class Variable[C](
      tpe: gql.parser.Type,
      value: Either[Json, V[Const, C]]
  )
