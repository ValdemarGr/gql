package gql.dslutil

import cats.data._
import gql.ast._

trait Aliases {
  type Fields[F[_], -A] = NonEmptyList[(String, Field[F, A, ?])]

  type AbstractFields[F[_]] = NonEmptyList[(String, AbstractField[F, ?])]

  type AnyFields[F[_], -A] = NonEmptyList[(String, AnyField[F, A, ?])]
}

object Aliases extends Aliases