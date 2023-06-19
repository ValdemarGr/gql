package gql

import cats.data._
import gql.ast._

package object dsl extends GqlDslFull {
  type Fields[F[_], -A] = NonEmptyList[(String, Field[F, A, ?])]

  type AbstractFields[F[_]] = NonEmptyList[(String, AbstractField[F, ?])]
}
