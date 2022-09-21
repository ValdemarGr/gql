package gql

import cats.data._

package object resolver {
  type ResultF[F[_], A] = F[Result[A]]

  type Result[A] = IorNec[String, A]
}
