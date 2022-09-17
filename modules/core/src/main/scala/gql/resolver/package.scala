package gql

import cats.data._

package object resolver {
  type ResultF[F[_], A] = IorT[F, String, A]

  type Result[A] = Ior[String, A]
}
