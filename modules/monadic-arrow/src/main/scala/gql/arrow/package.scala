package gql

import cats.free.Free
import gql.resolver._

package object arrow {
  type ResolverDeclAlg[F[_], A] = DeclAlg[Resolver[F, *, *], A]
  type ResolverDecl[F[_], A] = Free[ResolverDeclAlg[F, *], A]
}
