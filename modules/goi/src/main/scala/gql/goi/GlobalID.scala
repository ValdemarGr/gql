package gql.goi

import gql.resolver.Resolver

trait GlobalID[F[_], T, K] {
  type Key = K

  def typename: String

  def codec: IDCodec[Key]

  def toId: Resolver[F, T, Key]

  def fromId: Key => F[Option[T]]
}

object GlobalID {
  def apply[F[_], T, A](typename: String, toId: Resolver[F, T, A], fromId: A => F[Option[T]])(implicit
      codec: IDCodec[A]
  ): GlobalID[F, T, A] = {
    val typename0 = typename
    val toId0 = toId
    val fromId0 = fromId(_)
    val codec0 = codec
    new GlobalID[F, T, A] {
      override def typename: String = typename0

      override def codec: IDCodec[A] = codec0

      override def toId: Resolver[F, T, A] = toId0

      override def fromId: A => F[Option[T]] = fromId0
    }
  }
}
