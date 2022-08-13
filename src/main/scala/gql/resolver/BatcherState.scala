package gql.resolver

final case class BatchingState[F[_]](
    nextId: Int,
    batchers: Map[Int, Set[Any] => F[Map[Any, Any]]]
)
