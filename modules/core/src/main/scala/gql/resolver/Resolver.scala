package gql.resolver

final class Resolver[F[_], -I, O] (private val underlying: Step[F, I, O]) {
}

object Resolver {
}
