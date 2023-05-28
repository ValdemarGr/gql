package gql

import gql.resolver.Meta

final case class Directive[A](
    name: String
)

object Directive {
    sealed trait Position[A]
    object Position {
        final case class Field[A](
            enact: (
                Meta,
                A,
                ast.Field[f, ?, ?]
            ) => Unit
        ) extends Position[A]
    }
}