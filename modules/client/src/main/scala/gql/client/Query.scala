package gql.client

import gql.parser.QueryParser
import io.circe._

final case class SimpleQuery[A](
    operationType: QueryParser.OperationType,
    selectionSet: SelectionSet[A]
) {
    def compile: (String, Json) = ???
}

final case class NamedQuery[A](name: String, query: SimpleQuery[A]) {
    def compile: (String, Json) = ???
}

final case class NamedParameterizedQuery[A, V](
    name: String,
    query: SimpleQuery[A],
    variables: Var.Impl[V]
) {
    def compile(variables: V): (String, Json) = ???
}