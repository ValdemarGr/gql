package gql

import io.circe._

final case class QueryParameters(
    query: String,
    variables: Option[Map[String, Json]],
    operationName: Option[String]
)
