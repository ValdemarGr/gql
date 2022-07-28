package gql

final case class FieldMetadata[F[_], I](
    batchName: Option[String] = None,
    cost: Option[I => F[Long]] = None,
    description: Option[String] = None
)
