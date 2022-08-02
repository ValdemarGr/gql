package gql

final case class FieldMetadata[F[_], I](
    batchName: Option[String] = None,
    description: Option[String] = None
)
