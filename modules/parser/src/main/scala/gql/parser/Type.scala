package gql.parser

  sealed trait Type
  object Type {
    final case class Named(name: String) extends Type
    final case class List(of: Type) extends Type
    final case class NonNull(of: Type) extends Type
  }