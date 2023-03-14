package gql.parser

  sealed trait Type
  sealed  trait NonNullType extends Type
  object Type {
    final case class Named(name: String) extends NonNullType
    final case class List(of: Type) extends NonNullType
    final case class NonNull(of: NonNullType) extends Type
  }
