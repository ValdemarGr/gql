package gql.dsl

trait GqlDslFull
  extends DirectiveDslFull
  with EnumDslFull
  with FieldDslFull
  with InputDslFull
  with InterfaceDslFull
  with TypeDslFull
  with UnionDslFull

trait GqlDsl[F[_]]
  extends DirectiveDsl[F]
  with EnumDslFull
  with FieldDsl[F]
  with InputDslFull
  with InterfaceDsl[F]
  with TypeDsl[F]
  with UnionDsl[F]

object GqlDsl extends GqlDslFull {
  def apply[F[_]]: GqlDsl[F] = new GqlDsl[F] {}
}
