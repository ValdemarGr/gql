package gql.preparation

import cats._
import cats.data._
import gql.parser.{QueryAst => QA, Value => V, AnyValue, Const}
import gql.SchemaShape
import gql.parser.QueryAst

trait RootPreparation[F[_], G[_], P[_]] {
  def pickRootOperation(
      ops: List[P[QA.OperationDefinition[P]]],
      operationName: Option[String]
  ): F[QA.OperationDefinition[P]]

  def variables(
      op: QA.OperationDefinition[P]
  ): F[VariableMap]

  def prepareRoot[Q, M, S](
      op: QA.OperationDefinition[P],
      frags: List[P[QA.FragmentDefinition[P]]],
      schema: SchemaShape[G, Q, M, S]
  ): F[PreparedRoot[G, Q, M, S]]
}

object RootPreparation {
  def apply[F[_], G[_], P[_], C](implicit
      F: Monad[F],
      EA: ErrorAlg[F, C],
      P: Positioned[P, C]
  ) = {
    import EA._

    new RootPreparation[F, G, P] {
      override def pickRootOperation(
          ops: List[P[QueryAst.OperationDefinition[P]]],
          operationName: Option[String]
      ): F[QueryAst.OperationDefinition[P]] = {
        lazy val applied = ops.map(P(_))

        lazy val positions = ops.map(P.position(_))

        lazy val possible = applied
          .collect { case d: QA.OperationDefinition.Detailed[P] => d.name }
          .collect { case Some(x) => s"'$x'" }
          .mkString(", ")
        (ops, operationName) match {
          case (Nil, _)      => raise(s"No operations provided.", Nil)
          case (x :: Nil, _) => F.pure(P(x))
          case (_, _) if applied.exists {
                case _: QA.OperationDefinition.Simple[P]                     => true
                case x: QA.OperationDefinition.Detailed[P] if x.name.isEmpty => true
                case _                                                       => false
              } =>
            raise(s"Exactly one operation must be suplied if the operations include at least one unnamed operation.", positions)
          case (_, None) =>
            raise(s"Operation name must be supplied when supplying multiple operations, provided operations are $possible.", positions)
          case (_, Some(name)) =>
            val o = applied.collectFirst { case d: QA.OperationDefinition.Detailed[P] if d.name.contains(name) => d }
            raiseOpt(o, s"Unable to find operation '$name', provided possible operations are $possible.", positions)
        }
      }

      override def variables(op: QueryAst.OperationDefinition[P]): F[VariableMap] = ???

      override def prepareRoot[Q, M, S](
          op: QueryAst.OperationDefinition[P],
          frags: List[P[QueryAst.FragmentDefinition[P]]],
          schema: SchemaShape[G, Q, M, S]
      ): F[PreparedRoot[G, Q, M, S]] = ???

    }
  }
}

sealed trait PreparedRoot[G[_], Q, M, S]
object PreparedRoot {
  final case class Query[G[_], Q, M, S](query: NonEmptyList[PreparedField[G, Q]]) extends PreparedRoot[G, Q, M, S]
  final case class Mutation[G[_], Q, M, S](mutation: NonEmptyList[PreparedField[G, M]]) extends PreparedRoot[G, Q, M, S]
  final case class Subscription[G[_], Q, M, S](subscription: NonEmptyList[PreparedField[G, S]]) extends PreparedRoot[G, Q, M, S]
}
