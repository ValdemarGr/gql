package gql.client

import gql.std.FreeApply
import cats.implicits._
import cats._
import cats.data._

final case class VariableClosure[A, V](
    variables: Var.Impl[V],
    query: SelectionSet[A]
) {
  def ~[C, D](that: VariableClosure[C, D]): VariableClosure[(A, C), (V, D)] =
    VariableClosure(
      Var.Impl((variables.variables, that.variables.variables).tupled),
      (query, that.query).tupled
    )
}

// Don't construct such an instance directly
final case class VariableName[A](name: String) extends AnyVal

final case class Var[V, B](
    impl: Var.Impl[V],
    variableNames: B
) {
  def ~[C, D](that: Var[C, D]): Var[(V, C), (B, D)] =
    Var(
      Var.Impl((impl.variables, that.impl.variables).tupled),
      (variableNames, that.variableNames)
    )

  def introduce[A](f: B => SelectionSet[A]): VariableClosure[A, V] =
    VariableClosure(impl, f(variableNames))

  def flatIntroduce[A, V2](f: B => VariableClosure[A, V2]): VariableClosure[A, (V, V2)] = {
    val vc = f(variableNames)
    VariableClosure(
      Var.Impl((impl.variables, vc.variables.variables).tupled),
      vc.query
    )
  }
}

object Var {
  final case class Impl[A](variables: FreeApply[Var.One, A])

  final case class One[A](
      name: VariableName[A],
      tpe: String,
      default: Option[gql.parser.QueryParser.Value],
      encoder: io.circe.Encoder[A]
  )

  type G[A] = Reader[A, (VariableName[A], io.circe.Json)]
  val variableEncoderCompiler: One ~> G = new (One ~> G) {
    override def apply[A](fa: One[A]): G[A] =
      Reader(a => fa.name -> fa.encoder(a))
  }

  final case class QueryVariable(
      name: String,
      tpe: String,
      default: Option[gql.parser.QueryParser.Value]
  )
  type C[A] = Const[NonEmptyChain[QueryVariable], A]
  val queryVariablesCompiler: One ~> C = new (One ~> C) {
    override def apply[A](fa: One[A]): C[A] =
      Const(NonEmptyChain.one(QueryVariable(fa.name.name, fa.tpe, fa.default)))
  }

  def apply[A](name: String, tpe: String, default: Option[gql.parser.QueryParser.Value] = None)(implicit
      encoder: io.circe.Encoder[A]
  ): Var[A, VariableName[A]] = {
    val vn = VariableName[A](name)
    new Var(
      Impl(FreeApply.lift(One(vn, tpe, default, encoder))),
      vn
    )
  }
}