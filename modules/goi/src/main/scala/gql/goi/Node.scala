package gql.goi

// import cats.implicits._
import java.util.UUID

trait Node {
  def id: String
}

trait GetNode[F[_], A] {
  def getNode(id: String): Option[F[Option[A]]]
}

object Test {
  final case class Contract(
      contractId: UUID,
      contractName: String
  ) extends Node {
    lazy val id = s"Contract:$contractId"
  }

  import gql.dsl._
  import gql.ast._
  implicit def node[F[_], N <: Node](implicit get: GetNode[F, N]) = interface[F, N](
    "Node",
    "id" -> pure(_.id),
    "delete_me" -> pure[F, N](_ => get)(Scalar[F, GetNode[F, N]]("delete_me", _ => gql.Value.StringValue(""), _ => Left("")))
  )

  implicit def contract[F[_]] = tpe[F, Contract](
    "Contract",
    "contractId" -> pure(_.contractId),
    "contractName" -> pure(_.contractName)
  )
  // .implements { case c: Contract => c }(node[F, Contract])
}
