package gql.preparation

import cats.implicits._
import gql.parser.{QueryAst => QA}
import cats.data._

object QueryValidation {
  def validateQuery[C](
      execs: NonEmptyList[QA.ExecutableDefinition[C]],
      operationName: String
  ): EitherNec[PositionalError[C], QA.ExecutableDefinition[C]] = {
    val (ops, frags) = execs.toList.partitionEither {
      case QA.ExecutableDefinition.Operation(op, c)  => Left((op, c))
      case QA.ExecutableDefinition.Fragment(frag, _) => Right(frag)
    }

    lazy val applied = ops.map { case (x, _) => x }

    lazy val positions = ops.map { case (_, x) => x }

    lazy val possible = applied
      .collect { case d: QA.OperationDefinition.Detailed[C] => d.name }
      .collect { case Some(x) => s"'$x'" }
      .mkString(", ")

    // def raise(msg: String) =
    //   Left(NonEmptyChain.fromSeq(positions.flatMap(_.toList).map(p => PositionalError(msg, p))).get)

    // (applied, operationName) match {
    //   case (Nil, _)      => G.raise(s"No operations provided.", Nil)
    //   case (x :: Nil, _) => G.pure(x)
    //   case (_, _) if applied.exists {
    //         case _: QA.OperationDefinition.Simple[C]                     => true
    //         case x: QA.OperationDefinition.Detailed[C] if x.name.isEmpty => true
    //         case _                                                       => false
    //       } =>
    //     G.raise(s"Exactly one operation must be suplied if the operations include at least one unnamed operation.", positions)
    //   case (_, None) =>
    //     G.raise(s"Operation name must be supplied when supplying multiple operations, provided operations are $possible.", positions)
    //   case (_, Some(name)) =>
    //     val o = applied.collectFirst { case d: QA.OperationDefinition.Detailed[C] if d.name.contains(name) => d }
    //     G.raiseOpt(o, s"Unable to find operation '$name', provided possible operations are $possible.", positions)
    // }
    ???
  }
}
