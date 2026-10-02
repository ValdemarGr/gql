package gql.preparation

import cats._
import cats.data._
import cats.implicits._
import gql.Arg
import gql.std.LazyT
import org.typelevel.scalaccompat.annotation._

object SubstitueVariables {
  def substSel[F[_], C, A](
      fa: Selection[F, A, Stage.Compilation[C]]
  ): Alg[C, Selection[F, A, Stage.Execution]] = new Binder[F, C].selection(fa)

  private class Binder[F[_], C] {
    type G[A] = Alg[C, A]
    type Comp = Stage.Compilation[C]
    type Exec = Stage.Execution
    type Write[A] = WriterT[G, Chain[(Arg[?], Any)], A]
    type Analyze[A] = LazyT[Write, PreparedDataField[F, ?, ?, Exec], A]
    val G = Alg.Ops[C]
    implicit val L: Applicative[Analyze] = LazyT.applicativeForParallelLazyT

    def lift[A](fa: G[A]): Analyze[A] = LazyT.liftF(WriterT.liftF(fa))

    @nowarn3("msg=.*cannot be checked at runtime because its type arguments can't be determined.*")
    @nowarn2("msg=.*type parameters eliminated by erasure.*")
    def step[I, O](fa: PreparedStep[F, I, O, Comp]): Analyze[PreparedStep[F, I, O, Exec]] = {
      import PreparedStep._
      fa match {
        case node: Lift[F, I, O]        => lift(G.pure(node))
        case node: EmbedEffect[F, O]    => lift(G.pure(node))
        case node: EmbedStream[F, O]    => lift(G.pure(node))
        case node: EmbedError[F, O]     => lift(G.pure(node))
        case node: Batch[F, k, v]       => lift(G.pure(node))
        case node: InlineBatch[F, k, v] => lift(G.pure(node))
        case node: EvalMeta[F, I]       => lift(G.pure(node))
        case node: Compose[F, I, a, O, Comp] =>
          (step(node.left), step(node.right)).mapN(Compose(node.nodeId, _, _))
        case node: First[F, i, o, c, Comp] => step(node.step).map(First[F, i, o, c, Exec](node.nodeId, _))
        case node: Choose[F, a, b, c, d, Comp] =>
          (step(node.fac), step(node.fbd)).mapN(Choose(node.nodeId, _, _))
        case node: SubstVars[F, I, O, C] =>
          LazyT.liftF(WriterT(node.sub.map { value =>
            (Chain.one[(Arg[?], Any)](node.arg -> value), Lift[F, I, O](node.nodeId, _ => value))
          }))
        case node: PrecompileMeta[F, I, C] =>
          (lift(G.defer(node.meta.value)), LazyT.id[Write, PreparedDataField[F, ?, ?, Exec]])
            .mapN { (meta, owner) => EvalMeta[F, I](node.nodeId, owner.map(pdf => PreparedMeta(meta.variables, meta.args, pdf))) }
      }
    }

    def cont[I, A](fa: PreparedCont[F, I, A, Comp]): Analyze[PreparedCont[F, I, A, Exec]] =
      (step(fa.edges), prepared(fa.cont)).mapN(PreparedCont(_, _))

    def prepared[I](fa: Prepared[F, I, Comp]): Analyze[Prepared[F, I, Exec]] = fa match {
      case node: Selection[F, I, Comp] => lift(selection(node))
      case node: PreparedLeaf[F, I, Comp] =>
        lift(G.pure(PreparedLeaf(node.nodeId, node.name, node.encode)))
      case node: PreparedList[F, a, I, b, Comp] =>
        cont(node.of).map(PreparedList(node.id, _, node.toSeq))
      case node: PreparedOption[F, i, o, Comp] => cont(node.of).map(PreparedOption(node.id, _))
    }

    def dataField[I, A](fa: PreparedDataField[F, I, A, Comp]): G[PreparedDataField[F, I, A, Exec]] = {
      val analyzed: LazyT[G, PreparedDataField[F, ?, ?, Exec], PreparedDataField[F, I, A, Exec]] =
        cont(fa.cont).mapF(_.run.map { case (args, f) =>
          f.andThen { bound =>
            PreparedDataField(fa.nodeId, fa.name, fa.alias, bound, fa.source, ParsedArgs.Execution(args.toList.toMap))
          }
        })
      analyzed.runWithValue(identity)
    }

    def specialization[I, A](fa: Specialization[F, I, A, Comp]): Specialization[F, I, A, Exec] = fa match {
      case node: Specialization.Type[F, I, Comp]         => Specialization.Type(node.source)
      case node: Specialization.Union[F, I, A, Comp]     => Specialization.Union(node.source, node.variant)
      case node: Specialization.Interface[F, I, A, Comp] => Specialization.Interface(node.target, node.impl)
    }

    def field[I](fa: PreparedField[F, I, Comp]): G[PreparedField[F, I, Exec]] = fa match {
      case node: PreparedDataField[F, I, a, Comp] => dataField(node)
      case node: PreparedSpecification[F, I, a, Comp] =>
        node.selection
          .parTraverse { case pdf: PreparedDataField[F, a, b, Comp] => dataField(pdf) }
          .map(PreparedSpecification(node.nodeId, specialization(node.specialization), _))
    }

    def selection[I](fa: Selection[F, I, Comp]): G[Selection[F, I, Exec]] =
      G.defer(fa.fields.parTraverse(field(_)).map(Selection(fa.nodeId, _, fa.source)))
  }
}
