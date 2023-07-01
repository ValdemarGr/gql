package gql.util

import cats._
import cats.mtl._
import cats.data._
import cats.implicits._
import gql.ast._
import gql.SchemaShape

trait SchemaFold[F[_], G[_]] {
  def in[A]: PartialFunction[In[A], G[Unit]] = PartialFunction.empty

  def out[A]: PartialFunction[Out[F, A], G[Unit]] = PartialFunction.empty
}

object SchemaFold {
  def foldWith[F[_], G[_]: Monad](root: SchemaShape[F, ?, ?, ?])(
      inPf: PartialFunction[In[?], G[Unit]],
      outPf: PartialFunction[Out[F, ?], G[Unit]]
  ): G[Unit] = {
    type Alg[F[_]] = Stateful[F, Set[String]]
    type Fk[H[_]] = MonadPartialOrder[G, H]

    def nextIfNotSeen[H[_]](tl: Toplevel[F, ?])(ha: => H[Unit])(implicit H: Monad[H], A: Alg[H]): H[Unit] =
      A.get.flatMap { seen =>
        if (seen.contains(tl.name)) H.unit
        else A.modify(_ + tl.name) >> ha
      }

    def goOutput[H[_]](out: Out[F, ?])(implicit H: Monad[H], A: Alg[H], fk: Fk[H]): H[Unit] = {
      lazy val lifted = fk(outPf.lift(out).sequence_)
      out match {
        case o: OutArr[?, ?, ?, ?] => lifted >> goOutput[H](o.of)
        case o: OutOpt[?, ?, ?]    => lifted >> goOutput[H](o.of)
        case t: OutToplevel[F, ?] =>
          nextIfNotSeen[H](t) {
            lazy val nextF = t match {
              case ol: ObjectLike[F, ?] =>
                ol.abstractFields.traverse_ { case (_, af) => goOutput[H](af.output.value) } >>
                  ol.implementsMap.values.toList.map(_.value).traverse_(goOutput[H])
              case Union(_, instances, _) =>
                instances.traverse_(inst => goOutput[H](inst.tpe.value))
              case _ => H.unit
            }

            lifted >> nextF
          }
      }
    }

    def goInput[H[_]](in: In[?])(implicit H: Monad[H], A: Alg[H], fk: Fk[H]): H[Unit] = {
      lazy val lifted = fk(inPf.lift(in).sequence_)
      in match {
        case InArr(of, _) => lifted >> goInput[H](of)
        case InOpt(of)    => lifted >> goInput[H](of)
        case t: InToplevel[?] =>
          nextIfNotSeen[H](t) {
            t match {
              case Input(_, fields, _) =>
                fields.entries.traverse_(x => goInput[H](x.input.value))
              case _ => H.unit
            }
          }
      }
    }

    val outs = root.query :: root.mutation.toList ++ root.subscription.toList ++ root.outputTypes

    type Effect[A] = StateT[G, Set[String], A]
    val outsF = outs.traverse_(goOutput[Effect])

    val insF = root.inputTypes.traverse_(goInput[Effect])

    (outsF >> insF).runA(Set.empty)
  }
}
