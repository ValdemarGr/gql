package gql

import cats.implicits._
import cats.data._
import cats._
import shapeless.Lazy
import scala.reflect.ClassTag
import gql.resolver._

abstract class OutputSyntax {
  def field[F[_], I, A](r: Resolver[F, I, A])(implicit tpe: => Output[F, A]) =
    Output.Field(
      Applicative[Arg].unit,
      r.contramap[(I, Unit)] { case (i, _) => i },
      Eval.later(tpe)
    )

  // def augment[F[_], I, A, Ag](arg: Arg[Ag])(f: Output.Field[F, (I, Ag), A, Unit]) =
  //   Output.Field(
  //     f.args *> arg,
  //     f.resolve.contramap[(I, Ag)] { case (i, ag) => ((i, ag), ()) },
  //     f.output
  //   )

  def argumented[F[_], I, A, Ag](arg: Arg[Ag])(r: Resolver[F, (I, Ag), A])(implicit tpe: => Output[F, A]) =
    Output.Field(
      arg,
      r,
      Eval.later(tpe)
    )

  def obj[F[_], A](
      name: String,
      hd: (String, Output.Field[F, A, _, _]),
      tl: (String, Output.Field[F, A, _, _])*
  ) = Output.Obj[F, A](name, NonEmptyList(hd, tl.toList))

  def union[F[_], A](
      name: String,
      hd: Output.Unification.Instance[F, A, _],
      tl: Output.Unification.Instance[F, A, _]*
  ) = Output.Union[F, A](
    name,
    NonEmptyList(hd, tl.toList).map(x => x.ol.name -> x.asInstanceOf[Output.Unification.Instance[F, A, Any]]).toNem
  )

  def interface[F[_], A](
      o: Output.Obj[F, A],
      hd: Output.Unification.Instance[F, A, _],
      tl: Output.Unification.Instance[F, A, _]*
  ) = Output.Interface[F, A](
    o.name,
    NonEmptyList(hd, tl.toList).map(x => x.ol.name -> x.asInstanceOf[Output.Unification.Instance[F, A, Any]]).toList.toMap,
    o.fields
  )

  def contra[B] = OutputSyntax.PartiallyAppliedContra[B]()

  def effect[F[_], I, T](resolver: I => F[T])(implicit tpe: => Output[F, T]): Output.Field[F, I, T, Unit] =
    effect[F, I, T, Unit](Applicative[Arg].unit) { case (i, _) => resolver(i) }(tpe)

  def effect[F[_], I, T, A](arg: Arg[A])(resolver: (I, A) => F[T])(implicit
      tpe: => Output[F, T]
  ): Output.Field[F, I, T, A] =
    Output.Field[F, I, T, A](
      arg,
      EffectResolver { case (i, a) => resolver(i, a) },
      Eval.later(tpe)
    )

  def eff[F[_], I, T](resolver: I => F[T]) =
    EffectResolver[F, I, T](resolver)

  def pur[F[_], I, T](resolver: I => T) =
    PureResolver[F, I, T](resolver)

  def pure[F[_], I, T](resolver: I => T)(implicit tpe: => Output[F, T]): Output.Field[F, I, T, Unit] =
    pure[F, I, T, Unit](Applicative[Arg].unit) { case (i, _) => resolver(i) }(tpe)

  def pure[F[_], I, T, A](arg: Arg[A])(resolver: (I, A) => T)(implicit tpe: => Output[F, T]): Output.Field[F, I, T, A] =
    Output.Field[F, I, T, A](
      arg,
      PureResolver { case (i, a) => resolver(i, a) },
      Eval.later(tpe)
    )

  def arg[A](name: String, default: Option[A] = None)(implicit tpe: Input[A]): Arg[A] =
    Arg.initial[A](ArgParam(name, tpe, default))

  // def batchResolver[K, T](batchName: String, resolver: Set[K] => F[Map[K, T]]): BatcherReference[K, T] =
  //   Resolver.Batcher(batchName, resolver)

  def batchTraverse[F[_]: Functor, G[_]: Traverse, I, T, K](batchRef: BatcherReference[K, T])(keys: I => F[G[K]])(implicit
      tpe: => Output[F, G[T]],
      F: Applicative[F]
  ): Output.Field[F, I, G[T], Unit] = {
    implicit lazy val tpe2 = tpe
    batchTraverse[F, G, I, T, K, Unit](Applicative[Arg].unit)(batchRef)(keys)
  }

  def batchTraverse[F[_]: Functor, G[_]: Traverse, I, T, K, A](arg: Arg[A])(batchRef: BatcherReference[K, T])(keys: I => F[G[K]])(implicit
      tpe: => Output[F, G[T]],
      F: Applicative[F]
  ): Output.Field[F, I, G[T], A] =
    Output.Field[F, I, G[T], A](
      arg,
      BatchResolver[F, (I, A), K, G[T], T](
        batchRef,
        { case (i, _) =>
          keys(i).map { gk =>
            Batch(
              gk.toList,
              { xs =>
                val arr = xs.toVector
                F.pure {
                  gk.mapWithIndex { case (_, i) =>
                    val (_, v) = xs(i)
                    v
                  }
                }
              }
            )
          }
        }
      ),
      Eval.later(tpe)
    )

//   def batchPure[F[_], I, T, K](batchRes: Resolver.Batcher[F, K, T])(key: I => K)(implicit
//       tpe: => Output[F, T],
//       F: Applicative[F]
//   ): Output.Fields.Field[F, I, T] = {
//     implicit lazy val tpe2 = tpe
//     batchEffect[F, I, T, K](batchRes)(key.andThen(F.pure))
//   }

//   def batchEffect[F[_], I, T, K](batchRes: Resolver.Batcher[F, K, T])(key: I => F[K])(implicit
//       tpe: => Output[F, T],
//       F: Applicative[F]
//   ): Output.Fields.Field[F, I, T] =
//     Output.Fields.SimpleField[F, I, T](
//       Resolver.Batched[F, I, K, T, T](
//         key.andThen(_.map(k => Resolver.Batch(List(k), xs => F.pure(xs.map { case (_, v) => v }.head)))),
//         batchRes
//       ),
//       Eval.later(tpe)
//     )
}

object OutputSyntax {
  case class PartiallyAppliedContra[B](val dummy: Boolean = false) extends AnyVal {
    def apply[F[_], A](pf: PartialFunction[A, B])(implicit ol: ObjectLike[F, B]): Output.Unification.Instance[F, A, B] =
      Output.Unification.Instance[F, A, B](ol)(Output.Unification.Specify.make(pf.lift))
  }
}
