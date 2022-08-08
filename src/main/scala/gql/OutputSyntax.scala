package gql

import cats.implicits._
import cats.data._
import cats._
import shapeless.Lazy
import scala.reflect.ClassTag

abstract class OutputSyntax {
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

  // def instance[F[_], A, B <: A: ClassTag](ol: ObjectLike[F, B]): Output.Unification.Instance[F, A, B] =
  //   Output.Unification.Instance(ol)(Output.Unification.Specify.specifyForSubtype[A, B])

  // def contraInstance[F[_], A, B <: A: ClassTag, C](ol: ObjectLike[F, C], f: B => C): Output.Unification.Instance[F, A, C] =
  //   Output.Unification.Instance[F, A, C](ol)(Output.Unification.Specify.specifyForSubtype[A, B].map(f))

  def contra[B] = OutputSyntax.PartiallyAppliedContra[B]()

  def effect[F[_], I, T](resolver: I => F[T])(implicit tpe: => Output[F, T]): Output.Field[F, I, T, Unit] =
    Output.Field[F, I, T, Unit](
      Applicative[Arg].unit,
      Resolver.Effect { case (i, _) => resolver(i) },
      Eval.later(tpe)
    )

  def effect[F[_], I, T, A](arg: Arg[A])(resolver: (I, A) => F[T])(implicit
      tpe: => Output[F, T]
  ): Output.Field[F, I, T, A] =
    Output.Field[F, I, T, A](
      arg,
      Resolver.Effect { case (i, a) => resolver(i, a) },
      Eval.later(tpe)
    )

  def pure[F[_], I, T](resolver: I => T)(implicit tpe: => Output[F, T]): Output.Field[F, I, T, Unit] =
    Output.Field[F, I, T, Unit](
      Applicative[Arg].unit,
      Resolver.Pure { case (i, _) => resolver(i) },
      Eval.later(tpe)
    )

  def pure[F[_], I, T, A](arg: Arg[A])(resolver: (I, A) => T)(implicit tpe: => Output[F, T]): Output.Field[F, I, T, A] =
    Output.Field[F, I, T, A](
      arg,
      Resolver.Pure { case (i, a) => resolver(i, a) },
      Eval.later(tpe)
    )

  def arg[A](name: String, default: Option[A] = None)(implicit tpe: Input[A]): Arg[A] =
    Arg.initial[A](ArgParam(name, tpe, default))

  def batchResolver[F[_], K, T](batchName: String, resolver: Set[K] => F[Map[K, T]]): Resolver.Batcher[F, K, T] =
    Resolver.Batcher(batchName, resolver)

  // def batch[F[_]: Functor, I, K, A, T](batchRes: Resolver.Batcher[F, K, T])(execute: I => F[(List[K], List[(K, T)] => F[A])])(implicit
  //     tpe: => Output[F, A],
  //     F: Applicative[F]
  // ): Output.Fields.Field[F, I, A] =
  //   Output.Fields.SimpleField[F, I, A](
  //     Resolver.Batched[F, I, K, A, T](
  //       execute
  //         .andThen(_.map { case (ks, post) =>
  //           Resolver.Batch(ks, post)
  //         }),
  //       batchRes
  //     ),
  //     Eval.later(tpe)
  //   )

  def batchTraverse[F[_]: Functor, G[_]: Traverse, I, T, K](batchRes: Resolver.Batcher[F, K, T])(keys: I => F[G[K]])(implicit
      tpe: => Output[F, G[T]],
      F: Applicative[F]
  ): Output.Field[F, I, G[T], Unit] =
    Output.Field[F, I, G[T], Unit](
      Applicative[Arg].unit,
      Resolver.Batched[F, (I, Unit), K, G[T], T](
        { case (i, _) =>
          keys(i).map { gk =>
            Resolver.Batch(
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
        },
        batchRes
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
