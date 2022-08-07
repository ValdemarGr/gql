package gql

import cats.data._
import cats._
import shapeless.Lazy
import scala.reflect.ClassTag

object syntax {
  def outputObject[F[_], A](
      name: String,
      hd: (String, Output.Fields.Field[F, A, _]),
      tl: (String, Output.Fields.Field[F, A, _])*
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

  def instance[F[_], A, B <: A: ClassTag](ol: ObjectLike[F, B]): Output.Unification.Instance[F, A, B] =
    Output.Unification.Instance(ol)(Output.Unification.Specify.specifyForSubtype[A, B])

  def contraInstance[F[_], A, B <: A: ClassTag, C](ol: ObjectLike[F, C], f: B => C): Output.Unification.Instance[F, A, C] =
    Output.Unification.Instance[F, A, C](ol)(Output.Unification.Specify.specifyForSubtype[A, B].map(f))

  case class PartiallyAppliedContra[B](val dummy: Boolean = false) extends AnyVal {
    def apply[F[_], A](pf: PartialFunction[A, B])(implicit ol: ObjectLike[F, B]): Output.Unification.Instance[F, A, B] =
      Output.Unification.Instance[F, A, B](ol)(Output.Unification.Specify.make(pf.lift))
  }

  def contra[B] = PartiallyAppliedContra[B]()

  def effect[F[_], I, T](resolver: I => F[T])(implicit tpe: => Output[F, T]): Output.Fields.Field[F, I, T] =
    Output.Fields.SimpleField[F, I, T](
      Output.Fields.DeferredResolution(resolver),
      Eval.later(tpe)
    )

  def effectArg[F[_], I, T, A](arg: Output.Fields.Arg[A])(resolver: (I, A) => F[T])(implicit
      tpe: => Output[F, T]
  ): Output.Fields.Field[F, I, T] =
    Output.Fields.ArgField[F, I, T, A](
      arg,
      Output.Fields.DeferredResolution { case (i, a) => resolver(i, a) },
      Eval.later(tpe)
    )

  def pure[F[_], I, T](resolver: I => T)(implicit tpe: => Output[F, T]): Output.Fields.Field[F, I, T] =
    Output.Fields.SimpleField[F, I, T](
      Output.Fields.PureResolution(resolver),
      Eval.later(tpe)
    )

  def pureArg[F[_], I, T, A](
      arg: Output.Fields.Arg[A]
  )(resolver: (I, A) => T)(implicit tpe: => Output[F, T]): Output.Fields.Field[F, I, T] =
    Output.Fields.ArgField[F, I, T, A](
      arg,
      Output.Fields.PureResolution { case (i, a) => resolver(i, a) },
      Eval.later(tpe)
    )

  def arg[A](name: String, default: Option[A] = None)(implicit tpe: Input[A]): Output.Fields.Arg[A] =
    Output.Fields.Arg.initial[A](Output.Fields.ArgParam(name, tpe, default))

  final case class BatchResolver[F[_], K, T](
      batchName: String,
      resolver: Set[K] => F[Map[K, T]]
  )

  def batchResolver[F[_], K, T](batchName: String, resolver: Set[K] => F[Map[K, T]]): BatchResolver[F, K, T] =
    BatchResolver(batchName, resolver)

  def batchPure[F[_], I, T, K](batchRes: BatchResolver[F, K, T])(key: I => K)(implicit
      tpe: => Output[F, T],
      F: Applicative[F]
  ): Output.Fields.Field[F, I, T] =
    batchEffect[F, I, T, K](batchRes)(key.andThen(F.pure))(tpe)

  def batchEffect[F[_], I, T, K](batchRes: BatchResolver[F, K, T])(key: I => F[K])(implicit
      tpe: => Output[F, T]
  ): Output.Fields.Field[F, I, T] =
    Output.Fields.SimpleField[F, I, T](
      Output.Fields.BatchedResolution(batchRes.batchName, key, batchRes.resolver),
      Eval.later(tpe)
    )
}
