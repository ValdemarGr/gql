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

  def arg[A](name: String, default: Option[A] = None)(implicit tpe: Input[A]): Output.Fields.Arg[A] =
    Output.Fields.Arg.initial[A](Output.Fields.ArgParam(name, tpe, default))

  def batch[F[_], I, T, K](batchName: String, key: I => F[K], resolve: Set[K] => F[Map[K, T]])(implicit
      tpe: => Output[F, T]
  ): Output.Fields.Field[F, I, T] =
    Output.Fields.SimpleField[F, I, T](
      Output.Fields.BatchedResolution(batchName, key, resolve),
      Eval.later(tpe)
    )
}
