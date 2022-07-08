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

  case class PartiallyAppliedContra[F[_], C](val closure: ObjectLike[F, C]) extends AnyVal {
    def apply[A](f: PartialFunction[A, C]): Output.Unification.Instance[F, A, C] =
      Output.Unification.Instance[F, A, C](closure)(Output.Unification.Specify.make(f.lift))
  }

  def contra[F[_], C](ol: ObjectLike[F, C]) = PartiallyAppliedContra(ol)

  def effect[F[_], I, T](resolver: I => F[T])(implicit tpe: => Output[F, T]): Output.Fields.Field[F, I, T] =
    Output.Fields.SimpleField[F, I, T](
      i => Output.Fields.DeferredResolution(resolver(i)),
      Eval.later(tpe)
    )

  def pure[F[_], I, T](resolver: I => T)(implicit tpe: => Output[F, T]): Output.Fields.Field[F, I, T] =
    Output.Fields.SimpleField[F, I, T](
      i => Output.Fields.PureResolution(resolver(i)),
      Eval.later(tpe)
    )
}
