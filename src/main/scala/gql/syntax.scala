package gql

import cats.data._
import cats._

object syntax {
  def outputObject[F[_], A](
      name: String,
      hd: (String, Types.Output.Fields.Field[F, A, _]),
      tl: (String, Types.Output.Fields.Field[F, A, _])*
  ) = Types.Output.Object[F, A](name, NonEmptyList(hd, tl.toList))

  def effect[F[_], I, T](resolver: I => F[T])(implicit tpe: => Types.Output[F, T]): Types.Output.Fields.Field[F, I, T] =
    Types.Output.Fields.SimpleField[F, I, T](
      resolver andThen (fa => Types.Output.Fields.DeferredResolution(fa)),
      Eval.later(tpe)
    )

  def pure[F[_], I, T](resolver: I => T)(implicit tpe: => Types.Output[F, T]): Types.Output.Fields.Field[F, I, T] =
    Types.Output.Fields.SimpleField[F, I, T](
      resolver andThen (fa => Types.Output.Fields.PureResolution(fa)),
      Eval.later(tpe)
    )

  implicit def outputScalar[F[_], A](scalar: Types.ScalarCodec[A]): Types.Output.Scalar[F, A] =
    Types.Output.Scalar(scalar)

  implicit def inputScalar[F[_], A](scalar: Types.ScalarCodec[A]): Types.Input.Scalar[A] =
    Types.Input.Scalar(scalar)
}
