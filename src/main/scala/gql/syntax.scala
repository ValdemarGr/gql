package gql

import cats.data._
import cats._

object syntax {
  def outputObject[F[_], A](
      name: String,
      hd: (String, Output.Fields.Field[F, A, _]),
      tl: (String, Output.Fields.Field[F, A, _])*
  ) = Output.Obj[F, A](name, NonEmptyList(hd, tl.toList))

  def effect[F[_], I, T](resolver: I => F[T])(implicit tpe: => Output[F, T]): Output.Fields.Field[F, I, T] =
    Output.Fields.SimpleField[F, I, T](
      resolver andThen (fa => Output.Fields.DeferredResolution(fa)),
      Eval.later(tpe)
    )

  def pure[F[_], I, T](resolver: I => T)(implicit tpe: => Output[F, T]): Output.Fields.Field[F, I, T] =
    Output.Fields.SimpleField[F, I, T](
      resolver andThen (fa => Output.Fields.PureResolution(fa)),
      Eval.later(tpe)
    )

  implicit def outputScalar[F[_], A](scalar: ScalarCodec[A]): Output.Scalar[F, A] =
    Output.Scalar(scalar)

  implicit def inputScalar[F[_], A](scalar: ScalarCodec[A]): Input.Scalar[A] =
    Input.Scalar(scalar)
}
