package gql

import cats.data._
import cats._
import shapeless.Lazy

object syntax {
  def outputObject[F[_], A](
      name: String,
      hd: (String, Output.Fields.Field[F, A, _]),
      tl: (String, Output.Fields.Field[F, A, _])*
  ) = Output.Obj[F, A](name, NonEmptyList(hd, tl.toList))

  def effect[F[_], I, T](resolver: I => F[T])(implicit tpe: Lazy[Output[F, T]]): Output.Fields.Field[F, I, T] =
    Output.Fields.SimpleField[F, I, T](
      i => Output.Fields.DeferredResolution(resolver(i)),
      Eval.later(tpe.value)
    )

  def pure[F[_], I, T](resolver: I => T)(implicit tpe: Output[F, T]): Output.Fields.Field[F, I, T] =
    Output.Fields.SimpleField[F, I, T](
      resolver andThen (fa => Output.Fields.PureResolution(fa)),
      Eval.later(tpe)
    )
}
