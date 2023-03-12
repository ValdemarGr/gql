package gql.client

import io.circe.syntax._
import cats.implicits._
import cats.data._
import io.circe._
import cats.Contravariant

final case class VariableClosure[A, V](
    variables: Var.Impl[V],
    query: SelectionSet[A]
) {
  def ~[C, D](that: VariableClosure[C, D]): VariableClosure[(A, C), (V, D)] =
    VariableClosure(Var.Impl.product(variables, that.variables), (query, that.query).tupled)
}

object VariableClosure {
  implicit def contravariantForVaiableClosure[A]: Contravariant[VariableClosure[A, *]] = {
    type G[B] = VariableClosure[A, B]
    new Contravariant[G] {
      override def contramap[B, C](fa: G[B])(f: C => B): G[C] = 
        VariableClosure(fa.variables.map(_.contramapObject(f)), fa.query)
    }
  }
}

// Don't construct such an instance directly
final case class VariableName[A](name: String) extends AnyVal {
  def asValue: gql.parser.QueryParser.Value =
    gql.parser.QueryParser.Value.VariableValue(name)
}

final case class Var[V, B](
    impl: Var.Impl[V],
    variableNames: B
) {
  def ~[C, D](that: Var[C, D]): Var[(V, C), (B, D)] =
    Var(Var.Impl.product(impl, that.impl), (variableNames, that.variableNames))

  def introduce[A](f: B => SelectionSet[A]): VariableClosure[A, V] =
    VariableClosure(impl, f(variableNames))

  def flatIntroduce[A, V2](f: B => VariableClosure[A, V2]): VariableClosure[A, (V, V2)] = {
    val vc = f(variableNames)
    VariableClosure(Var.Impl.product(impl, vc.variables), vc.query)
  }
}

object Var {
  type Impl[A] = Writer[NonEmptyChain[One[?]], Encoder.AsObject[A]]
  object Impl {
    def product[A, B](fa: Impl[A], fb: Impl[B]): Impl[(A, B)] =
      fa.flatMap(aenc =>
        fb.map(benc =>
          Encoder.AsObject.instance[(A, B)] { case (a, b) =>
            JsonObject.fromMap(aenc.encodeObject(a).toMap ++ benc.encodeObject(b).toMap)
          }
        )
      )
  }

  implicit def contravariantForVar[B]: Contravariant[Var[*, B]] = {
    type G[A] = Var[A, B]
    new Contravariant[G] {
      override def contramap[A, B](fa: G[A])(f: B => A): G[B] = 
        Var(fa.impl.map(_.contramapObject(f)), fa.variableNames)
    }
  }

  final case class One[A](
      name: VariableName[A],
      tpe: String,
      default: Option[gql.parser.QueryParser.Value]
  )

  def apply[A](name: String, tpe: String, default: Option[gql.parser.QueryParser.Value] = None)(implicit
      encoder: io.circe.Encoder[A]
  ): Var[A, VariableName[A]] = {
    val vn = VariableName[A](name)
    val enc = Encoder.AsObject.instance[A](a => JsonObject(name -> a.asJson))
    new Var(Writer(NonEmptyChain.one(One(vn, tpe, default)), enc), vn)
  }
}
