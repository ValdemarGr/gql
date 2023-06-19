package gql.dsl

import gql.ast._
import cats.data._
import cats._
import scala.reflect.ClassTag

trait UnionDsl[F[_]] {
  def union[A](name: String): UnionDsl.PartiallyAppliedUnion0[F, A] =
    UnionDsl.union[F, A](name)

  implicit def unionDslUnionOps[F[_], A](u: Union[F, A]): UnionDsl.UnionOps[F, A] =
    UnionDsl.unionDslFullUnionOps[F, A](u)
}

trait UnionDslFull {
  def union[F[_], A](name: String): UnionDsl.PartiallyAppliedUnion0[F, A] =
    new UnionDsl.PartiallyAppliedUnion0[F, A](name)

  implicit def unionDslFullUnionOps[F[_], A](u: Union[F, A]): UnionDsl.UnionOps[F, A] =
    new UnionDsl.UnionOps[F, A](u)
}

object UnionDsl extends UnionDslFull {
  final class PartiallyAppliedUnion0[F[_], A](private val name: String) extends AnyVal {
    def variant[B](pf: PartialFunction[A, B])(implicit innerTpe: => Type[F, B]): PartiallyAppliedUnion1[F, A] =
      new PartiallyAppliedUnion1[F, A](name, Variant[F, A, B](Eval.later(innerTpe))(pf.lift))

    def subtype[B: ClassTag](implicit ev: B <:< A, innerTpe: => Type[F, B]): PartiallyAppliedUnion1[F, A] =
      variant[B] { case a: B => a }(innerTpe)
  }

  final class PartiallyAppliedUnion1[F[_], A](private val name: String, private val hd: Variant[F, A, ?]) {
    def variant[B](pf: PartialFunction[A, B])(implicit innerTpe: => Type[F, B]): Union[F, A] =
      Union[F, A](name, NonEmptyList.of(hd, Variant[F, A, B](Eval.later(innerTpe))(pf.lift)), None)

    def subtype[B: ClassTag](implicit ev: B <:< A, innerTpe: => Type[F, B]): Union[F, A] =
      variant[B] { case a: B => a }(innerTpe)
  }

  final case class UnionOps[F[_], A](private val u: Union[F, A]) extends AnyVal {
    def variant[B](pf: PartialFunction[A, B])(implicit innerTpe: => Type[F, B]): Union[F, A] =
      u.copy(types = Variant[F, A, B](Eval.later(innerTpe))(pf.lift) :: u.types)

    def subtype[B: ClassTag](implicit ev: B <:< A, innerTpe: => Type[F, B]): Union[F, A] =
      variant[B] { case a: B => a }(innerTpe)
  }
}
