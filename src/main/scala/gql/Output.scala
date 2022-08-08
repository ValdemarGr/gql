package gql

import cats.implicits._
import cats.data._
import io.circe._
import Value._
import cats._
import scala.reflect.ClassTag
import gql.Output.Interface
import gql.Output.Obj

sealed trait Output[F[_], +A] {
  def mapK[G[_]: Functor](fk: F ~> G): Output[G, A]
}

sealed trait ToplevelOutput[F[_], +A] extends Output[F, A] {
  def name: String
}

sealed trait ObjectLike[F[_], A] extends ToplevelOutput[F, A] {
  def fieldsList: List[(String, Output.Field[F, A, _, _])]

  def fieldMap: Map[String, Output.Field[F, A, _, _]]

  override def mapK[G[_]: Functor](fk: F ~> G): ObjectLike[G, A]

  def contramap[B](f: B => A): Output[F, B]
}

final case class Schema[F[_], Q](query: Output.Obj[F, Q], types: Map[String, ToplevelOutput[F, _]])

object Output {
  final case class Arr[F[_], A](of: Output[F, A]) extends Output[F, Vector[A]] {
    def mapK[G[_]: Functor](fk: F ~> G): Output[G, Vector[A]] = Arr(of.mapK(fk))
  }

  final case class Opt[F[_], A](of: Output[F, A]) extends Output[F, Option[A]] {
    def mapK[G[_]: Functor](fk: F ~> G): Output[G, Option[A]] = Opt(of.mapK(fk))
  }

  final case class Interface[F[_], A](
      name: String,
      instances: Map[String, Unification.Instance[F, A, Any]],
      fields: NonEmptyList[(String, Field[F, A, _, _])]
  ) extends Output[F, A]
      with ToplevelOutput[F, A]
      with ObjectLike[F, A] {

    override def mapK[G[_]: Functor](fk: F ~> G): Interface[G, A] =
      copy[G, A](
        instances = instances.map { case (k, v) => k -> v.mapK(fk) },
        fields = fields.map { case (k, v) => k -> v.mapK(fk) }
      )

    lazy val fieldsList = fields.toList

    lazy val fieldMap = fields.toNem.toSortedMap.toMap

    def contramap[B](g: B => A): Interface[F, B] =
      Interface(
        name,
        instances.map { case (k, v) => k -> v.contramap(g) },
        fields.map { case (k, v) => k -> v.contramap(g) }
      )
  }
  object Unification {
    sealed trait Specify[A, B] extends (A => Option[B]) { self =>
      def contramap[C](g: C => A): Specify[C, B] =
        new Specify[C, B] {
          def apply(c: C): Option[B] = self(g(c))
        }

      def map[C](f: B => C): Specify[A, C] =
        new Specify[A, C] {
          def apply(a: A): Option[C] = self(a).map(f)
        }
    }
    object Specify {
      def make[A, B](f: A => Option[B]): Specify[A, B] = new Specify[A, B] {
        def apply(a: A): Option[B] = f(a)
      }

      def specifyForSubtype[A, B <: A: ClassTag]: Specify[A, B] =
        new Specify[A, B] {
          def apply(a: A): Option[B] = Some(a).collect { case b: B => b }
        }
    }

    final case class Instance[F[_], A, B](
        ol: ObjectLike[F, B]
    )(implicit val specify: Specify[A, B]) {
      def mapK[G[_]: Functor](fk: F ~> G): Instance[G, A, B] =
        Instance(ol.mapK(fk))

      def contramap[C](g: C => A): Instance[F, C, B] =
        Instance[F, C, B](ol)(specify.contramap[C](g))
    }
  }

  final case class Obj[F[_], A](
      name: String,
      fields: NonEmptyList[(String, Field[F, A, _, _])]
  ) extends Output[F, A]
      with ToplevelOutput[F, A]
      with ObjectLike[F, A] {
    lazy val fieldsList: List[(String, Field[F, A, _, _])] = fields.toList

    override def contramap[B](f: B => A): Obj[F, B] =
      Obj(name, fields.map { case (k, v) => k -> v.contramap(f) })

    lazy val fieldMap = fields.toNem.toSortedMap.toMap

    def mapK[G[_]: Functor](fk: F ~> G): Obj[G, A] =
      Obj(name, fields.map { case (k, v) => k -> v.mapK(fk) })
  }

  final case class Field[F[_], I, T, A](
      args: Arg[A],
      resolve: Resolver[F, (I, A), T],
      output: Eval[Output[F, T]]
  ) {
    def mapK[G[_]: Functor](fk: F ~> G): Field[G, I, T, A] =
      Field[G, I, T, A](
        args,
        resolve.mapK(fk),
        output.map(_.mapK(fk))
      )

    def contramap[B](g: B => I): Field[F, B, T, A] =
      Field(
        args,
        resolve.contramap[(B, A)] { case (b, a) => (g(b), a) },
        output
      )
  }

  final case class Union[F[_], A](
      name: String,
      types: NonEmptyMap[String, Unification.Instance[F, A, Any]]
  ) extends Output[F, A]
      with ObjectLike[F, A]
      with ToplevelOutput[F, A] {

    override def contramap[B](f: B => A): Union[F, B] =
      Union(name, types.map(_.contramap(f)))

    lazy val instances = types.toSortedMap.toMap

    lazy val fieldMap = Map.empty

    lazy val fieldsList: List[(String, Field[F, A, _, _])] = Nil

    def mapK[G[_]: Functor](fk: F ~> G): Union[G, A] =
      Union(
        name,
        types.map(_.mapK(fk))
      )
  }

  final case class Scalar[F[_], A](name: String, encoder: Encoder[A]) extends Output[F, A] with ToplevelOutput[F, A] {
    override def mapK[G[_]: Functor](fk: F ~> G): Scalar[G, A] =
      Scalar(name, encoder)
  }

  final case class Enum[F[_], A](name: String, encoder: NonEmptyMap[A, String]) extends Output[F, A] with ToplevelOutput[F, A] {
    override def mapK[G[_]: Functor](fk: F ~> G): Output[G, A] =
      Enum(name, encoder)
  }
}
