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
  def mapK[G[_]](fk: F ~> G): Output[G, A]

  def name: String
}

sealed trait ToplevelOutput[F[_], +A] extends Output[F, A]

sealed trait Unifyable[F[_], A] extends Output[F, A] {
  def instances: Map[String, Output.Unification.Instance[F, A, _]]
}

sealed trait Selectable[F[_], A] extends Output[F, A] {
  def fieldMap: Map[String, Output.Fields.Field[F, A, _]]
}

sealed trait ObjectLike[F[_], A] extends Output[F, A] {
  def name: String

  def fields: NonEmptyList[(String, Output.Fields.Field[F, A, _])]

  override def mapK[G[_]](fk: F ~> G): ObjectLike[G, A]

  def contramap[B](f: B => A): Output[F, B]
}

final case class Schema[F[_], Q](query: Output.Obj[F, Q], types: Map[String, ToplevelOutput[F, _]])

object Output {
  final case class Arr[F[_], A](of: Output[F, A]) extends Output[F, Vector[A]] {
    def mapK[G[_]](fk: F ~> G): Output[G, Vector[A]] = Arr(of.mapK(fk))

    def name: String = s"[${of.name}]"
  }

  final case class Opt[F[_], A](of: Output[F, A]) extends Output[F, Option[A]] {
    def mapK[G[_]](fk: F ~> G): Output[G, Option[A]] = Opt(of.mapK(fk))

    def name: String = s"(${of.name} | null)"
  }

  final case class Interface[F[_], A](
      name: String,
      instances: Map[String, Unification.Instance[F, A, _]],
      fields: NonEmptyList[(String, Fields.Field[F, A, _])]
  ) extends Output[F, A]
      with ToplevelOutput[F, A]
      with Selectable[F, A]
      with Unifyable[F, A]
      with ObjectLike[F, A] {
    override def mapK[G[_]](fk: F ~> G): Interface[G, A] =
      copy[G, A](
        instances = instances.map { case (k, v) => k -> v.mapK(fk) },
        fields = fields.map { case (k, v) => k -> v.mapK(fk) }
      )

    lazy val fieldMap = fields.toNem.toSortedMap.toMap

    def contramap[B](g: B => A): Interface[F, B] =
      Interface(name, instances.map { case (k, v) => k -> v.contramap(g) }, fields.map { case (k, v) => k -> v.contramap(g) })
  }
  object Unification {
    sealed trait Specify[A, B] extends (A => Option[B]) { self =>
      def contramap[C](g: C => A): Specify[C, B] =
        new Specify[C, B] {
          def apply(c: C): Option[B] = self(g(c))
        }
    }
    object Specify {
      implicit def specifyForSubtype[A, B <: A: ClassTag]: Specify[A, B] =
        new Specify[A, B] {
          def apply(a: A): Option[B] = Some(a).collect { case b: B => b }
        }
    }

    final case class Instance[F[_], A, B](
        ol: ObjectLike[F, B]
    )(implicit val specify: Specify[A, B]) {
      def mapK[G[_]](fk: F ~> G): Instance[G, A, B] =
        Instance(ol.mapK(fk))

      def contramap[C](g: C => A): Instance[F, C, B] =
        Instance[F, C, B](ol)(specify.contramap(g))
    }
  }

  final case class Obj[F[_], A](
      name: String,
      fields: NonEmptyList[(String, Fields.Field[F, A, _])]
  ) extends Output[F, A]
      with ToplevelOutput[F, A]
      with Selectable[F, A]
      with ObjectLike[F, A] {

    override def contramap[B](f: B => A): Obj[F, B] =
      Obj(name, fields.map { case (k, v) => k -> v.contramap(f) })

    lazy val fieldMap = fields.toNem.toSortedMap.toMap

    def mapK[G[_]](fk: F ~> G): Obj[G, A] =
      Obj(name, fields.map { case (k, v) => k -> v.mapK(fk) })
  }

  object Fields {
    sealed trait Resolution[F[_], A] {
      def mapK[G[_]](fk: F ~> G): Resolution[G, A]
    }
    final case class PureResolution[F[_], A](value: A) extends Resolution[F, A] {
      override def mapK[G[_]](fk: F ~> G): Resolution[G, A] =
        PureResolution(value)
    }
    final case class DeferredResolution[F[_], A](fa: F[A]) extends Resolution[F, A] {
      override def mapK[G[_]](fk: F ~> G): Resolution[G, A] =
        DeferredResolution(fk(fa))
    }

    sealed trait Field[F[_], I, T] {
      def output: Eval[Output[F, T]]

      def mapK[G[_]](fk: F ~> G): Field[G, I, T]

      def contramap[B](g: B => I): Field[F, B, T]
    }

    final case class SimpleField[F[_], I, T](
        resolve: I => Resolution[F, T],
        output: Eval[Output[F, T]]
    ) extends Field[F, I, T] {
      def mapK[G[_]](fk: F ~> G): Field[G, I, T] =
        SimpleField(resolve andThen (_.mapK(fk)), output.map(_.mapK(fk)))

      def contramap[B](g: B => I): SimpleField[F, B, T] = {
        SimpleField(
          i => resolve(g(i)),
          output
        )
      }
    }

    final case class Arg[A](
        name: String,
        input: Input[A],
        default: Option[A] = None
    )

    final case class Args[A](
        entries: NonEmptyVector[Arg[_]],
        decode: List[_] => (List[_], A)
    )
    object Args {
      def apply[A](entry: Arg[A]): Args[A] =
        Args(NonEmptyVector.one(entry), { s => (s.tail, s.head.asInstanceOf[A]) })
    }

    implicit lazy val applyForArgs = new Apply[Args] {
      override def map[A, B](fa: Args[A])(f: A => B): Args[B] =
        fa.copy(decode = fa.decode andThen { case (s, a) => (s, f(a)) })

      override def ap[A, B](ff: Args[A => B])(fa: Args[A]): Args[B] =
        Args(
          ff.entries ++: fa.entries,
          { s1 =>
            val (s2, f) = ff.decode(s1)
            val (s3, a) = fa.decode(s2)
            (s3, f(a))
          }
        )
    }

    final case class ArgField[F[_], I, T, A](
        args: Args[A],
        resolve: (I, A) => Resolution[F, T],
        output: Eval[Output[F, T]]
    ) extends Field[F, I, T] {
      def mapK[G[_]](fk: F ~> G): Field[G, I, T] =
        ArgField[G, I, T, A](
          args,
          (i, a) => (resolve(i, a).mapK(fk)),
          output.map(_.mapK(fk))
        )

      def contramap[B](g: B => I): ArgField[F, B, T, A] =
        ArgField(
          args,
          (b, a) => (resolve(g(b), a)),
          output
        )
    }
  }

  final case class Union[F[_], A](
      name: String,
      types: NonEmptyMap[String, Unification.Instance[F, A, _]]
  ) extends Output[F, A]
      with Unifyable[F, A]
      with Selectable[F, A]
      with ToplevelOutput[F, A] {
    lazy val instances = types.toSortedMap.toMap

    lazy val fieldMap = Map.empty

    def mapK[G[_]](fk: F ~> G): Union[G, A] =
      Union(
        name,
        types.map(_.mapK(fk))
      )
  }

  final case class Scalar[F[_], A](name: String, encoder: Encoder[A]) extends Output[F, A] with ToplevelOutput[F, A] {
    override def mapK[G[_]](fk: F ~> G): Scalar[G, A] =
      Scalar(name, encoder)
  }

  final case class Enum[F[_], A](name: String, encoder: A => String) extends Output[F, A] with ToplevelOutput[F, A] {
    override def mapK[G[_]](fk: F ~> G): Output[G, A] =
      Enum(name, encoder)
  }
}
