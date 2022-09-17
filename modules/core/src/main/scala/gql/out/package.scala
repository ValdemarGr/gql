package gql

import cats.implicits._
import io.circe._
import cats._
import cats.effect._
import cats.data._
import gql.resolver._

package object out {
  trait Output[F[_], +A] {
    def mapK[G[_]: MonadCancelThrow](fk: F ~> G): Output[G, A]
  }

  trait Toplevel[F[_], +A] extends Output[F, A] {
    def name: String
  }

  trait ObjLike[F[_], A] extends Toplevel[F, A] {
    def fieldsList: List[(String, Field[F, A, _, _])]

    def fieldMap: Map[String, Field[F, A, _, _]]

    override def mapK[G[_]: MonadCancelThrow](fk: F ~> G): ObjLike[G, A]

    def contramap[B](f: B => A): Output[F, B]
  }

  final case class Obj[F[_], A](
      name: String,
      fields: NonEmptyList[(String, Field[F, A, _, _])]
  ) extends ObjLike[F, A] {
    lazy val fieldsList: List[(String, Field[F, A, _, _])] = fields.toList

    override def contramap[B](f: B => A): Obj[F, B] =
      Obj(name, fields.map { case (k, v) => k -> v.contramap(f) })

    lazy val fieldMap = fields.toNem.toSortedMap.toMap

    def mapK[G[_]: MonadCancelThrow](fk: F ~> G): Obj[G, A] =
      Obj(name, fields.map { case (k, v) => k -> v.mapK(fk) })
  }

  final case class Union[F[_], A](
      name: String,
      types: NonEmptyList[Instance[F, A, Any]]
  ) extends ObjLike[F, A] {
    override def contramap[B](f: B => A): Union[F, B] =
      Union(name, types.map(_.contramap(f)))

    lazy val instanceMap = types.map(i => i.ol.name -> i).toList.toMap

    lazy val fieldMap = Map.empty

    lazy val fieldsList: List[(String, Field[F, A, _, _])] = Nil

    def mapK[G[_]: MonadCancelThrow](fk: F ~> G): Union[G, A] =
      Union(
        name,
        types.map(_.mapK(fk))
      )
  }

  final case class Interface[F[_], A](
      name: String,
      instances: List[Instance[F, A, Any]],
      fields: NonEmptyList[(String, Field[F, A, _, _])]
  ) extends ObjLike[F, A] {
    override def mapK[G[_]: MonadCancelThrow](fk: F ~> G): Interface[G, A] =
      copy[G, A](
        instances = instances.map(_.mapK(fk)),
        fields = fields.map { case (k, v) => k -> v.mapK(fk) }
      )

    lazy val fieldsList = fields.toList

    lazy val fieldMap = fields.toNem.toSortedMap.toMap

    lazy val instanceMap = instances.map(x => x.ol.name -> x).toMap

    def contramap[B](g: B => A): Interface[F, B] =
      Interface(
        name,
        instances.map(_.contramap(g)),
        fields.map { case (k, v) => k -> v.contramap(g) }
      )
  }

  final case class Opt[F[_], A](of: Output[F, A]) extends Output[F, Option[A]] {
    def mapK[G[_]: MonadCancelThrow](fk: F ~> G): Output[G, Option[A]] = Opt(of.mapK(fk))
  }

  final case class Arr[F[_], A](of: Output[F, A]) extends Output[F, Vector[A]] {
    def mapK[G[_]: MonadCancelThrow](fk: F ~> G): Output[G, Vector[A]] = Arr(of.mapK(fk))
  }

  final case class Scalar[F[_], A](name: String, encoder: Encoder[A]) extends Toplevel[F, A] {
    override def mapK[G[_]: MonadCancelThrow](fk: F ~> G): Scalar[G, A] =
      Scalar(name, encoder)
  }

  final case class Enum[F[_], A](name: String, encoder: NonEmptyMap[A, String]) extends Toplevel[F, A] {
    override def mapK[G[_]: MonadCancelThrow](fk: F ~> G): Output[G, A] =
      Enum(name, encoder)
  }

  final case class Field[F[_], I, T, A](
      args: Arg[A],
      resolve: Resolver[F, (I, A), T],
      output: Eval[Output[F, T]]
  ) {
    def mapK[G[_]: MonadCancelThrow](fk: F ~> G): Field[G, I, T, A] =
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

  final case class Instance[F[_], A, B](
      ol: ObjLike[F, B]
  )(implicit val specify: A => Option[B]) {
    def mapK[G[_]: MonadCancelThrow](fk: F ~> G): Instance[G, A, B] =
      Instance(ol.mapK(fk))

    def contramap[C](g: C => A): Instance[F, C, B] =
      Instance[F, C, B](ol)(c => specify(g(c)))
  }
}
