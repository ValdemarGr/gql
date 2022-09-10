package gql.out

import cats.effect._
import cats._

trait ObjLike[F[_], A] extends Toplevel[F, A] {
  def fieldsList: List[(String, Field[F, A, _, _])]

  def fieldMap: Map[String, Field[F, A, _, _]]

  override def mapK[G[_]: MonadCancelThrow](fk: F ~> G): ObjLike[G, A]

  def contramap[B](f: B => A): Output[F, B]
}
