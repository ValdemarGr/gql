package gql.preparation

import gql.GraphArc
import cats.mtl.Local
import gql.Cursor

trait PathAlg[F[_]] {
    def ambientEdge[A](edge: GraphArc)(fa: F[A]): F[A]

    def ambientField[A](name: String)(fa: F[A]): F[A] =
        ambientEdge(GraphArc.Field(name))(fa)

    def ambientIndex[A](i: Int)(fa: F[A]): F[A] =
        ambientEdge(GraphArc.Index(i))(fa)
}

object PathAlg {
    def apply[F[_]](implicit ev: PathAlg[F]): PathAlg[F] = ev

    implicit def pathAlgForLocal[F[_]](implicit L: Local[F, Cursor]): PathAlg[F] = new PathAlg[F] {
      override def ambientEdge[A](edge: GraphArc)(fa: F[A]): F[A] = L.local(fa)(_.add(edge))
    }
}