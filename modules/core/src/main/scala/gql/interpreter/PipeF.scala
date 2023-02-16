package gql.interpreter

import fs2._

trait PipeF[F[_]] {
  def apply[A]: fs2.Pipe[F, Chunk[A], Chunk[A]]
}

object PipeF {
  type τ[F[_]]

  def apply[F[_]](f: fs2.Pipe[F, Chunk[τ[F]], Chunk[τ[F]]]) = new PipeF[F] {
    def apply[A] = f.asInstanceOf[fs2.Pipe[F, Chunk[A], Chunk[A]]]
  }

  def identity[F[_]] = apply[F](x => x)
}