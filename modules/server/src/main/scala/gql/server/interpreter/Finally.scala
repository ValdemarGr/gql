package gql.server.interpreter

import cats.implicits._
import cats.effect._

trait Finally[F[_]] {
  def rememberResource[A](fa: Resource[F, A]): F[Unit]

  def remember[A](fa: F[A]): F[Unit] =
    rememberResource(Resource.eval(fa))
}

object Finally {
  def make[F[_]](implicit F: Concurrent[F]): Resource[F, Finally[F]] =
    Resource.make(F.ref(List.empty[F[Unit]]))(_.get.flatMap(_.sequence_)).map { state =>
      new Finally[F] {
        def rememberResource[A](fa: Resource[F, A]): F[Unit] =
          F.uncancelable { poll =>
            poll(fa.allocated).flatMap { case (_, release) => state.update(release :: _).void }
          }
      }
    }
}
