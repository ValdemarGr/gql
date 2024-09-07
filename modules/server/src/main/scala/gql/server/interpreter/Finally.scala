package gql.server.interpreter

import cats.implicits._
import cats.effect._

trait Finally[F[_]] {
  def rememberResource[A](fa: Resource[F, A]): F[Unique.Token]

  def remember[A](fa: F[A]): F[Unique.Token] =
    rememberResource(Resource.eval(fa))

  def cancel(token: Unique.Token): F[Unit]

//   def leased(fa: Resource[F, Unit]): F[SharedResource[F]]
}

object Finally {
  def make[F[_]](implicit F: Concurrent[F]): Resource[F, Finally[F]] =
    Resource.make(F.ref(Map.empty[Unique.Token, F[Unit]]))(_.get.flatMap(_.values.toList.sequence_)).map { state =>
      new Finally[F] {
        def rememberResource[A](fa: Resource[F, A]): F[Unique.Token] =
          F.unique.flatTap { tok =>
            F.uncancelable { poll =>
              poll(fa.allocated).flatMap { case (_, release) => state.update(_ + ((tok, release))) }
            }
          }

        def cancel(token: Unique.Token): F[Unit] =
          F.uncancelable { _ =>
            state.modify(m => (m - token, m.get(token))).flatMap(_.sequence_)
          }
      }
    }
}
