package fs2

import fs2.internal.Scope
import cats.effect.Resource
import cats._
import cats.implicits._

object UnsafeFs2Access {
  def getScope[F[_]]: Pull[F, Nothing, Scope[F]] = Pull.getScope[F]

  def leaseScope[F[_]: MonadThrow]: Pull[F, Nothing, Resource[F, Unit]] =
    getScope[F].map(scope => Resource.make(scope.lease)(x => x.cancel.rethrow).void)

  def interruptWhen[F[_]: MonadThrow, O](fa: F[Unit])(p: Pull[F, O, Unit]): Pull[F, O, Unit] =
    Pull.interruptScope(Pull.scope(Pull.interruptWhen(fa.attempt) >> p))
}
