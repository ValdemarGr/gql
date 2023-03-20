package gql.preparation

import cats._
import cats.implicits._
import cats.mtl._
import gql.Cursor

trait ErrorAlg[F[_], C] {
  def raise[A](message: String, caret: Option[C]): F[A]

  def raiseEither[A](e: Either[String, A], caret: Option[C]): F[A]

  def raiseOpt[A](oa: Option[A], message: String, caret: Option[C]): F[A] =
    raiseEither(oa.toRight(message), caret)

  def modifyError[A](f: PositionalError[C] => PositionalError[C])(fa: F[A]): F[A]

  def appendMessage[A](message: String)(fa: F[A]): F[A] =
    modifyError[A](d => d.copy(message = d.message + "\n" + message))(fa)
}

object ErrorAlg {
  def apply[F[_], C](implicit ev: ErrorAlg[F, C]): ErrorAlg[F, C] = ev

  implicit def errorAlgForHandle[F[_]: Monad, G[_]: Applicative, C](implicit
      H: Handle[F, G[PositionalError[C]]],
      L: Local[F, Cursor]
  ): ErrorAlg[F, C] = new ErrorAlg[F, C] {
    override def raise[A](message: String, caret: Option[C]): F[A] =
      L.ask.flatMap(c => H.raise(Applicative[G].pure(PositionalError(c, caret.toList, message))))

    override def raiseEither[A](e: Either[String, A], caret: Option[C]): F[A] =
      e match {
        case Left(value)  => raise(value, caret)
        case Right(value) => Monad[F].pure(value)
      }

    override def modifyError[A](f: PositionalError[C] => PositionalError[C])(fa: F[A]): F[A] =
      H.handleWith(fa)(xs => H.raise(xs.map(f)))
  }
}
