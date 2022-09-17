package gql.resolver

import cats._
import cats.data._
import gql._
import cats.effect._

trait StreamRef[F[_], I, O] { self =>
  type M

  type K

  def ref: I => Int

  def filterMapping: (I, M) => OptionT[ResultF[F, *], O]

  def getKey: I => ResultF[F, K]

  def contramap[I2](f: I2 => I): StreamRef[F, I2, O] =
    new StreamRef[F, I2, O] {
      type M = self.M
      type K = self.K

      def ref = i => self.ref(f(i))
      def filterMapping = (i, m) => self.filterMapping(f(i), m)
      def getKey = f andThen self.getKey
    }

  def filterMapF[O2](f: (I, O) => OptionT[ResultF[F, *], O2])(implicit F: Monad[F]): StreamRef[F, I, O2] =
    new StreamRef[F, I, O2] {
      type M = self.M
      type K = self.K

      def ref = self.ref
      def filterMapping = (i, m) => self.filterMapping(i, m).flatMap(f(i, _))
      def getKey = self.getKey
    }

  def filterMapR[O2](f: (I, O) => Result[Option[O2]])(implicit F: Monad[F]): StreamRef[F, I, O2] =
    filterMapF[O2] { case (i, o) => OptionT(IorT.apply(F.pure(f(i, o)))) }

  def filterMap[O2](f: (I, O) => Option[O2])(implicit F: Monad[F]): StreamRef[F, I, O2] =
    filterMapR[O2] { case (i, o) => Ior.right(f(i, o)) }

  def mapK[G[_]](fk: F ~> G): StreamRef[G, I, O] =
    new StreamRef[G, I, O] {
      type M = self.M
      type K = self.K

      def ref = self.ref
      def filterMapping = (i, m) => OptionT(self.filterMapping(i, m).value.mapK(fk))
      def getKey = i => self.getKey(i).mapK(fk)
    }
}

object StreamRef {
  def apply[F[_]: Applicative, I, O](pick: I => Resource[F, fs2.Stream[F, O]]): State[SchemaState[F], StreamRef[F, I, O]] =
    State { s =>
      val id = s.nextId
      val entry = pick.asInstanceOf[Any => Resource[F, fs2.Stream[F, Any]]]
      val sr = new StreamRef[F, I, O] {
        type M = O
        type K = I

        def ref = _ => id

        def getKey = IorT.pure[F, String](_)

        def filterMapping = (_, m) => OptionT(IorT.pure[F, String](Some(m)))
      }
      (s.copy(streams = s.streams + (id -> entry)), sr)
    }
}
