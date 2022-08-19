package gql.interpreter

import cats.effect.implicits._
import gql.resolver._
import cats.data._
import gql._
import cats.effect._
import cats.implicits._
import cats.effect.std._
import fs2.Chunk

trait SubscriptionSupervisor[F[_]] {
  def subscribe(ref: StreamReference[Any, Any], key: Any): F[BigInt]

  def remove(id: BigInt): F[Unit]

  def changeLog: fs2.Stream[F, NonEmptyList[(BigInt, Any)]]
}

object SubscriptionSupervisor {
  def apply[F[_]](ss: SchemaState[F])(implicit F: Concurrent[F]): fs2.Stream[F, SubscriptionSupervisor[F]] = {
    type ResourceGroupId = Int
    type ResourceKey = Any
    type Cleanup = F[Unit]
    type Listeners = Int

    final case class SubscriptionState(
        nextId: BigInt,
        subscriptions: Map[BigInt, (ResourceGroupId, ResourceKey)],
        openResources: Map[(ResourceGroupId, ResourceKey), (Listeners, Cleanup)]
    )

    val stateS = fs2.Stream
      .bracket(F.ref(SubscriptionState(BigInt(1), Map.empty, Map.empty)))(
        _.get.flatMap(_.openResources.toList.parTraverse { case (_, (_, fa)) => fa }.void)
      )

    val killableStream: fs2.Stream[F, Throwable => F[Unit]] = fs2.Stream
      .eval(F.deferred[Throwable])
      .flatMap { d =>
        fs2
          .Stream(d)
          .interruptWhen(d.get.map(_.asLeft[Unit]))
          .map(d => (t: Throwable) => d.complete(t).void)
      }

    val qS = fs2.Stream.eval(Queue.bounded[F, Chunk[NonEmptyList[(BigInt, Any)]]](1024))

    killableStream.flatMap { complete =>
      qS.flatMap { q =>
        stateS.map { state =>
          def addStreamData(
              rgi: ResourceGroupId,
              k: ResourceKey,
              stream: fs2.Stream[F, Any]
          ): fs2.Stream[F, NonEmptyList[(BigInt, Any)]] =
            stream
              .evalMap { x =>
                state.get
                  .map(_.subscriptions.toList.collect {
                    case (bd, (rgi2, k2)) if rgi2 == rgi && k2 == k => bd
                  }.toNel)
                  .map(_.map((x, _)))
              }
              .unNone
              .map { case (x, nel) => nel.map((_, x)) }

          def openStream(rgi: ResourceGroupId, k: ResourceKey): F[F[Unit]] = {
            val res = ss.streams(rgi)(k)

            fs2.Stream
              .resource(res)
              .flatMap(addStreamData(rgi, k, _).enqueueUnterminatedChunks(q))
              .handleErrorWith(e => fs2.Stream.eval(complete(e)))
              .compile
              .drain
              .start
              .map(_.cancel)
          }

          // Maybe there is already a resource open for this type of stream?
          // Also, this is pretty volatile and unsafe, so no cancellation here
          def reserveResourceF(rgi: ResourceGroupId, k: ResourceKey): F[BigInt] = F.uncancelable { _ =>
            for {
              d <- F.deferred[Cleanup]
              tryAlloc <- state.modify { s =>
                val compositeKey = (rgi, k)
                val (newMap, openF) =
                  s.openResources.get(compositeKey) match {
                    case Some((listeners, fa)) =>
                      val newEntry = (listeners + 1, fa)
                      (s.openResources + (compositeKey -> newEntry), F.unit)
                    case None =>
                      val open: F[Unit] = openStream(rgi, k).flatMap(d.complete).void

                      val newEntry = (1, d.get.flatten)

                      (s.openResources + (compositeKey -> newEntry), open)
                  }

                val nextId = s.nextId

                val sub = (nextId -> compositeKey)

                (
                  s.copy(
                    openResources = newMap,
                    nextId = nextId + 1,
                    subscriptions = s.subscriptions + sub
                  ),
                  openF.as(nextId)
                )
              }
              o <- tryAlloc
            } yield o
          }

          def releaseSubscriptionF(id: BigInt): F[Unit] =
            state.modify { s =>
              s.subscriptions.get(id) match {
                case None => (s, F.unit)
                case Some(ck) =>
                  s.openResources.get(ck) match {
                    case None => (s, F.unit)
                    case Some((listeners, result)) =>
                      if (listeners > 1) {
                        val newEntry = (listeners - 1, result)
                        (
                          s.copy(
                            openResources = s.openResources + (ck -> newEntry),
                            subscriptions = s.subscriptions - id
                          ),
                          F.unit
                        )
                      } else {
                        (
                          s.copy(
                            openResources = s.openResources - ck,
                            subscriptions = s.subscriptions - id
                          ),
                          result
                        )
                      }
                  }
              }
            }.flatten

          new SubscriptionSupervisor[F] {
            override def subscribe(ref: StreamReference[Any, Any], key: Any): F[BigInt] =
              reserveResourceF(ref.id, key)

            override def remove(id: BigInt): F[Unit] =
              releaseSubscriptionF(id)

            override def changeLog: fs2.Stream[F, NonEmptyList[(BigInt, Any)]] =
              fs2.Stream.fromQueueUnterminatedChunk(q)
          }
        }
      }
    }
  }
}
