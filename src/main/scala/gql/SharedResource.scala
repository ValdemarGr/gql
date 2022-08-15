package gql

import cats.effect.implicits._
import cats.implicits._
import cats.effect._

trait SharedResource[F[_], K, V] {
  def allocate(key: K): F[(BigInt, V)]

  def remove(leaseId: BigInt): F[V]
}

// object SharedResource {
//   sealed trait AllocationOutcome
//   object AllocationOutcome {
//     case object Allocated extends AllocationOutcome
//     case object ReUsed extends AllocationOutcome
//   }

//   def apply[F[_], K, V](implicit F: Concurrent[F]) = {
//     type Listeners = Long
//     type Cleanup = F[Unit]

//     final case class Allocation(
//         value: V,
//         cleanup: Cleanup
//     )

//     final case class InternalState(
//         nextId: BigInt,
//         allocations: Map[BigInt, K],
//         openResources: Map[K, (Listeners, F[Either[Throwable, Allocation]])]
//     )

//     val refR = Resource.make(F.ref(InternalState(BigInt(1), Map.empty, Map.empty)))(
//       _.get.flatMap(_.openResources.toList.parTraverse { case (_, (_, fa)) => fa.flatMap(_.traverse(_.cleanup)) }.void)
//     )

//     refR.map { state =>
//       def reserve(k: K): F[]
//     }
//   }
// }
