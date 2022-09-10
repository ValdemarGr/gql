package gql.interpreter

import cats.data._

sealed trait EvalFailure { def paths: Chain[CursorGroup] }
object EvalFailure {
  final case class SignalHeadResolution(
      path: CursorGroup,
      error: Either[Throwable, String],
      input: Any
  ) extends EvalFailure { lazy val paths = Chain(path) }
  final case class SignalTailResolution(
      path: CursorGroup,
      error: Either[Throwable, String],
      input: Any
  ) extends EvalFailure { lazy val paths = Chain(path) }
  final case class BatchResolution(
      paths: Chain[CursorGroup],
      exception: Throwable,
      keys: Set[Any]
  ) extends EvalFailure
  final case class BatchPostProcessing(
      path: CursorGroup,
      error: Either[Throwable, String],
      resultMap: Chain[(BatchKey, BatchValue)]
  ) extends EvalFailure { lazy val paths = Chain(path) }
  final case class BatchMissingKey(
      path: CursorGroup,
      resultMap: Map[BatchKey, BatchValue],
      expectedKeys: List[BatchKey],
      conflictingKey: BatchKey
  ) extends EvalFailure { lazy val paths = Chain(path) }
  final case class BatchPartitioning(
      path: CursorGroup,
      error: Either[Throwable, String],
      input: Any
  ) extends EvalFailure { lazy val paths = Chain(path) }
  final case class EffectResolution(
      path: CursorGroup,
      error: Either[Throwable, String],
      input: Any
  ) extends EvalFailure { lazy val paths = Chain(path) }
}
