package gql.interpreter

import io.circe.syntax._
import io.circe._
import cats.data._
import cats.implicits._

sealed trait EvalFailure {
  def paths: Chain[CursorGroup]

  def asGraphQL: Chain[JsonObject] = {
    final case class GQLError(
        message: String,
        path: CursorGroup
    )

    def formatEither(e: Either[Throwable, String]) =
      e.swap.as("internal error").merge

    import EvalFailure._
    val errors =
      this match {
        case EffectResolution(path, error, _) =>
          Chain(GQLError(formatEither(error), path))
        case SignalHeadResolution(path, error, _) =>
          Chain(GQLError(formatEither(error), path))
        case BatchMissingKey(path, _, _, _) =>
          Chain(GQLError("internal error", path))
        case SignalTailResolution(path, error, input) =>
          Chain(GQLError(formatEither(error), path))
        case BatchPartitioning(path, error, input) =>
          Chain(GQLError(formatEither(error), path))
        case BatchPostProcessing(path, error, resultMap) =>
          Chain(GQLError(formatEither(error), path))
        case BatchResolution(paths, exception, keys) =>
          paths.map(path => GQLError("internal error", path))
        case StreamHeadResolution(path, err, _) =>
          Chain(GQLError(formatEither(err), path))
        case StreamTailResolution(path, err) =>
          Chain(GQLError(formatEither(err), path))
      }

    errors.map { err =>
      JsonObject(
        "message" -> Json.fromString(err.message),
        "path" -> err.path.absolutePath.path.map {
          case GraphArc.Field(_, name)    => Json.fromString(name)
          case GraphArc.Index(idx)        => Json.fromInt(idx)
          case GraphArc.Fragment(_, name) => Json.fromString(s"fragment:$name")
        }.asJson
      )
    }
  }
}
object EvalFailure {
  final case class StreamHeadResolution(
      path: CursorGroup,
      error: Either[Throwable, String],
      input: Any
  ) extends EvalFailure { lazy val paths = Chain(path) }
  final case class StreamTailResolution(
      path: CursorGroup,
      error: Either[Throwable, String]
  ) extends EvalFailure { lazy val paths = Chain(path) }
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
      expectedKeys: Set[BatchKey],
      conflictingKeys: Set[BatchKey]
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
