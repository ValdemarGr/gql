package gql.interpreter

import io.circe.syntax._
import io.circe._
import cats.data._
import cats.implicits._

sealed trait EvalFailure {
  def paths: Chain[Cursor]

  def exception: Option[Throwable]

  def asGraphQL: Chain[JsonObject] = {
    final case class GQLError(
        message: String,
        path: Cursor
    )

    def formatEither(e: Either[Throwable, String]) =
      e.swap.as("internal error").merge

    import EvalFailure._
    val errors =
      this match {
        case EffectResolution(path, error, _) =>
          Chain(GQLError(formatEither(error), path))
        case BatchResolution(paths, _, _) =>
          paths.map(path => GQLError("internal error", path))
        case StreamHeadResolution(path, err, _) =>
          Chain(GQLError(formatEither(err), path))
        case StreamTailResolution(path, err) =>
          Chain(GQLError(formatEither(err), path))
      }

    errors.map { err =>
      JsonObject(
        "message" -> Json.fromString(err.message),
        "path" -> err.path.path.map {
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
      path: Cursor,
      error: Either[Throwable, String],
      input: Any
  ) extends EvalFailure {
    lazy val paths = Chain(path)
    lazy val exception = error.swap.toOption
  }
  final case class StreamTailResolution(
      path: Cursor,
      error: Either[Throwable, String]
  ) extends EvalFailure {
    lazy val paths = Chain(path)
    lazy val exception = error.swap.toOption
  }
  final case class BatchResolution(
      paths: Chain[Cursor],
      ex: Throwable,
      keys: Set[Any]
  ) extends EvalFailure {
    lazy val exception = Some(ex)
  }
  final case class EffectResolution(
      path: Cursor,
      error: Either[Throwable, String],
      input: Any
  ) extends EvalFailure {
    lazy val paths = Chain(path)
    lazy val exception = error.swap.toOption
  }
}
