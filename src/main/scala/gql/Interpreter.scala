package gql

import cats.data._
import PreparedQuery._
import cats.implicits._
import cats.effect._
import cats.effect.implicits._
import io.circe._
import io.circe.syntax._

object Interpreter {
  def interpretPrep[F[_]](input: Any, prep: Prepared[F, Any])(implicit F: Async[F]): F[Json] = {
    prep match {
      case Selection(fields) => interpret[F](input, fields).map(_.reduceLeft(_ deepMerge _).asJson)
      case PreparedLeaf(_, encode) =>
        encode(input) match {
          case Left(value)  => F.raiseError(new Exception(value))
          case Right(value) => F.pure(value)
        }
      case pl if pl.isInstanceOf[PreparedList[F, Any]] =>
        val pl2 = pl.asInstanceOf[PreparedList[F, Any]]
        input.asInstanceOf[Vector[Any]].traverse(i => interpretPrep[F](i, pl2.of)).map(_.reduceLeft(_ deepMerge _).asJson)
    }
  }

  def interpretField[F[_]](input: Any, pf: PreparedField[F, Any])(implicit F: Async[F]): F[JsonObject] = {
    pf match {
      case PreparedDataField(name, resolve, selection) =>
        val fa = resolve(input) match {
          case Output.Fields.PureResolution(value)  => F.pure(value)
          case Output.Fields.DeferredResolution(fa) => fa
        }

        fa
          .flatMap(i => interpretPrep[F](i, selection))
          .map(x => JsonObject(name -> x))
      case PreparedFragmentReference(reference) =>
        reference.specify(input) match {
          case None => F.pure(JsonObject.empty)
          case Some(i) =>
            interpret(i, reference.fields).map(_.reduceLeft(_ deepMerge _))
        }
      case PreparedInlineFragment(specify, selection) =>
        specify(input) match {
          case None => F.pure(JsonObject.empty)
          case Some(i) =>
            interpret(i, selection.fields).map(_.reduceLeft(_ deepMerge _))
        }
    }
  }

  def interpret[F[_]](input: Any, s: NonEmptyList[PreparedField[F, Any]])(implicit F: Async[F]) =
    s.traverse(pf => interpretField[F](input, pf))
}
