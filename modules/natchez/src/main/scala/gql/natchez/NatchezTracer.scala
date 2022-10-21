package gql.natchez

import gql._
import _root_.natchez._
import cats._
import cats.implicits._

object NatchezTracer {
  def compiler[F[_]: Trace](compiler: Compiler[F])(implicit F: Monad[F]): Compiler[F] = { cp =>
    Trace[F].span("graphql") {
      val operationName: List[(String, TraceValue)] =
        cp.operationName.map("graphql.operation_name" -> TraceValue.stringToTraceValue(_)).toList

      Trace[F].put(operationName: _*) >>
        Trace[F]
          .span("graphql.preparation") {
            compiler.compile(cp)
          }
          .flatMap {
            case e @ Left(CompilationError.Parse(pe)) =>
              Trace[F].span("graphql.parse.error") {
                Trace[F].put("graphql.parse.error.message" -> TraceValue.stringToTraceValue(pe.prettyError.value)) as e
              }
            case e @ Left(CompilationError.Preparation(pe)) =>
              Trace[F].span("graphql.preparation.error") {
                Trace[F].put(
                  "graphql.preparation.error.message" -> pe.message,
                  "graphql.preparation.error.path" -> pe.position.position.map(_.name).mkString_(".")
                ) as e
              }
            case Right(Application.Query(fa)) =>
              F.pure(Right(Application.Query(Trace[F].span("graphql.query")(fa))))
            case Right(Application.Mutation(fa)) =>
              F.pure(Right(Application.Mutation(Trace[F].span("graphql.mutation")(fa))))
            case Right(Application.Subscription(stream)) =>
              // Cannot trace a stream with just trace :(
              // TODO introduce a Resource[F, F ~> F] tracer
              F.pure {
                Right(Application.Subscription(stream.evalMap(a => Trace[F].span("graphql.subscription")(F.pure(a)))))
              }
          }
    }
  }
}
