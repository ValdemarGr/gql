/*
 * Copyright 2023 Valdemar Grange
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package gql.natchez

import cats.effect.IO
import gql._
import gql.dsl.all._
import gql.preparation.PreparedRoot
import io.circe.{Json, JsonObject}
import munit.CatsEffectSuite
import _root_.natchez.{InMemory, Span, Trace}
import _root_.natchez.InMemory.{Lineage, NatchezCommand}
import _root_.natchez.InMemory.NatchezCommand.{CreateSpan, ReleaseSpan}
import java.util.concurrent.atomic.AtomicInteger

class NatchezTracerTest extends CatsEffectSuite {
  private val argument = arg[Int]("value")
  private lazy val schema = Schema
    .simple(
      SchemaShape.unit[IO](builder[IO, Unit](b => b.fields("echo" -> b.lift(argument)((value, _) => value))))
    )
    .unsafeRunSync()
  private val compiler = Compiler[IO]
  private val query = "query Echo($n: Int!) { echo(value: $n) }"
  private val uncached = "graphql.compilation.uncached"
  private val cacheable = "graphql.compilation.cacheable"
  private val variables = "graphql.compilation.variables"
  private val request = Lineage.Root("request")
  private val miss = request / uncached
  private val compilationSpans: List[(Lineage, NatchezCommand)] = List(
    request -> CreateSpan(uncached, None, Span.Options.Defaults),
    miss -> CreateSpan(cacheable, None, Span.Options.Defaults),
    miss -> ReleaseSpan(cacheable)
  )
  private val bindingSpans: List[(Lineage, NatchezCommand)] = List(
    miss -> CreateSpan(variables, None, Span.Options.Defaults),
    miss -> ReleaseSpan(variables),
    request -> ReleaseSpan(uncached)
  )
  private val hitSpans: List[(Lineage, NatchezCommand)] = List(
    request -> CreateSpan(variables, None, Span.Options.Defaults),
    request -> ReleaseSpan(variables)
  )

  private def traced[A](run: Trace[IO] => IO[A]): IO[(A, List[(Lineage, NatchezCommand)])] =
    for {
      entry <- InMemory.EntryPoint.create[IO]
      value <- entry.root("request").use(span => Trace.ioTrace(span).flatMap(run))
      log <- entry.ref.get
    } yield (
      value,
      log.toList.collect {
        case event @ (_, _: CreateSpan)  => event
        case event @ (_, _: ReleaseSpan) => event
      }
    )

  private def prepare(query: String, operationName: Option[String]): Either[CompilationError, CacheableQuery[IO, Unit, Unit, Unit]] =
    compiler.parsePrep(schema, QueryParameters(query, None, operationName))

  private def execute(result: Either[CompilationError, PreparedRoot[IO, Unit, Unit, Unit]]): IO[JsonObject] =
    result match {
      case Left(error) => IO(fail(error.toString))
      case Right(root) =>
        compiler.compilePrepared(schema, root) match {
          case Application.Query(run) =>
            run.map { result =>
              assertEquals(result.errors.toList, Nil)
              result.data
            }
          case _ => IO(fail("Expected query"))
        }
    }

  test("miss traces compilation then binding; hit traces binding with independent variables") {
    val preparations = new AtomicInteger(0)
    traced { implicit trace =>
      for {
        cache <- QueryCache[IO, Unit, Unit, Unit](maxEntries = 1) { (query, operationName) =>
          preparations.incrementAndGet()
          prepare(query, operationName)
        }
        first = NatchezTracer.compile(cache, QueryParameters(query, Some(Map("n" -> Json.fromInt(1))), None))
        _ <- IO(assertEquals(preparations.get(), 0))
        one <- first.flatMap(execute)
        two <- NatchezTracer.compile(cache, QueryParameters(query, Some(Map("n" -> Json.fromInt(2))), None)).flatMap(execute)
      } yield (one, two)
    }.map { case ((one, two), spans) =>
      assertEquals(one, JsonObject("echo" -> Json.fromInt(1)))
      assertEquals(two, JsonObject("echo" -> Json.fromInt(2)))
      assertEquals(preparations.get(), 1)
      assertEquals(spans, compilationSpans ++ bindingSpans ++ hitSpans)
    }
  }

  test("compilation errors close miss span without binding or persisting") {
    val preparations = new AtomicInteger(0)
    val invalid = QueryParameters("{ missing }", None, None)
    traced { implicit trace =>
      for {
        cache <- QueryCache[IO, Unit, Unit, Unit](maxEntries = 1) { (query, operationName) =>
          preparations.incrementAndGet()
          prepare(query, operationName)
        }
        first <- NatchezTracer.compile(cache, invalid)
        second <- NatchezTracer.compile(cache, invalid)
        stored <- cache.getPrep(invalid.query)
      } yield (first, second, stored)
    }.map { case ((first, second, stored), spans) =>
      assert(first.isLeft)
      assertEquals(second, first)
      assertEquals(stored, None)
      assertEquals(preparations.get(), 2)
      val failedMiss = compilationSpans :+ (request -> ReleaseSpan(uncached))
      assertEquals(spans, failedMiss ++ failedMiss)
    }
  }

  test("binding errors preserve prepared entry for subsequent traced cache hit") {
    val preparations = new AtomicInteger(0)
    traced { implicit trace =>
      for {
        cache <- QueryCache[IO, Unit, Unit, Unit](maxEntries = 1) { (query, operationName) =>
          preparations.incrementAndGet()
          prepare(query, operationName)
        }
        invalid <- NatchezTracer.compile(cache, QueryParameters(query, Some(Map("n" -> Json.fromString("invalid"))), None))
        stored <- cache.getPrep(query)
        valid <- NatchezTracer.compile(cache, QueryParameters(query, Some(Map("n" -> Json.fromInt(3))), None)).flatMap(execute)
      } yield (invalid, stored, valid)
    }.map { case ((invalid, stored, valid), spans) =>
      assert(invalid.isLeft)
      assert(stored.nonEmpty)
      assertEquals(valid, JsonObject("echo" -> Json.fromInt(3)))
      assertEquals(preparations.get(), 1)
      assertEquals(spans, compilationSpans ++ bindingSpans ++ hitSpans)
    }
  }
}
