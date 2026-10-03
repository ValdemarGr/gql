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
package gql

import cats.Eval
import cats.effect.IO
import cats.implicits._
import gql.ast._
import gql.dsl.all._
import gql.preparation.{MergedFieldInfo, PreparedDataField, PreparedRoot, PreparedSpecification, Selection}
import gql.parser.{QueryAst => QA}
import gql.resolver.Resolver
import io.circe.{Json, JsonObject}
import munit.CatsEffectSuite
import java.util.concurrent.atomic.AtomicInteger

class QueryCacheTest extends CatsEffectSuite {
  private case class Owner(summary: String)
  private trait AbstractEcho
  private case object EchoImplementation extends AbstractEcho
  private val argument = arg[Int]("value")
  private val positiveArgument = argument.emap(value => Either.cond(value > 0, value, "Must be positive"))
  private implicit lazy val ownerType: Type[IO, Owner] = tpe[IO, Owner](
    "CacheOwner",
    "summary" -> lift(_.summary)
  )
  private implicit lazy val abstractEcho: Interface[IO, AbstractEcho] = interface[IO, AbstractEcho](
    "CachedInterface",
    "echo" -> abstWith[IO, Int, Int](argument),
    "positive" -> abstWith[IO, Int, Int](positiveArgument)
  )
  private lazy val echoImplementation: Type[IO, EchoImplementation.type] = tpe[IO, EchoImplementation.type](
    "CachedImplementation",
    "echo" -> lift(argument)((value, _) => value),
    "positive" -> lift(argument)((value, _) => value)
  ).subtypeOf[AbstractEcho]

  private lazy val schema = Schema
    .simple(
      SchemaShape.unit[IO](
        builder[IO, Unit] { b =>
          b.fields(
            "echo" -> b.lift(argument)((value, _) => value),
            "positive" -> b.lift(positiveArgument)((value, _) => value),
            "flag" -> b.lift(_ => true),
            "abstract" -> b.lift(_ => EchoImplementation: AbstractEcho),
            "meta" -> b.from(Resolver.meta[IO, Unit].map { meta =>
              meta.queryMeta.variables.get("n").map(_.value.fold(_.noSpaces, _.toString)).getOrElse("absent")
            }),
            "owner" -> b.from(Resolver.meta[IO, Unit].arg(argument).map { case (value, meta) =>
              assertEquals(meta.astNode.arg(argument), Some(value))
              assertEquals(meta.astNode.name, "owner")
              val selection = meta.astNode.cont.cont match {
                case fields: Selection[IO, ?] =>
                  fields.fields.flatMap {
                    case field: PreparedDataField[IO, ?, ?]    => List(field.outputName)
                    case spec: PreparedSpecification[IO, ?, ?] => spec.selection.map(_.outputName)
                  }
                case _ => fail("Expected owner selection")
              }
              Owner(s"${meta.alias.getOrElse(meta.astNode.name)}:$value:${selection.mkString(",")}")
            })
          )
        },
        mutation = Some(builder[IO, Unit](b => b.fields("echo" -> b.lift(argument)((value, _) => value)))),
        subscription = Some(builder[IO, Unit] { b =>
          b.fields("echo" -> b(_.arg(argument).streamMap { case (value, _) => fs2.Stream.emit(value).covary[IO] }))
        }),
        outputTypes = List(echoImplementation)
      )
    )
    .unsafeRunSync()

  private val compiler = Compiler[IO]
  private val echo = "query Echo($n: Int!) { echo(value: $n) }"
  private val firstVars = Map("n" -> Json.fromInt(1))
  private val secondVars = Map("n" -> Json.fromInt(2))

  private def prepare(query: String, operationName: Option[String]): Either[CompilationError, CacheableQuery[IO, Unit, Unit, Unit]] =
    compiler.parsePrep(schema, QueryParameters(query, None, operationName))

  private def execute(prepared: Either[CompilationError, PreparedRoot[IO, Unit, Unit, Unit]]): IO[JsonObject] =
    prepared match {
      case Left(error) => IO(fail(error.toString))
      case Right(root) =>
        val run = compiler.compilePrepared(schema, root) match {
          case Application.Query(run)           => run
          case Application.Mutation(run)        => run
          case Application.Subscription(stream) => stream.take(1).compile.lastOrError
        }
        run.map { result =>
          assertEquals(result.errors.toList, Nil)
          result.data
        }
    }

  test("cache compiles once and binds each request independently") {
    val preparations = new AtomicInteger(0)
    for {
      cache <- QueryCache[IO, Unit, Unit, Unit](maxEntries = 2) { (query, operationName) =>
        preparations.incrementAndGet()
        prepare(query, operationName)
      }
      first <- cache.compile(QueryParameters(echo, Some(firstVars), None)).flatMap(execute)
      second <- cache.compile(QueryParameters(echo, Some(secondVars), None)).flatMap(execute)
      _ <- IO {
        assertEquals(first, JsonObject("echo" -> Json.fromInt(1)))
        assertEquals(second, JsonObject("echo" -> Json.fromInt(2)))
        assertEquals(preparations.get(), 1)
      }
    } yield ()
  }

  test("cache reuses queries with no variables") {
    val query = "{ echo(value: 7) }"
    val preparations = new AtomicInteger(0)
    for {
      cache <- QueryCache[IO, Unit, Unit, Unit](maxEntries = 1) { (text, operationName) =>
        preparations.incrementAndGet()
        prepare(text, operationName)
      }
      first <- cache.compile(QueryParameters(query, None, None)).flatMap(execute)
      second <- cache.compile(QueryParameters(query, Some(Map.empty), None)).flatMap(execute)
      _ <- IO {
        assertEquals(first, JsonObject("echo" -> Json.fromInt(7)))
        assertEquals(second, first)
        assertEquals(preparations.get(), 1)
      }
    } yield ()
  }

  test("literal fields and metadata resolver chains reuse completed preparation") {
    val cached = prepare("{ echo(value: 7) flag meta }", None).fold(error => fail(error.toString), identity)
    val first = cached.run(Map.empty).fold(error => fail(error.toString), identity)
    val second = cached.run(Map.empty).fold(error => fail(error.toString), identity)
    assert(first.operation eq second.operation)

    (execute(Right(first)), execute(Right(second))).parTupled.map { case (a, b) =>
      assertEquals(a, JsonObject("echo" -> Json.fromInt(7), "flag" -> Json.True, "meta" -> Json.fromString("absent")))
      assertEquals(b, a)
    }
  }

  test("binding does not repeat static output preparation") {
    val outputReads = new AtomicInteger(0)
    val field = Field[IO, Unit, Int](
      Resolver.argument[IO, Unit, Int](argument),
      Eval.always {
        outputReads.incrementAndGet()
        intScalar
      }
    )
    val countedSchema = Schema.simple(SchemaShape.unit[IO](fields("echo" -> field))).unsafeRunSync()
    val cached = compiler
      .parsePrep(countedSchema, QueryParameters(echo, None, None))
      .fold(error => fail(error.toString), identity)
    val preparedReads = outputReads.get()
    assert(preparedReads > 0)

    assert(cached.run(firstVars).isRight)
    assertEquals(outputReads.get(), preparedReads)
    assert(cached.run(secondVars).isRight)
    assertEquals(outputReads.get(), preparedReads)
  }

  test("parent variable validation does not repeat static child preparation") {
    val outputReads = new AtomicInteger(0)
    val child = Field[IO, Owner, String](
      Resolver.lift[IO, Owner](_.summary),
      Eval.always {
        outputReads.incrementAndGet()
        stringScalar
      }
    )
    val nestedType = tpe[IO, Owner]("CountedOwner", "summary" -> child)
    val parent = Field[IO, Unit, Owner](
      Resolver.argument[IO, Unit, Int](positiveArgument).map(value => Owner(value.toString)),
      Eval.now(nestedType)
    )
    val countedSchema = Schema.simple(SchemaShape.unit[IO](fields("owner" -> parent))).unsafeRunSync()
    val query = "query Owner($n: Int!) { owner(value: $n) { summary } }"
    val cached = compiler
      .parsePrep(countedSchema, QueryParameters(query, None, None))
      .fold(error => fail(error.toString), identity)
    val preparedReads = outputReads.get()
    assert(preparedReads > 0)

    cached.run(Map("n" -> Json.fromInt(-1))) match {
      case Left(CompilationError.Preparation(errors)) =>
        assertEquals(errors.toChain.toList.map(_.message), List("Must be positive"))
      case _ => fail("Expected parent argument validation error")
    }
    assertEquals(outputReads.get(), preparedReads)

    List(firstVars, secondVars)
      .traverse { variables =>
        val root = cached.run(variables).fold(error => fail(error.toString), identity)
        assertEquals(outputReads.get(), preparedReads)
        compiler.compilePrepared(countedSchema, root) match {
          case Application.Query(run) =>
            run.map { result =>
              assertEquals(result.errors.toList, Nil)
              result.data
            }
          case _ => fail("Expected query")
        }
      }
      .map { results =>
        assertEquals(
          results,
          List(
            JsonObject("owner" -> Json.obj("summary" -> Json.fromString("1"))),
            JsonObject("owner" -> Json.obj("summary" -> Json.fromString("2")))
          )
        )
        assertEquals(outputReads.get(), preparedReads)
      }
  }

  test("parent errors skip child validation without delaying or repeating static child preparation") {
    val outputReads = new AtomicInteger(0)
    val argumentReads = new AtomicInteger(0)
    val childArgument = argument.emap { value =>
      argumentReads.incrementAndGet()
      Either.cond(value > 0, value, "Child must be positive")
    }
    val child = Field[IO, Owner, String](
      Resolver.argument[IO, Owner, Int](childArgument).map(_.toString),
      Eval.always {
        outputReads.incrementAndGet()
        stringScalar
      }
    )
    val nestedType = tpe[IO, Owner]("CountedOwner", "summary" -> child)
    val parent = Field[IO, Unit, Owner](
      Resolver.argument[IO, Unit, Int](positiveArgument).map(value => Owner(value.toString)),
      Eval.now(nestedType)
    )
    val countedSchema = Schema.simple(SchemaShape.unit[IO](fields("owner" -> parent))).unsafeRunSync()
    outputReads.set(0)
    argumentReads.set(0)
    val query = "query Owner($n: Int!) { owner(value: $n) { summary(value: -1) } }"
    val cached = compiler
      .parsePrep(countedSchema, QueryParameters(query, None, None))
      .fold(error => fail(error.toString), identity)
    val preparedReads = (outputReads.get(), argumentReads.get())
    assert(preparedReads._1 > 0)
    assert(preparedReads._2 > 0)

    List(-1 -> "Must be positive", 1 -> "Child must be positive", -2 -> "Must be positive").foreach { case (value, message) =>
      val previousArgumentReads = argumentReads.get()
      cached.run(Map("n" -> Json.fromInt(value))) match {
        case Left(CompilationError.Preparation(errors)) =>
          assertEquals(errors.toChain.toList.map(_.message), List(message))
          val cursor = if (value < 0) Cursor.empty.field("owner") else Cursor.empty.field("owner").field("summary")
          assertEquals(errors.toChain.toList.map(_.position), List(cursor))
        case _ => fail("Expected argument validation error")
      }
      assertEquals(outputReads.get(), preparedReads._1)
      assertEquals(argumentReads.get(), previousArgumentReads + (if (value > 0) 1 else 0))
    }
  }

  test("preparation ignores supplied variables until run") {
    val cached = compiler
      .parsePrep(schema, QueryParameters(echo, Some(Map("n" -> Json.fromString("invalid"))), None))
      .fold(error => fail(error.toString), identity)

    execute(cached.run(secondVars)).map(result => assertEquals(result, JsonObject("echo" -> Json.fromInt(2))))
  }

  test("cached bindings use defaults without retaining earlier supplied values") {
    val query = "query Echo($n: Int! = 7) { echo(value: $n) }"
    for {
      cache <- QueryCache[IO, Unit, Unit, Unit](maxEntries = 1)(prepare)
      supplied <- cache.compile(QueryParameters(query, Some(secondVars), None)).flatMap(execute)
      defaulted <- cache.compile(QueryParameters(query, None, None)).flatMap(execute)
      suppliedAgain <- cache.compile(QueryParameters(query, Some(firstVars), None)).flatMap(execute)
      _ <- IO {
        assertEquals(supplied, JsonObject("echo" -> Json.fromInt(2)))
        assertEquals(defaulted, JsonObject("echo" -> Json.fromInt(7)))
        assertEquals(suppliedAgain, JsonObject("echo" -> Json.fromInt(1)))
      }
    } yield ()
  }

  test("binding errors retain prepared cache entry and do not affect later bindings") {
    val preparations = new AtomicInteger(0)
    for {
      cache <- QueryCache[IO, Unit, Unit, Unit](maxEntries = 1) { (query, operationName) =>
        preparations.incrementAndGet()
        prepare(query, operationName)
      }
      missing <- cache.compile(QueryParameters(echo, None, None))
      entry <- cache.getPrep(echo)
      invalid <- cache.compile(QueryParameters(echo, Some(Map("n" -> Json.fromString("invalid"))), None))
      valid <- cache.compile(QueryParameters(echo, Some(firstVars), None)).flatMap(execute)
      _ <- IO {
        assert(missing.isLeft)
        assert(entry.isDefined)
        assert(invalid.isLeft)
        assertEquals(valid, JsonObject("echo" -> Json.fromInt(1)))
        assertEquals(preparations.get(), 1)
      }
    } yield ()
  }

  test("binding accumulates independent variable errors") {
    val query = "query Echo($x: Int!, $y: Int!) { first: positive(value: $x) second: positive(value: $y) }"
    val cached = prepare(query, None).fold(error => fail(error.toString), identity)
    List(Json.fromString("invalid"), Json.fromInt(-1)).foreach { value =>
      cached.run(Map("x" -> value, "y" -> value)) match {
        case Left(CompilationError.Preparation(errors)) => assertEquals(errors.toChain.toList.size, 2)
        case _                                          => fail("Expected accumulated variable errors")
      }
    }
  }

  test("variable validation errors do not block independent body binding") {
    val argumentReads = new AtomicInteger(0)
    val outputReads = new AtomicInteger(0)
    val countedArgument = argument.emap { value =>
      argumentReads.incrementAndGet()
      Right(value)
    }
    val counted = Field[IO, Unit, Int](
      Resolver.argument[IO, Unit, Int](countedArgument),
      Eval.always {
        outputReads.incrementAndGet()
        intScalar
      }
    )
    val countedSchema = Schema
      .simple(SchemaShape.unit[IO](fields("echo" -> lift(argument)((value, _) => value), "counted" -> counted)))
      .unsafeRunSync()
    val query = "query Echo($n: Int!, $m: Int!) { echo(value: $n) counted(value: $m) }"
    val cached = compiler
      .parsePrep(countedSchema, QueryParameters(query, None, None))
      .fold(error => fail(error.toString), identity)
    val preparedReads = outputReads.get()
    assert(preparedReads > 0)
    assertEquals(argumentReads.get(), 0)

    List(Map("n" -> Json.fromString("invalid"), "m" -> Json.fromInt(1)), Map("m" -> Json.fromInt(2))).foreach { variables =>
      val before = argumentReads.get()
      cached.run(variables) match {
        case Left(CompilationError.Preparation(errors)) =>
          assertEquals(errors.toChain.toList.size, 1)
          assertEquals(errors.head.position, if (variables.contains("n")) Cursor.empty.field("n").field("Int") else Cursor.empty)
        case _ => fail("Expected root variable validation error")
      }
      assert(argumentReads.get() > before)
      assertEquals(outputReads.get(), preparedReads)
    }

    val root = cached.run(Map("n" -> Json.fromInt(1), "m" -> Json.fromInt(3)))
    assertEquals(outputReads.get(), preparedReads)
    root match {
      case Left(error) => fail(error.toString)
      case Right(root) =>
        compiler.compilePrepared(countedSchema, root) match {
          case Application.Query(run) =>
            run.map(result => assertEquals(result.data, JsonObject("echo" -> Json.fromInt(1), "counted" -> Json.fromInt(3))))
          case _ => fail("Expected query")
        }
    }
  }

  test("supplied null overrides optional variable defaults across cached bindings") {
    val optional = arg[Option[Int]]("value")
    val optionalSchema = Schema
      .simple(SchemaShape.unit[IO](fields("echo" -> lift(optional)((value, _) => value.getOrElse(-1)))))
      .unsafeRunSync()
    val cached = compiler
      .parsePrep(optionalSchema, QueryParameters("query Echo($n: Int = 7) { echo(value: $n) }", None, None))
      .fold(error => fail(error.toString), identity)

    List(Map.empty[String, Json] -> 7, Map("n" -> Json.Null) -> -1, firstVars -> 1, Map.empty[String, Json] -> 7).traverse_ {
      case (variables, expected) =>
        cached.run(variables) match {
          case Left(error) => IO(fail(error.toString))
          case Right(root) =>
            compiler.compilePrepared(optionalSchema, root) match {
              case Application.Query(run) => run.map(result => assertEquals(result.data, JsonObject("echo" -> Json.fromInt(expected))))
              case _                      => IO(fail("Expected query"))
            }
        }
    }
  }

  test("structural errors stop later phases without requiring variables") {
    val cases = List(
      ("query Echo($n: Int!) { positive(value: $n) unknown }", Cursor.empty, "Field 'unknown' is not a member of `Query`."),
      (
        "query Echo($n: Int!) { abstract { positive(value: $n) unknown } }",
        Cursor.empty.field("abstract"),
        "Field 'unknown' is not a member of `CachedInterface`."
      ),
      ("query Echo($n: Int!) { positive(value: $n) ... Missing }", Cursor.empty, "Unknown fragment name 'Missing'."),
      (
        "query Echo($n: Int!) { abstract { positive(value: $n) { invalid } } }",
        Cursor.empty.field("abstract").field("positive"),
        "Field `positive` of scalar type `Int` must not have a selection set."
      )
    )
    cases.foreach { case (query, cursor, message) =>
      prepare(query, None) match {
        case Left(CompilationError.Preparation(errors)) =>
          assertEquals(errors.toChain.toList.map(_.message), List(message))
          assertEquals(errors.toChain.toList.map(_.position), List(cursor))
        case _ => fail("Expected structural error during preparation")
      }
    }
  }

  test("duplicate arguments suppress dependent argument errors") {
    prepare("{ flag(unexpected: 1, unexpected: 2) }", None) match {
      case Left(CompilationError.Preparation(errors)) =>
        assertEquals(errors.toChain.toList.map(_.message), List("Duplicate argument names found: 'unexpected'."))
      case _ => fail("Expected duplicate argument error")
    }
  }

  test("merge errors suppress preparation errors while static preparation remains eager") {
    val outputReads = new AtomicInteger(0)
    val field = Field[IO, Unit, Boolean](
      Resolver.lift[IO, Unit](_ => true),
      Eval.always {
        outputReads.incrementAndGet()
        booleanScalar
      }
    )
    val countedSchema = Schema
      .simple(
        SchemaShape.unit[IO](fields("echo" -> lift(argument)((value, _) => value), "flag" -> field))
      )
      .unsafeRunSync()
    outputReads.set(0)
    val query = "{ same: echo(value: 1) same: flag(unexpected: 1) }"
    val prepared = compiler
      .parsePrep(countedSchema, QueryParameters(query, None, None))
    // Collection and preparation both inspect output, despite the merge failure.
    val preparedReads = outputReads.get()
    assert(preparedReads >= 2)
    prepared match {
      case Left(CompilationError.Preparation(errors)) =>
        val messages = errors.toChain.toList.map(_.message)
        assert(messages.exists(_.contains("must have the same name")))
        assert(messages.exists(_.contains("Scalars are not the same")))
        assert(!messages.exists(_.contains("Too many arguments")))
      case _ => fail("Expected merge error during preparation")
    }
  }

  test("validation and build share normalized fragment handlers") {
    val inlineCalls = new AtomicInteger(0)
    val namedCalls = new AtomicInteger(0)
    val directive = Directive[Unit]("observe")
    val inline = Position.InlineFragmentSpread[Unit](
      directive,
      new Position.QueryHandler[QA.InlineFragment, Unit] {
        def apply[C](value: Unit, query: QA.InlineFragment[C]): Either[String, List[QA.InlineFragment[C]]] = {
          inlineCalls.incrementAndGet()
          Right(List(query))
        }
      }
    )
    val named = Position.FragmentSpread[Unit](
      directive,
      new Position.QueryHandler[QA.FragmentSpread, Unit] {
        def apply[C](value: Unit, query: QA.FragmentSpread[C]): Either[String, List[QA.FragmentSpread[C]]] = {
          namedCalls.incrementAndGet()
          Right(List(query))
        }
      }
    )
    val shape = SchemaShape
      .unit[IO](fields("positive" -> lift(positiveArgument)((value, _) => value), "echo" -> lift(argument)((value, _) => value)))
      .copy(positions = List(inline, named))
    val fragmentSchema = Schema.simple(shape).unsafeRunSync()
    val query = """query Echo($n: Int!) {
      ... @observe { inline: positive(value: $n) }
      ... EchoFields @observe
    }
    fragment EchoFields on Query { named: echo(value: $n) }"""
    val cached = compiler
      .parsePrep(fragmentSchema, QueryParameters(query, None, None))
      .fold(error => fail(error.toString), identity)
    assertEquals(inlineCalls.get(), 1)
    assertEquals(namedCalls.get(), 1)

    assert(cached.run(firstVars).isRight)
    assert(cached.run(secondVars).isRight)
    cached.run(Map("n" -> Json.fromInt(-1))) match {
      case Left(CompilationError.Preparation(errors)) =>
        assertEquals(errors.toChain.toList.map(_.message), List("Must be positive"))
      case _ => fail("Expected normalized fragment argument validation error")
    }
    assertEquals(inlineCalls.get(), 1)
    assertEquals(namedCalls.get(), 1)
  }

  test("static field directives are cached while validation errors retain precedence") {
    val calls = new AtomicInteger(0)
    val rejection = Position.Field[IO, Unit](
      Directive[Unit]("reject"),
      new Position.FieldHandler[IO, Unit] {
        def apply[I, C](
            value: Unit,
            field: Field[IO, I, ?],
            info: MergedFieldInfo[IO, C]
        ): Either[String, List[(Field[IO, I, ?], MergedFieldInfo[IO, C])]] = {
          calls.incrementAndGet()
          Left("Directive rejects")
        }
      }
    )
    val shape = SchemaShape
      .unit[IO](
        fields("positive" -> lift(positiveArgument)((value, _) => value), "plain" -> lift(_ => 1))
      )
      .copy(positions = List(rejection))
    val directiveSchema = Schema.simple(shape).unsafeRunSync()
    val query = "query Echo($n: Int!) { positive(value: $n) plain @reject }"
    val cached = compiler
      .parsePrep(directiveSchema, QueryParameters(query, None, None))
      .fold(error => fail(error.toString), identity)
    assertEquals(calls.get(), 1)

    cached.run(Map("n" -> Json.fromInt(-1))) match {
      case Left(CompilationError.Preparation(errors)) => assertEquals(errors.toChain.toList.map(_.message), List("Must be positive"))
      case _                                          => fail("Expected argument validation error")
    }
    assertEquals(calls.get(), 1)

    cached.run(firstVars) match {
      case Left(CompilationError.Preparation(errors)) => assertEquals(errors.toChain.toList.map(_.message), List("Directive rejects"))
      case _                                          => fail("Expected cached directive rejection")
    }
    assertEquals(calls.get(), 1)
  }

  test("cached metadata uses variables from each binding") {
    val query = "query Echo($n: Int!) { echo(value: $n) meta }"
    for {
      cache <- QueryCache[IO, Unit, Unit, Unit](maxEntries = 1)(prepare)
      firstPrepared <- cache.compile(QueryParameters(query, Some(firstVars), None))
      secondPrepared <- cache.compile(QueryParameters(query, Some(secondVars), None))
      results <- (execute(firstPrepared), execute(secondPrepared)).parTupled
      _ <- IO {
        val (first, second) = results
        assertEquals(first, JsonObject("echo" -> Json.fromInt(1), "meta" -> Json.fromString("1")))
        assertEquals(second, JsonObject("echo" -> Json.fromInt(2), "meta" -> Json.fromString("2")))
      }
    } yield ()
  }

  test("runtime metadata retains supplied and default variables for every operation and stream update") {
    val resolver = Resolver.meta[IO, Unit].arg(argument).map { case (value, meta) =>
      s"$value:${meta.queryMeta.variables("n").value.fold(_.noSpaces, _ => "default")}"
    }
    val streaming = Resolver
      .argument[IO, Unit, Int](argument)
      .streamMap(value => fs2.Stream.emits(List(value, value + 1)).covary[IO])
      .meta[IO]
      .map { case (meta, value) =>
        s"$value:${meta.queryMeta.variables("n").value.fold(_.noSpaces, _ => "default")}"
      }
    val metadataSchema = Schema
      .simple(
        SchemaShape.unit[IO](
          builder[IO, Unit](b => b.fields("meta" -> b.from(resolver))),
          mutation = Some(builder[IO, Unit](b => b.fields("meta" -> b.from(resolver)))),
          subscription = Some(builder[IO, Unit](b => b.fields("meta" -> b.from(streaming))))
        )
      )
      .unsafeRunSync()

    List("query", "mutation", "subscription").traverse_ { operation =>
      val cached = compiler
        .parsePrep(metadataSchema, QueryParameters(s"$operation Echo($$n: Int! = 7) { meta(value: $$n) }", None, None))
        .fold(error => fail(error.toString), identity)
      List((secondVars, 2, "2"), (Map.empty[String, Json], 7, "default"), (firstVars, 1, "1")).traverse_ {
        case (variables, value, metadata) =>
          val root = cached.run(variables).fold(error => fail(error.toString), identity)
          val results = compiler.compilePrepared(metadataSchema, root, accumulate = None) match {
            case Application.Query(run)           => run.map(List(_))
            case Application.Mutation(run)        => run.map(List(_))
            case Application.Subscription(stream) => stream.take(2).compile.toList
          }
          results.map { results =>
            results.foreach(result => assertEquals(result.errors.toList, Nil))
            val values = if (operation == "subscription") List(value, value + 1) else List(value)
            assertEquals(results.map(_.data), values.map(value => JsonObject("meta" -> Json.fromString(s"$value:$metadata"))))
          }
      }
    }
  }

  test("metadata owner contains bound arguments and its actual selection") {
    val query = "query Owner($n: Int!) { chosen: owner(value: $n) { selected: summary } }"
    for {
      cache <- QueryCache[IO, Unit, Unit, Unit](maxEntries = 1)(prepare)
      first <- cache.compile(QueryParameters(query, Some(firstVars), None)).flatMap(execute)
      second <- cache.compile(QueryParameters(query, Some(secondVars), None)).flatMap(execute)
      _ <- IO {
        assertEquals(first, JsonObject("chosen" -> Json.obj("selected" -> Json.fromString("chosen:1:selected"))))
        assertEquals(second, JsonObject("chosen" -> Json.obj("selected" -> Json.fromString("chosen:2:selected"))))
      }
    } yield ()
  }

  test("variable binding reuses prepared node identities") {
    val cached = prepare("query Echo($n: Int!) { echo(value: $n) flag meta }", None).fold(error => fail(error.toString), identity)
    val first = cached.run(firstVars).fold(error => fail(error.toString), identity)
    val second = cached.run(secondVars).fold(error => fail(error.toString), identity)

    (first.operation, second.operation) match {
      case (PreparedRoot.Query(a), PreparedRoot.Query(b)) =>
        assert(a.nodeId.id eq b.nodeId.id)
        assertEquals(a.fields.size, b.fields.size)
        a.fields.zip(b.fields).foreach {
          case (x: PreparedSpecification[IO, ?, ?], y: PreparedSpecification[IO, ?, ?]) =>
            assert(x.nodeId.id eq y.nodeId.id)
            assertEquals(x.selection.map(_.outputName), y.selection.map(_.outputName))
            x.selection.zip(y.selection).foreach { case (xf, yf) =>
              assert(xf.nodeId.id eq yf.nodeId.id)
              assert(xf.cont.edges.nodeId.id eq yf.cont.edges.nodeId.id)
              if (xf.name != "echo") assert(xf eq yf)
            }
          case _ => fail("Expected prepared specifications")
        }
      case _ => fail("Expected query roots")
    }
  }

  test("cached mutation and subscription roots bind separately") {
    List("mutation", "subscription").traverse_ { operation =>
      val query = s"$operation Echo($$n: Int!) { echo(value: $$n) }"
      for {
        cache <- QueryCache[IO, Unit, Unit, Unit](maxEntries = 1)(prepare)
        first <- cache.compile(QueryParameters(query, Some(firstVars), None)).flatMap(execute)
        second <- cache.compile(QueryParameters(query, Some(secondVars), None)).flatMap(execute)
        _ <- IO {
          assertEquals(first, JsonObject("echo" -> Json.fromInt(1)))
          assertEquals(second, JsonObject("echo" -> Json.fromInt(2)))
        }
      } yield ()
    }
  }

  test("cached directives bind separately for each request") {
    val query = "query Echo($n: Int!, $show: Boolean!) { echo(value: $n) optional: echo(value: 9) @include(if: $show) }"
    for {
      cache <- QueryCache[IO, Unit, Unit, Unit](maxEntries = 1)(prepare)
      hidden <- cache.compile(QueryParameters(query, Some(firstVars + ("show" -> Json.False)), None)).flatMap(execute)
      shown <- cache.compile(QueryParameters(query, Some(secondVars + ("show" -> Json.True)), None)).flatMap(execute)
      _ <- IO {
        assertEquals(hidden, JsonObject("echo" -> Json.fromInt(1)))
        assertEquals(shown, JsonObject("echo" -> Json.fromInt(2), "optional" -> Json.fromInt(9)))
      }
    } yield ()
  }

  test("cached inline and named fragment directives bind hidden shown hidden independently") {
    val query = """query Echo($n: Int!, $show: Boolean!) {
      echo(value: $n)
      ... @include(if: $show) { inline: echo(value: 9) }
      ... Optional @include(if: $show)
    }
    fragment Optional on Query { named: echo(value: 8) }"""
    for {
      cache <- QueryCache[IO, Unit, Unit, Unit](maxEntries = 1)(prepare)
      hidden <- cache.compile(QueryParameters(query, Some(firstVars + ("show" -> Json.False)), None)).flatMap(execute)
      shown <- cache.compile(QueryParameters(query, Some(secondVars + ("show" -> Json.True)), None)).flatMap(execute)
      hiddenAgain <- cache.compile(QueryParameters(query, Some(firstVars + ("show" -> Json.False)), None)).flatMap(execute)
      _ <- IO {
        assertEquals(hidden, JsonObject("echo" -> Json.fromInt(1)))
        assertEquals(shown, JsonObject("echo" -> Json.fromInt(2), "inline" -> Json.fromInt(9), "named" -> Json.fromInt(8)))
        assertEquals(hiddenAgain, hidden)
      }
    } yield ()
  }

  test("variables on skipped fields remain used and validated") {
    val query = "query Echo($n: Int!) { echo(value: 9) hidden: echo(value: $n) @include(if: false) }"
    val cached = prepare(query, None).fold(error => fail(error.toString), identity)
    assert(cached.run(Map("n" -> Json.fromString("invalid"))).isLeft)
    assert(cached.run(Map.empty).isLeft)

    execute(cached.run(firstVars)).map(result => assertEquals(result, JsonObject("echo" -> Json.fromInt(9))))
  }

  test("variables only in excluded fragments fail unused-variable validation during preparation") {
    val query = "query Echo($n: Int!) { echo(value: 9) ... @include(if: false) { hidden: echo(value: $n) } }"
    prepare(query, None) match {
      case Left(CompilationError.Preparation(errors)) => assert(errors.toChain.toList.exists(_.message.contains("Unused variables")))
      case _                                          => fail("Expected unused variable error")
    }
  }

  test("skipped fields retain field-specific argument validation") {
    val query = "query Echo($n: Int!) { echo(value: 9) hidden: positive(value: $n) @include(if: false) }"
    val cached = prepare(query, None).fold(error => fail(error.toString), identity)

    cached.run(Map("n" -> Json.fromInt(-1))) match {
      case Left(CompilationError.Preparation(errors)) =>
        assertEquals(
          errors.toChain.toList.map(error => (error.message, error.position)),
          List(("Must be positive", Cursor.empty.field("positive")))
        )
      case _ => fail("Expected field-specific argument validation error")
    }

    execute(cached.run(firstVars)).map(result => assertEquals(result, JsonObject("echo" -> Json.fromInt(9))))
  }

  test("abstract fields retain argument validation beyond their concrete implementation") {
    val query = "query Echo($n: Int!) { abstract { positive(value: $n) } }"
    val cached = prepare(query, None).fold(error => fail(error.toString), identity)

    cached.run(Map("n" -> Json.fromInt(-1))) match {
      case Left(CompilationError.Preparation(errors)) =>
        assertEquals(
          errors.toChain.toList.map(error => (error.message, error.position)),
          List(("Must be positive", Cursor.empty.field("abstract").field("positive")))
        )
      case _ => fail("Expected abstract argument validation error")
    }

    execute(cached.run(firstVars)).map { result =>
      assertEquals(result, JsonObject("abstract" -> Json.obj("positive" -> Json.fromInt(1))))
    }
  }

  test("abstract fields validate arguments and bind variables") {
    val query = "query Echo($n: Int!) { abstract { echo(value: $n) } }"
    val cached = prepare(query, None).fold(error => fail(error.toString), identity)
    assert(cached.run(Map("n" -> Json.fromString("invalid"))).isLeft)

    for {
      first <- execute(cached.run(firstVars))
      second <- execute(cached.run(secondVars))
      _ <- IO {
        assertEquals(first, JsonObject("abstract" -> Json.obj("echo" -> Json.fromInt(1))))
        assertEquals(second, JsonObject("abstract" -> Json.obj("echo" -> Json.fromInt(2))))
        val invalidLiteral = prepare("{ abstract { echo(value: \"invalid\") } }", None).flatMap(_.run(Map.empty))
        val missingArgument = prepare("{ abstract { echo } }", None).flatMap(_.run(Map.empty))
        assert(invalidLiteral.isLeft)
        assert(missingArgument.isLeft)
      }
    } yield ()
  }

  test("concurrent bindings share preparation without sharing variables") {
    for {
      cache <- QueryCache[IO, Unit, Unit, Unit](maxEntries = 1)(prepare)
      cached = prepare(echo, None).fold(error => fail(error.toString), identity)
      _ <- cache.persist(cached)
      results <- (1 to 20).toList.parTraverse { value =>
        cache.compile(QueryParameters(echo, Some(Map("n" -> Json.fromInt(value))), None)).flatMap(execute)
      }
      _ <- IO(assertEquals(results, (1 to 20).toList.map(value => JsonObject("echo" -> Json.fromInt(value)))))
    } yield ()
  }

  test("query text and operation name identify separate entries") {
    val query = "query First { echo(value: 1) } query Second { echo(value: 2) }"
    for {
      cache <- QueryCache[IO, Unit, Unit, Unit](maxEntries = 3)(prepare)
      first <- cache.compile(QueryParameters(query, None, Some("First"))).flatMap(execute)
      second <- cache.compile(QueryParameters(query, None, Some("Second"))).flatMap(execute)
      firstEntry <- cache.getPrep(query, Some("First"))
      secondEntry <- cache.getPrep(query, Some("Second"))
      noOperation <- cache.getPrep(query)
      alteredText <- cache.getPrep(query + " ", Some("First"))
      _ <- IO {
        assertEquals(first, JsonObject("echo" -> Json.fromInt(1)))
        assertEquals(second, JsonObject("echo" -> Json.fromInt(2)))
        assert(firstEntry.isDefined)
        assert(secondEntry.isDefined)
        assert(firstEntry.get ne secondEntry.get)
        assertEquals(noOperation, None)
        assertEquals(alteredText, None)
      }
    } yield ()
  }

  test("getPrep refreshes recency before least recently used eviction") {
    val first = prepare("{ echo(value: 1) }", None).fold(error => fail(error.toString), identity)
    val second = prepare("{ echo(value: 2) }", None).fold(error => fail(error.toString), identity)
    val third = prepare("{ echo(value: 3) }", None).fold(error => fail(error.toString), identity)
    for {
      cache <- QueryCache[IO, Unit, Unit, Unit](maxEntries = 2)(prepare)
      _ <- cache.persist(first)
      _ <- cache.persist(second)
      _ <- cache.getPrep("{ echo(value: 1) }")
      _ <- cache.persist(third)
      evicted <- cache.getPrep("{ echo(value: 2) }")
      retainedFirst <- cache.getPrep("{ echo(value: 1) }")
      retainedThird <- cache.getPrep("{ echo(value: 3) }")
      _ <- IO {
        assertEquals(evicted, None)
        assert(retainedFirst.contains(first))
        assert(retainedThird.contains(third))
      }
    } yield ()
  }

  test("compile hits refresh recency and preparation misses persist automatically") {
    val first = "{ echo(value: 1) }"
    val second = "{ echo(value: 2) }"
    val third = "{ echo(value: 3) }"
    for {
      cache <- QueryCache[IO, Unit, Unit, Unit](maxEntries = 2)(prepare)
      _ <- cache.compile(QueryParameters(first, None, None))
      _ <- cache.compile(QueryParameters(second, None, None))
      _ <- cache.compile(QueryParameters(first, None, None))
      _ <- cache.compile(QueryParameters(third, None, None))
      evicted <- cache.getPrep(second)
      retainedFirst <- cache.getPrep(first)
      retainedThird <- cache.getPrep(third)
      _ <- IO {
        assertEquals(evicted, None)
        assert(retainedFirst.isDefined)
        assert(retainedThird.isDefined)
      }
    } yield ()
  }

  test("persist replaces existing entry and refreshes its recency") {
    val query = "{ echo(value: 1) }"
    val original = prepare(query, None).fold(error => fail(error.toString), identity)
    val replacement = prepare(query, None).fold(error => fail(error.toString), identity)
    val second = prepare("{ echo(value: 2) }", None).fold(error => fail(error.toString), identity)
    val third = prepare("{ echo(value: 3) }", None).fold(error => fail(error.toString), identity)
    for {
      cache <- QueryCache[IO, Unit, Unit, Unit](maxEntries = 2)(prepare)
      _ <- cache.persist(original)
      _ <- cache.persist(second)
      _ <- cache.persist(replacement)
      _ <- cache.persist(third)
      retained <- cache.getPrep(query)
      evicted <- cache.getPrep("{ echo(value: 2) }")
      _ <- IO {
        assert(retained.exists(_ eq replacement))
        assertEquals(evicted, None)
      }
    } yield ()
  }

  test("parse and static preparation errors are not cached") {
    List("query {", "{ unknownField }").traverse_ { query =>
      val preparations = new AtomicInteger(0)
      for {
        cache <- QueryCache[IO, Unit, Unit, Unit](maxEntries = 1) { (text, operationName) =>
          preparations.incrementAndGet()
          prepare(text, operationName)
        }
        first <- cache.compile(QueryParameters(query, None, None))
        second <- cache.compile(QueryParameters(query, None, None))
        entry <- cache.getPrep(query)
        _ <- IO {
          assert(first.isLeft)
          assert(second.isLeft)
          assertEquals(entry, None)
          assertEquals(preparations.get(), 2)
        }
      } yield ()
    }
  }

  test("concurrent persistence respects capacity") {
    val queries = (1 to 20).toList.map(value => s"{ echo(value: $value) }")
    val prepared = queries.map(query => prepare(query, None).fold(error => fail(error.toString), identity))
    for {
      cache <- QueryCache[IO, Unit, Unit, Unit](maxEntries = 2)(prepare)
      _ <- prepared.parTraverse_(cache.persist)
      entries <- queries.traverse(cache.getPrep(_))
      _ <- IO(assertEquals(entries.count(_.isDefined), 2))
    } yield ()
  }

  test("cache requires positive capacity") {
    List(0, -1).traverse_ { capacity =>
      QueryCache[IO, Unit, Unit, Unit](maxEntries = capacity)(prepare).attempt.map {
        case Left(_: IllegalArgumentException) => ()
        case _                                 => fail("Expected invalid capacity failure")
      }
    }
  }
}
