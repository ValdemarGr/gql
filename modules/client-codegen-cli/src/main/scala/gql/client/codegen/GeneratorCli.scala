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
package gql.client.codegen

import cats.effect._
import cats.implicits._
import com.monovore.decline._
import com.monovore.decline.effect._
import fs2.io.file._
import cats.data._
import io.circe._

object GeneratorCli
    extends CommandIOApp(
      name = "gql-client-codegen",
      header = "GraphQL client code generator for GQL"
    ) {
  implicit val argForPath: Argument[Path] = Argument.from[Path]("path") { path =>
    Either.catchNonFatal(Path(path)).leftMap(_.getMessage).toValidatedNel
  }

  implicit def argForTuple[A: Argument, B: Argument]: Argument[(A, B)] =
    Argument.from[(A, B)](Argument[A].defaultMetavar + "=" + Argument[B].defaultMetavar) { str =>
      str.split('=').toList match {
        case x :: y :: Nil => (Argument[A].read(x), Argument[B].read(y)).mapN(_ -> _)
        case _             => s"Invalid key=value pair: $str".invalidNel
      }
    }

  final case class Query(
      query: Path,
      output: Option[Path]
  )
  final case class Input(
      schema: Path,
      queries: NonEmptyList[Query]
  )
  val kvPairs =
    Opts.options[Input](
      "input",
      help = """|Path to the GraphQL schemas and query files, given as json value.
                |For instance: {"schema": "/my/schema", "queries": [{ "query": "/my/query", "output": "/my/Query.scala" }] }.
                |Omitting "output" will generate the file beside the input query""".stripMargin
    ) {
      Argument.from[Input]("json-input") { str =>
        implicit val decoderForPath: Decoder[Path] = Decoder.decodeString.emap { str =>
          Either.catchNonFatal(Path(str)).leftMap(_.getMessage)
        }

        implicit val queryDec: Decoder[Query] = Decoder.instance[Query] { c =>
          (
            c.downField("query").as[Path],
            c.downField("output").as[Option[Path]]
          ).mapN(Query.apply)
        }

        implicit val inputDec: Decoder[Input] = Decoder.instance[Input] { c =>
          (
            c.get[Path]("schema"),
            c.get[NonEmptyList[Query]]("queries")
          ).mapN(Input.apply)
        }

        io.circe.parser.decode[Input](str).leftMap(_.show).toValidatedNel
      }
    }

  override def main: Opts[IO[ExitCode]] =
    kvPairs
      .map { kvs =>
        fs2.Stream
          .emits(kvs.toList)
          .lift[IO]
          .flatMap { i =>
            fs2.Stream
              .emits(
                i.queries.toList.map(q =>
                  Generator.Input(q.query, q.output.getOrElse(q.query.parent.get / (q.query.fileName.toString + ".scala")))
                )
              )
              .lift[IO]
              .through(Generator.readGenerateWrite[IO](i.schema))
          }
          .compile
          .foldMonoid
          .flatMap {
            case Nil => IO.unit
            case xs  => IO.raiseError(new Exception(s"Failed to generate code for ${xs.mkString(", ")}"))
          }
          .as(ExitCode.Success)
      }
}