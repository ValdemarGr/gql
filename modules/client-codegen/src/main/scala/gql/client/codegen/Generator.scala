/*
 * Copyright 2024 Valdemar Grange
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
import fs2.io.file._
import gql.parser.TypeSystemAst._
import gql.parser.QueryAst._
import cats.data._
import org.typelevel.paiges.Doc
import cats.implicits._
import cats.mtl.Handle
import cats.parse.Caret
import gql.parser.QueryAst
import gql.client.QueryValidation
import io.circe.Json
import gql.preparation.RootPreparation
import gql.parser.ParserUtil
import gql.util.SchemaUtil

object Generator {
  type Err[F[_]] = Handle[F, NonEmptyChain[String]]

  final case class Input(
      query: Path,
      output: Path
  )
  final case class Output(
      path: Path,
      doc: Doc
  )
  final case class PositionalInfo(
      caret: Caret,
      input: Input,
      sourceQuery: String
  )

  def mainGenerate[F[_]: Async](
      schemaPath: Path,
      sharedPath: Path,
      validate: Boolean,
      packageName: Option[String]
  )(data: List[Input]): F[List[String]] = {
    type G[A] = EitherT[F, NonEmptyChain[String], A]
    implicit val files: Files[G] = Files.forAsync[G]
    val g = new Generator[G]
    g.readAndGenerate(schemaPath, sharedPath, validate, packageName.getOrElse("gql.client.generated"))(data)
      .value
      .map(_.fold(_.toList, _ => Nil))
  }
}

import Generator._
class Generator[F[_]: Files](implicit
    F: Async[F],
    E: Err[F]
) {
  val ga = GenAst.make

  def generateFor(
      env: GenAst.Env,
      query: NonEmptyList[ExecutableDefinition[Caret]]
  ): F[(Set[GenAst.UsedInput], Doc)] =
    ga.generateExecutableDefs(env, query)
      .run
      .value
      .run(Chain.empty)
      .value
      .fold(E.raise, F.pure)

  def getSchemaFrom(s: String): Either[String, Map[String, TypeDefinition]] =
    gql.parser.parseSchema(s).leftMap(_.prettyError.value).map(_.map(td => td.name -> td).toList.toMap)

  def readInputData(i: Input): F[(String, NonEmptyList[ExecutableDefinition[Caret]])] =
    Files[F]
      .readAll(i.query)
      .through(fs2.text.utf8.decode[F])
      .compile
      .foldMonoid
      .flatMap { x =>
        val fa: F[NonEmptyList[ExecutableDefinition[Caret]]] =
          gql.parser.parseQuery(x).leftFlatMap(_.prettyError.value.leftNec).fold(E.raise, F.pure)

        fa tupleLeft x
      }

  def generateForInput(
      env: GenAst.Env,
      i: Input
  ): F[(String, Set[GenAst.UsedInput], Output, NonEmptyList[ExecutableDefinition[Caret]])] =
    readInputData(i)
      .flatMap { case (q, eds) => generateFor(env, eds) tupleLeft ((q, eds)) }
      .map { case ((q, eds), (usedInputs, doc)) => (q, usedInputs, Output(i.output, doc), eds) }

  def gatherFragmentInfos(
      xs: NonEmptyList[ExecutableDefinition[Caret]]
  ): List[GenAst.FragmentInfo] =
    xs.collect { case ExecutableDefinition.Fragment(f, _) =>
      GenAst.FragmentInfo(f.name, f.typeCnd)
    }

  def gatherFragInfo(i: Input): F[List[GenAst.FragmentInfo]] =
    readInputData(i).map { case (_, eds) => gatherFragmentInfos(eds) }

  def writeStream(path: Path, doc: Doc) =
    fs2.Stream
      .iterable(doc.renderStream(80))
      .lift[F]
      .through(fs2.text.utf8.encode)
      .through(Files[F].writeAll(path))

  def readSchema(schemaPath: Path): F[Map[String, TypeDefinition]] =
    Files[F]
      .readAll(schemaPath)
      .through(fs2.text.utf8.decode[F])
      .compile
      .foldMonoid
      .flatMap(s => getSchemaFrom(s).fold(s => E.raise(NonEmptyChain.one(s)), F.pure))

  def readEnv(packageName: String, schema: Path)(data: List[Input]): F[GenAst.Env] =
    readSchema(schema).flatMap { m =>
      data
        .flatTraverse(gatherFragInfo)
        .map(f => GenAst.Env(m, f.map(fi => fi.name -> fi).toMap, packageName))
    }

  def readAndGenerate(schemaPath: Path, sharedPath: Path, validate: Boolean, packageName: String)(
      data: List[Input]
  ): F[Unit] =
    readEnv(packageName, schemaPath)(data).flatMap { e =>
      data
        .traverse(d => generateForInput(e, d) tupleLeft d)
        .flatMap { xs =>
          val translated: List[ExecutableDefinition[PositionalInfo]] = xs.flatMap { case (i, (q, _, _, eds)) =>
            eds.toList.map(_.map(c => PositionalInfo(c, i, q)))
          }
          val allFrags = translated.collect { case f: ExecutableDefinition.Fragment[PositionalInfo] => f }
          val allOps = translated.collect { case op: ExecutableDefinition.Operation[PositionalInfo] => op }
          lazy val errors = SchemaUtil.stubSchema(e.schema).flatMap { stubSchema =>
            allOps.parTraverse { op =>
              val full: NonEmptyList[ExecutableDefinition[PositionalInfo]] = NonEmptyList(op, allFrags)
              val vars: ValidatedNec[String, Map[String, Json]] = op.o match {
                case QueryAst.OperationDefinition.Simple(_) => Map.empty.validNec
                case QueryAst.OperationDefinition.Detailed(_, _, vds, _, _) =>
                  vds.toList
                    .flatMap(_.nel.toList)
                    .traverse(x => QueryValidation.generateVariableStub(x, e.schema))
                    .map(_.foldLeft(Map.empty[String, Json])(_ ++ _))
              }
              vars.toEither.flatMap { variables =>
                RootPreparation
                  .prepareRun(full, stubSchema, variables, None)
                  .leftMap(_.map { pe =>
                    val pos = pe.position.formatted
                    val caretErrs = pe.caret.distinct
                      .map { c =>
                        val (msg, _, _) = ParserUtil.showVirtualTextLine(c.sourceQuery, c.caret.offset)
                        s"in file ${c.input.query}\n" + msg
                      }
                    val msg = pe.message
                    s"$msg at $pos\n${caretErrs.mkString_("\n")}"
                  })
              }
            }
          }

          lazy val validateF = errors.leftTraverse[F, Unit](E.raise(_))

          val writeF: F[Unit] =
            xs.flatTraverse { case (_, (_, used, x, _)) =>
              writeStream(x.path, x.doc).compile.drain.as(used.toList)
            }.flatMap(_.distinct.toNel.traverse_ { nel =>
              writeStream(sharedPath, ga.generateInputs(e, nel.toList, packageName)).compile.drain
            })

          F.whenA(validate)(validateF) *> writeF
        }
    }
}
