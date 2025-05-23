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
import cats.effect.std._
import fs2.Pure
import gql.preparation.PreparedRoot
import gql.SchemaShape

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

  def mainGenerate[F[_]: Async: Console](
      schemaPath: Path,
      sharedPath: Path,
      validate: Boolean,
      packageName: Option[String]
  )(data: List[Input]): F[List[String]] = {
    type G[A] = EitherT[F, NonEmptyChain[String], A]
    implicit val files: Files[G] = Files.forAsync[G]
    Logger
      .make[G](Console[G].println[String])
      .flatMap { lg =>
        val g = new Generator[G](lg)
        g.readAndGenerate(schemaPath, sharedPath, validate, packageName.getOrElse("gql.client.generated"))(data)
      }
      .value
      .map(_.fold(_.toList, _ => Nil))
  }
}

import Generator._
class Generator[F[_]: Files](lg: Logger[F])(implicit
    F: Async[F],
    E: Err[F]
) {
  val ga = GenAst.make

  def generateFor(
      env: GenAst.Env,
      query: NonEmptyList[ExecutableDefinition[Caret]]
  ): F[(Set[GenAst.UsedInput], Doc)] =
    lg.log(s"Generating executable definitions...") *>
      ga
        .generateExecutableDefs(env, query)
        .run
        .value
        .run(Chain.empty)
        .value
        .fold(E.raise, F.pure)

  def getSchemaFrom(s: String): Either[String, Map[String, TypeDefinition]] =
    gql.parser.parseSchema(s).leftMap(_.prettyError.value).map(_.map(td => td.name -> td).toList.toMap)

  def readInputData(i: Input): F[(String, NonEmptyList[ExecutableDefinition[Caret]])] =
    lg.log(s"Reading input data for ${i.query}") *>
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
    lg.log(s"Generating code for ${i.query}") *>
      readInputData(i)
        .flatMap { case (q, eds) => generateFor(env, eds) tupleLeft ((q, eds)) }
        .map { case ((q, eds), (usedInputs, doc)) => (q, usedInputs, Output(i.output, doc), eds) } <*
      lg.log(s"Done generating code for ${i.query}")

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
    lg.log(s"Generating code for ${data.map(_.query).mkString_(", ")}") *>
      readEnv(packageName, schemaPath)(data).flatMap { e =>
        lg.log(s"Constructed Env, generating...") *>
          data
            .traverse(d => generateForInput(e, d) tupleLeft d)
            .flatMap { xs =>
              val translated: List[ExecutableDefinition[PositionalInfo]] = xs.flatMap { case (i, (q, _, _, eds)) =>
                eds.toList.map(_.map(c => PositionalInfo(c, i, q)))
              }
              val allFrags = translated.collect { case f: ExecutableDefinition.Fragment[PositionalInfo] => f }
              val allOps = translated.collect { case op: ExecutableDefinition.Operation[PositionalInfo] => op }

              val genStub: F[EitherNec[String, SchemaShape[Pure, _, _, _]]] =
                lg.scoped("generating stub schema") {
                  F.delay(SchemaUtil.stubSchema(e.schema))
                }

              val errors = EitherT(genStub).flatMap { stubSchema =>
                val unSuspend = lg.scoped("forcing discovery of types in stub schema") {
                  F.delay(stubSchema.discover).void
                }

                EitherT.liftF(unSuspend) *>
                  allOps.parTraverse { op =>
                    val full: NonEmptyList[ExecutableDefinition[PositionalInfo]] = NonEmptyList(op, allFrags)
                    def vars: ValidatedNec[String, Map[String, Json]] = op.o match {
                      case QueryAst.OperationDefinition.Simple(_) => Map.empty.validNec
                      case QueryAst.OperationDefinition.Detailed(_, _, vds, _, _) =>
                        vds.toList
                          .flatMap(_.nel.toList)
                          .traverse(x => QueryValidation.generateVariableStub(x, e.schema))
                          .map(_.foldLeft(Map.empty[String, Json])(_ ++ _))
                    }
                    val gennedVars = EitherT {
                      lg.scoped(s"generating stub input vars") {
                        F.delay(vars.toEither)
                      }
                    }

                    gennedVars.flatMap { variables =>
                      val fo: EitherT[F, NonEmptyChainImpl.Type[String], PreparedRoot[Pure, _, _, _]] = EitherT {
                        F.delay {
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

                      fo
                    }
                  }
              }.value

              lazy val validateF: F[Either[Unit, List[PreparedRoot[Pure, _, _, _]]]] =
                lg.log(s"Validating queries..., if this takes long you can skip validation") *>
                  errors.flatMap(_.leftTraverse[F, Unit](E.raise(_))) <*
                  lg.log(s"Done validating")

              val writeF: F[Unit] =
                xs.flatTraverse { case (_, (_, used, x, _)) =>
                  lg.log(s"writing to ${x.path}") *>
                    writeStream(x.path, x.doc).compile.drain.as(used.toList) <*
                    lg.log(s"Done writing to ${x.path}")
                }.flatMap(_.distinct.toNel.traverse_ { nel =>
                  lg.log(s"Generating shared types...") *>
                    F.delay(ga.generateInputs(e, nel.toList, packageName)).flatMap { st =>
                      lg.log(s"Writing shared types to $sharedPath") *>
                        writeStream(sharedPath, st).compile.drain <*
                        lg.log(s"Done writing shared types to $sharedPath")
                    }
                })

              F.whenA(validate)(validateF) *> writeF <* lg.log(s"Done generating code for ${data.map(_.query).mkString_(", ")}")
            }
      }
}
