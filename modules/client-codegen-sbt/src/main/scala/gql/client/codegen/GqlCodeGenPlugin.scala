/*
 * Copyright 2023 Typelevel
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

import sbt._
import Keys._
import buildinfo.BuildInfo

import java.io.File
import scala.util.{Failure, Success}

object GqlCodeGenPlugin extends AutoPlugin {
  def safeListFiles(f: File): List[File] = {
    if (f.exists()) f.listFiles().toList
    else {
      throw new IllegalArgumentException(s"File ${f.getPath()} does not exist.")
    }
  }

  object autoImport {
    object Gql {
      final case class GroupOptions(
          packageName: String = "gql.client.generated"
      ) {
        def addPackageSuffix(suffix: String): GroupOptions =
          copy(packageName = packageName + "." + suffix)
      }

      sealed trait ResourceGroup {
        def modifyOptions(f: GroupOptions => GroupOptions): ResourceGroup
      }
      final case class CustomResourceGroup(
          name: String,
          schemaPath: File,
          files: Seq[File],
          options: GroupOptions = GroupOptions()
      ) extends ResourceGroup {
        def modifyOptions(f: GroupOptions => GroupOptions): CustomResourceGroup =
          copy(options = f(options))
      }
      final case class DefaultResourceGroup(
          options: GroupOptions = GroupOptions()
      ) extends ResourceGroup {
        def modifyOptions(f: GroupOptions => GroupOptions): DefaultResourceGroup =
          copy(options = f(options))
      }

      def resourceGroup(name: String, schema: File, files: File*): ResourceGroup =
        CustomResourceGroup(name, schema, files)

      def resourceGroup(name: String, path: File): ResourceGroup = {
        CustomResourceGroup(
          name,
          path / "schema.graphql",
          safeListFiles((path / "queries"))
        )
      }

      val resourceGroups = settingKey[Seq[ResourceGroup]]("The resource groups")

      val findResources = taskKey[Seq[CustomResourceGroup]]("Find the resources")

      val codeGenInput = taskKey[Seq[CodeGenInput]]("The code generator input")

      val invokeCodeGen = taskKey[Seq[File]]("Invoke the code generator")

      val libraryVersion = settingKey[String]("The CLI library version")

      val validate = settingKey[Boolean]("Validate queries")
    }
  }

  import autoImport._

  case class CodeGenInput(json: String, inFiles: Seq[File], outFiles: Seq[File])

  lazy val GqlCliConfig = config("gql-cli").hide

  override def projectConfigurations: Seq[Configuration] = Seq(GqlCliConfig)

  override def projectSettings: Seq[Setting[_]] =
    List(
      ivyConfigurations += GqlCliConfig,
      Gql.validate := true,
      Gql.resourceGroups := Seq(Gql.DefaultResourceGroup()),
      Gql.findResources := {
        val rs = Gql.resourceGroups.value

        def verifyResourceGroup(rg: Gql.CustomResourceGroup) = {
          val schema = rg.schemaPath
          val queries = rg.files.toList

          val log = streams.value.log

          val hint =
            s"""|Hint:
                |You can remove resource groups via `Gql.resourceGroups -= Gql.resourceGroup(file("./path/to/resources"))`
                |You can also remove the default group `Gql.resourceGroups -= Gql.DefaultResourceGroup`""".stripMargin

          if (!schema.exists()) {
            log.err(
              s"""|A resource group had no schema at ${rg.schemaPath.getPath()}
                  |$hint""".stripMargin
            )
            None
          } else if (queries.isEmpty) {
            log.err(
              s"""|Default resource group used but no queries found for ${(queries.map(_.getPath()).mkString(", "))}
                  |$hint""".stripMargin
            )
            None
          } else Some(rg)
        }

        val customs = rs
          .collect { case rg: Gql.CustomResourceGroup => verifyResourceGroup(rg) }
          .collect { case Some(rg) => rg }
        val default = rs
          .collectFirst { case Gql.DefaultResourceGroup(opts) =>
            val r = (Compile / resourceDirectory).value
            val schema = r / "schema.graphql"
            val queries = Option(r / "queries").toList.flatMap(safeListFiles)
            val rg = Gql.CustomResourceGroup("default", schema, queries, opts)
            verifyResourceGroup(rg)
          }
          .flatten
          .toList

        customs ++ default
      },
      Gql.libraryVersion := BuildInfo.version,
      libraryDependencies += "io.github.valdemargr" %% "gql-client-codegen-cli" % Gql.libraryVersion.value % GqlCliConfig,
      Gql.codeGenInput := {
        val base = (Compile / sourceManaged).value / "gql"
        IO.createDirectory(base)
        val resources = Gql.findResources.value
        resources.map { rg =>
          val f = base / rg.name
          IO.createDirectory(f)

          val sh = f / s"shared.scala"

          import _root_.io.circe._
          import _root_.io.circe.syntax._
          import Json._

          val queryInputs = rg.files.map { in =>
            val fn = in.name.replaceAll("\\.", "_")
            val outFile = f / s"${fn}.scala"

            (
              JsonObject(
                "query" -> fromString(in.absolutePath),
                "output" -> fromString(outFile.absolutePath)
              ).asJson,
              Seq(in),
              Seq(outFile)
            )
          }
          val (queries, inFiless, outFiless) = queryInputs.unzip3

          CodeGenInput(
            json = JsonObject(
              "schema" -> fromString(rg.schemaPath.absolutePath),
              "shared" -> fromString(sh.absolutePath),
              "queries" -> arr(queries: _*),
              "packageName" -> fromString(rg.options.packageName)
            ).asJson.spaces2,
            inFiles = inFiless.flatten :+ rg.schemaPath,
            outFiles = outFiless.flatten ++ Seq(sh)
          )
        }
      },
      Gql.invokeCodeGen := {
        val streamsObj = streams.value
        val cp = update.value.select(configurationFilter(GqlCliConfig.name))
        val cmd = Gql.codeGenInput.value
        val runnerObj = runner.value

        val cachedFun =
          FileFunction.cached(streamsObj.cacheDirectory / "gql-invoke-codegen", inStyle = FilesInfo.lastModified) { _ =>
            val args = List("--validate").filter(_ => Gql.validate.value) ++ List("--input") ++ cmd.map(_.json)

            runnerObj.run("gql.client.codegen.GeneratorCli", cp, args, streamsObj.log) match {
              case Success(_) => cmd.flatMap(_.outFiles).toSet
              case Failure(e) => throw e
            }
          }

        cachedFun(cmd.flatMap(_.inFiles).toSet).toSeq
      },
      Compile / sourceGenerators += Gql.invokeCodeGen.taskValue
    )
}
