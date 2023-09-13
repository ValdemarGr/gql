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
  object autoImport {
    object Gql {
      sealed trait ResourceGroup
      final case class CustomResourceGroup(
          name: String,
          schemaPath: File,
          files: Seq[File]
      ) extends ResourceGroup
      case object DefaultResourceGroup extends ResourceGroup

      def resourceGroup(name: String, schema: File, files: File*): ResourceGroup =
        CustomResourceGroup(name, schema, files)

      def resourceGroup(name: String, path: File): ResourceGroup = {
        CustomResourceGroup(
          name,
          path / "schema.graphql",
          (path / "queries").listFiles().toList
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
      Gql.resourceGroups := Seq(Gql.DefaultResourceGroup),
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
          } else Some(Gql.CustomResourceGroup(rg.name, schema, queries))
        }

        val customs = rs
          .collect { case rg: Gql.CustomResourceGroup => verifyResourceGroup(rg) }
          .collect { case Some(rg) => rg }
        val default = rs
          .collectFirst { case Gql.DefaultResourceGroup =>
            val r = (Compile / resourceDirectory).value
            val schema = r / "schema.graphql"
            val queries = Option((r / "queries").listFiles()).toList.flatMap(_.toList)
            val rg = Gql.CustomResourceGroup("default", schema, queries)
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

          def escapeSlash(s: String) = s.replace("\\", "\\\\")

          val queryInputs = rg.files.map { in =>
            val fn = in.name.replaceAll("\\.", "_")
            val outFile = f / s"${fn}.scala"

            CodeGenInput(
              json = s"""{"query": "${escapeSlash(in.absolutePath)}", "output": "${escapeSlash(outFile.absolutePath)}"}""",
              inFiles = Seq(in),
              outFiles = Seq(outFile)
            )
          }
          val (queries, inFiless, outFiless) = queryInputs.flatMap(CodeGenInput.unapply).unzip3

          CodeGenInput(
            json =
              s"""{"schema":"${escapeSlash(rg.schemaPath.absolutePath)}","shared":"${escapeSlash(sh.absolutePath)}","queries":[${queries
                  .mkString(",")}]}""",
            inFiles = inFiless.flatten,
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
          FileFunction.cached(streamsObj.cacheDirectory / "gql-invoke-codegen", inStyle = FilesInfo.lastModified, outStyle = FilesInfo.exists) { _ =>
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
