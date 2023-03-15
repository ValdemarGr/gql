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

object GqlCodeGenPlugin extends AutoPlugin {
  object autoImport {
    object Gql {
      sealed trait ResourceGroup
      final case class CustomResourceGroup(
          schemaPath: File,
          files: Seq[File]
      ) extends ResourceGroup
      case object DefaultResourceGroup extends ResourceGroup

      def resourceGroup(schema: File, files: File*): ResourceGroup =
        CustomResourceGroup(schema, files)

      def resourceGroup(path: File): ResourceGroup = {
        CustomResourceGroup(
          path / "schema.graphql",
          (path / "queries").listFiles().toList
        )
      }

      val resourceGroups = settingKey[Seq[ResourceGroup]]("The resource groups")

      val findResources = taskKey[Seq[CustomResourceGroup]]("Find the resources")

      // "Path to the code generator cli jar"
      // val codeGenCli = taskKey[Attributed[File]]("Path to the code generator cli jar")
      val coursierCmd = taskKey[String]("The coursier command to use")

      val invokeCodeGen = taskKey[Seq[File]]("Invoke the code generator")
    }
  }

  import autoImport._

  override def projectSettings: Seq[Setting[_]] =
    List(
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
          } else Some(Gql.CustomResourceGroup(schema, queries))
        }

        val customs = rs
          .collect { case rg: Gql.CustomResourceGroup => verifyResourceGroup(rg) }
          .collect { case Some(rg) => rg }
        val default = rs
          .collectFirst { case Gql.DefaultResourceGroup =>
            val r = (Compile / resourceDirectory).value
            val schema = r / "schema.graphql"
            val queries = Option((r / "queries").listFiles()).toList.flatMap(_.toList)
            val rg = Gql.CustomResourceGroup(schema, queries)
            verifyResourceGroup(rg)
          }
          .flatten
          .toList

        customs ++ default
      },
      // libraryDependencies += "io.github.valdemargr" % "client-codegen-cli" % "0.1" % Runtime,
      // Gql.codeGenCli := {
      //   val xs = Classpaths.managedJars(Runtime, Set("jar"), update.value)
      //   println(update.value)
      //   println(xs.map(_.data))
      //   println((Runtime / dependencyClasspath).value)
      //   println((Runtime / libraryDependencies).value)
      //   if (xs.size != 1) {
      //     sys.error("Expected exactly one jar in the classpath")
      //   } else {
      //     xs.head
      //   }
      // },
      Gql.coursierCmd := "coursier",
      Gql.invokeCodeGen := {
        val f = (Compile / sourceManaged).value / "gql"
        IO.createDirectory(f)
        val resources = Gql.findResources.value
        val cmd = resources.map { rg =>
          val queries = rg.files.map { in =>
            val fn = f.name.replaceAll("\\.", "_")
            val outFile = f / s"${fn}.scala"
            s"""{"query": "${in.absolutePath}", "output": "${outFile.absolutePath}.scala"}""" -> outFile
          }

          s"""{"schema":"${rg.schemaPath.absolutePath}","queries":[${queries.map(_._1).mkString(",")}]}""" -> queries.map(_._2)
        }
        // Run the code generator from coursier
        val args = List(
          Gql.coursierCmd.value,
          "launch",
          "io.github.valdemargr:client-codegen-cli:0.1",
          "--main",
          "gql.client.codegen.GeneratorCli",
          "--",
          "--input"
        ) ++ cmd.map(_._1)

        scala.sys.process.Process(args, None).! match {
          case 0 => cmd.flatMap(_._2)
          case n => sys.error(s"Process exited with code $n")
        }
      }
    )
}