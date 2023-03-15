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
      }
    )
}
