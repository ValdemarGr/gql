val pr = ProjectRef(file(".."), "clientCodegenCli")

lazy val codegenTest = project
  .in(file("."))
  .enablePlugins(GqlCodeGenPlugin)
  .aggregate(pr)
  .settings(
    scalaVersion := "2.13.9",
    Gql.libraryVersion := "0.1-305cbf3",
    Compile / Gql.resourceGroups += Gql.resourceGroup(
      file("./src/main/resources/schema.graphql"),
      file("./src/main/resources/query.graphql"),
    )
  )
