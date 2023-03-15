val pr = ProjectRef(file(".."), "clientCodegenCli")

lazy val codegenTest = project
  .in(file("."))
  .enablePlugins(GqlCodeGenPlugin)
  .aggregate(pr)
  .settings(
    Compile / Gql.resourceGroups += Gql.resourceGroup(
      file("./src/main/resources/schema.graphql"),
      file("./src/main/resources/query.graphql"),
    )
  )
