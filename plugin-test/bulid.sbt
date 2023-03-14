
lazy val codegenTest = project
  .in(file("."))
  .enablePlugins(GqlCodeGenPlugin)
  .settings(
    Compile / Gql.resourceGroups += Gql.resourceGroup(
      file("./src/main/resources/schema.graphql"),
      file("./src/main/resources/query.graphql"),
    )
  )
