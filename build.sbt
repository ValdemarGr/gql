val scala213Version = "2.13.9"

ThisBuild / scalaVersion := scala213Version
ThisBuild / crossScalaVersions := Seq(scala213Version, "3.2.2")
ThisBuild / organization := "io.github.valdemargr"

ThisBuild / tlBaseVersion := "0.1"
ThisBuild / tlCiHeaderCheck := false
ThisBuild / tlCiDocCheck := false
ThisBuild / tlCiScalafmtCheck := false
ThisBuild / tlUntaggedAreSnapshots := false

ThisBuild / licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0"))
ThisBuild / developers := List(
  Developer("valdemargr", "Valdemar Grange", "randomvald0069@gmail.com", url("https://github.com/valdemargr"))
)
ThisBuild / headerLicense := Some(HeaderLicense.Custom("Copyright (c) 2023 Valdemar Grange"))
ThisBuild / headerEmptyLine := false

ThisBuild / githubWorkflowAddedJobs +=
  WorkflowJob(
    id = "docs",
    name = "Run mdoc docs",
    scalas = List(scala213Version),
    steps = WorkflowStep.Checkout ::
      WorkflowStep.SetupJava(githubWorkflowJavaVersions.value.toList) ++
      githubWorkflowGeneratedCacheSteps.value ++
      List(
        WorkflowStep.Sbt(List("docs/mdoc")),
        WorkflowStep.Use(
          UseRef.Public("actions", "setup-node", "v3"),
          params = Map("node-version" -> "18")
        ),
        WorkflowStep.Run(List("cd website && yarn install")),
        WorkflowStep.Run(
          List(
            "git config --global user.name ValdemarGr",
            "git config --global user.email randomvald0069@gmail.com",
            "cd website && yarn deploy"
          ),
          env = Map(
            "GIT_USER" -> "valdemargr",
            "GIT_PASS" -> "${{ secrets.GITHUB_TOKEN }}"
          )
        )
      ),
    cond = Some("""github.event_name != 'pull_request' && (startsWith(github.ref, 'refs/tags/v') || github.ref == 'refs/heads/main')""")
  )

lazy val sharedSettings = Seq(
  organization := "io.github.valdemargr",
  organizationName := "Valdemar Grange",
  autoCompilerPlugins := true,
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-effect" % "3.3.14",
    "org.typelevel" %% "cats-collections-core" % "0.9.4",
    "org.typelevel" %% "cats-mtl" % "1.3.0",
    "org.typelevel" %% "cats-core" % "2.9.0",
    "org.typelevel" %% "cats-free" % "2.9.0",
    "co.fs2" %% "fs2-core" % "3.2.14",
    "co.fs2" %% "fs2-io" % "3.2.14",
    "org.typelevel" %% "cats-parse" % "0.3.8",
    "io.circe" %% "circe-core" % "0.14.3",
    "io.circe" %% "circe-generic" % "0.14.3",
    "io.circe" %% "circe-parser" % "0.14.3",
    "org.typelevel" %% "paiges-core" % "0.4.2",
    "org.scalameta" %% "munit" % "1.0.0-M6" % Test,
    "org.typelevel" %% "munit-cats-effect" % "2.0.0-M3" % Test
  )
)

lazy val parser = project
  .in(file("modules/parser"))
  .settings(sharedSettings)
  .settings(name := "gql-parser" /*, tlFatalWarnings := true*/ )

lazy val core = project
  .in(file("modules/core"))
  .settings(sharedSettings)
  .settings(name := "gql-core" /*, tlFatalWarnings := true*/ )
  .dependsOn(parser)

lazy val server = project
  .in(file("modules/server"))
  .settings(sharedSettings)
  .settings(name := "gql-server" /*, tlFatalWarnings := true*/ )
  .dependsOn(core)

lazy val client = project
  .in(file("modules/client"))
  .settings(sharedSettings)
  .settings(name := "gql-client" /*, tlFatalWarnings := true*/ )
  .dependsOn(core)

lazy val clientCodegen = project
  .in(file("modules/client-codegen"))
  .settings(sharedSettings)
  .settings(name := "gql-client-codegen" /*, tlFatalWarnings := true*/ )
  .dependsOn(core)
  .dependsOn(client)

lazy val clientCodegenSbt = project
  .in(file("modules/client-codegen-sbt"))
  /* .enablePlugins(BuildInfoPlugin) */
  .enablePlugins(SbtPlugin)
  .settings(
    sbtPlugin := true,
    scalaVersion := "2.12.17",
    name := "gql-client-codegen-sbt",
  )
  /* .enablePlugins(NoPublishPlugin) */

/* lazy val testProject = project */
/*   .in(file("modules/client-codegen-sbt-test")) */
/*   .settings(sharedSettings) */
/*   .enablePlugins(clientCodegenSbt) */

lazy val http4sClient = project
  .in(file("modules/client-http4s"))
  .dependsOn(core)
  .dependsOn(client)
  .settings(sharedSettings)
  .settings(
    name := "gql-client-http4s",
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-circe" % "1.0.0-M36",
      "org.http4s" %% "http4s-dsl" % "1.0.0-M36",
      "org.http4s" %% "http4s-client" % "1.0.0-M36"
    )
  )

/*
lazy val natchez = project
  .in(file("modules/natchez"))
  .settings(sharedSettings)
  .settings(
    name := "gql-natchez",
    libraryDependencies ++= Seq(
      "org.tpolecat" %% "natchez-core" % "0.1.4",
      "org.tpolecat" %% "natchez-noop" % "0.1.4"
    )
  )
  .dependsOn(core)*/

lazy val graphqlWs = project
  .in(file("modules/graphql-ws"))
  .settings(sharedSettings)
  .settings(name := "gql-graphqlws")
  .dependsOn(core)

lazy val serverGraphqlWs = project
  .in(file("modules/server-graphql-ws"))
  .settings(sharedSettings)
  .settings(name := "gql-server-graphqlws")
  .dependsOn(core)
  .dependsOn(graphqlWs)
  .dependsOn(server)

lazy val serverGoi = project
  .in(file("modules/server-goi"))
  .settings(sharedSettings)
  .settings(
    name := "gql-server--goi",
    libraryDependencies ++= Seq("com.beachape" %% "enumeratum" % "1.7.2")
  )
  .dependsOn(core)
  .dependsOn(server)

lazy val serverHttp4s = project
  .in(file("modules/server-http4s"))
  .dependsOn(core % "compile->compile;test->test")
  .dependsOn(server % "compile->compile;test->test")
  .dependsOn(serverGraphqlWs)
  .settings(sharedSettings)
  .settings(
    name := "gql-server-http4s",
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-server" % "1.0.0-M36",
      "org.http4s" %% "http4s-blaze-server" % "1.0.0-M36",
      "org.http4s" %% "http4s-circe" % "1.0.0-M36",
      "org.http4s" %% "http4s-dsl" % "1.0.0-M36",
      "org.http4s" %% "http4s-client" % "1.0.0-M36" % Test
    )
  )
  .dependsOn(server)

lazy val mdocExt = project
  .in(file("modules/mdoc-ext"))
  .settings(sharedSettings)
  .enablePlugins(NoPublishPlugin)

lazy val docs = project
  .in(file("modules/docs"))
  .settings(
    moduleName := "gql-docs",
    mdocOut := file("website/docs"),
    mdocVariables ++= Map(
      "VERSION" -> tlLatestVersion.value.getOrElse(version.value)
    ),
    libraryDependencies ++= Seq(
      "com.47deg" %% "fetch" % "3.1.0"
    ),
    tlFatalWarnings := false
  )
  .dependsOn(server % "compile->compile;test->test")
  .dependsOn(core % "compile->compile;compile->test")
  .dependsOn(serverHttp4s)
  .dependsOn(serverGraphqlWs)
  /* .dependsOn(goi) */
  .dependsOn(mdocExt)
  .enablePlugins(MdocPlugin, DocusaurusPlugin, NoPublishPlugin)
