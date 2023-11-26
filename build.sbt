val scala213Version = "2.13.12"

ThisBuild / scalaVersion := scala213Version
ThisBuild / crossScalaVersions := Seq(scala213Version, "3.3.0")
ThisBuild / organization := "io.github.valdemargr"

ThisBuild / tlBaseVersion := "0.3"
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

// I might re-enable this later
ThisBuild / tlCiMimaBinaryIssueCheck := false
ThisBuild / tlMimaPreviousVersions := Set.empty
ThisBuild / mimaReportSignatureProblems := false
ThisBuild / mimaFailOnProblem := false
ThisBuild / mimaPreviousArtifacts := Set.empty
ThisBuild / tlSonatypeUseLegacyHost := true
//ThisBuild / tlFatalWarnings := false

val dbStep = WorkflowStep.Run(
  commands = List("docker-compose up -d"),
  name = Some("Start services")
)

ThisBuild / githubWorkflowJobSetup += dbStep

ThisBuild / githubWorkflowAddedJobs ++= Seq(
  WorkflowJob(
    id = "compile-docs",
    name = "Verify that the docs compile",
    scalas = List(scala213Version),
    steps = WorkflowStep.Use(
      UseRef.Public("actions", "checkout", "v3"),
      name = Some("Checkout current branch (fast)"),
      params = Map("fetch-depth" -> "0")
    ) :: dbStep ::
      WorkflowStep.SetupJava(githubWorkflowJavaVersions.value.toList) ++
      githubWorkflowGeneratedCacheSteps.value ++
      List(
        WorkflowStep.Sbt(List("docs/mdoc"), name = Some("Compile the documentation scala code")),
        WorkflowStep.Sbt(List("readme/mdoc"), name = Some("Compile the readme scala code"))
      )
  ),
  WorkflowJob(
    id = "docs",
    name = "Run mdoc docs",
    needs = List("compile-docs"),
    scalas = List(scala213Version),
    steps = WorkflowStep.Use(
      UseRef.Public("actions", "checkout", "v3"),
      name = Some("Checkout current branch (fast)"),
      params = Map("fetch-depth" -> "0")
    ) :: dbStep ::
      WorkflowStep.SetupJava(githubWorkflowJavaVersions.value.toList) ++
      githubWorkflowGeneratedCacheSteps.value ++
      // We need all commits to track down the tag for the VERSION variable
      List(
        WorkflowStep.Run(
          List(
            "git fetch --all --tags --force"
          )
        ),
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
)

lazy val sharedSettings = Seq(
  organization := "io.github.valdemargr",
  organizationName := "Valdemar Grange",
  autoCompilerPlugins := true,
  tlCiMimaBinaryIssueCheck := false,
  tlMimaPreviousVersions := Set.empty,
  mimaReportSignatureProblems := false,
  mimaFailOnProblem := false,
  mimaPreviousArtifacts := Set.empty,
  scalacOptions ++= {
    if (scalaVersion.value.startsWith("2")) {
      Seq(
        "-Wunused:-nowarn",
        "-Wconf:cat=unused-nowarn:s",
        "-Ywarn-unused:-nowarn",
        "-Xmigration",
        "-Xsource:3"
      )
    } else Seq.empty // Seq("-explain")
  },
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-effect" % "3.5.2",
    "org.typelevel" %% "cats-mtl" % "1.3.1",
    "org.typelevel" %% "cats-core" % "2.9.0",
    "org.typelevel" %% "cats-free" % "2.9.0",
    "co.fs2" %% "fs2-core" % "3.7.0",
    "co.fs2" %% "fs2-io" % "3.7.0",
    "org.typelevel" %% "cats-parse" % "0.3.8",
    "io.circe" %% "circe-core" % "0.14.6",
    "io.circe" %% "circe-parser" % "0.14.6",
    "org.tpolecat" %% "sourcepos" % "1.1.0",
    "org.typelevel" %% "paiges-core" % "0.4.2",
    "org.scalameta" %% "munit" % "1.0.0-M10" % Test,
    "org.typelevel" %% "munit-cats-effect" % "2.0.0-M3" % Test
  )
)

lazy val parser = project
  .in(file("modules/parser"))
  .settings(sharedSettings)
  .settings(name := "gql-parser")

lazy val core = project
  .in(file("modules/core"))
  .settings(sharedSettings)
  .settings(name := "gql-core")
  .dependsOn(parser)

lazy val server = project
  .in(file("modules/server"))
  .settings(sharedSettings)
  .settings(name := "gql-server")
  .dependsOn(core)

lazy val client = project
  .in(file("modules/client"))
  .settings(sharedSettings)
  .settings(name := "gql-client")
  .dependsOn(core)

lazy val clientCodegen = project
  .in(file("modules/client-codegen"))
  .settings(sharedSettings)
  .settings(name := "gql-client-codegen")
  .dependsOn(core)
  .dependsOn(client)

lazy val clientCodegenCli = project
  .in(file("modules/client-codegen-cli"))
  .settings(sharedSettings)
  .settings(
    name := "gql-client-codegen-cli",
    libraryDependencies ++= Seq(
      "com.monovore" %% "decline" % "2.4.1",
      "com.monovore" %% "decline-effect" % "2.4.1"
    )
  )
  .dependsOn(core)
  .dependsOn(client)
  .dependsOn(clientCodegen)

lazy val clientCodegenSbt = project
  .in(file("modules/client-codegen-sbt"))
  .enablePlugins(BuildInfoPlugin)
  .enablePlugins(SbtPlugin)
  .settings(
    sbtPlugin := true,
    scalaVersion := "2.12.18",
    name := "gql-client-codegen-sbt",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % "0.14.6"
    )
  )
  .aggregate(clientCodegenCli)
/* .enablePlugins(NoPublishPlugin) */

val codeGenForTest = taskKey[Seq[File]]("Generate code for test")
lazy val testCodeGen = project
  .in(file("modules/client-codegen-test"))
  .settings(sharedSettings)
  .dependsOn(clientCodegenCli)
  .dependsOn(client)
  .dependsOn(server % "test->test")
  .dependsOn(http4sClient % "test->test")
  .dependsOn(serverHttp4s % "test->test")
  .settings(
    tlFatalWarnings := false,
    codeGenForTest := {
      Def.taskDyn {
        val sp = file("./modules/client-codegen-test/src/main/resources/schema.graphql").absolutePath.replace("\\", "\\\\")
        val qp = file("./modules/client-codegen-test/src/main/resources/queries/query.graphql").absolutePath.replace("\\", "\\\\")
        val f = (Compile / sourceManaged).value / "gql"
        IO.createDirectory(f)
        val s0 = f / "shared.scala"
        val s = s0.absolutePath.replace("\\", "\\\\")
        val outf = f / "query.scala"
        val outs = outf.absolutePath.replace("\\", "\\\\")
        val input =
          s""" --validate --input {"schema":"${sp}","shared":"${s}","queries":[{"query":"${qp}","output":"${outs}"}],"packageName":"gql.client.gen.test"}"""
        val ip2: String = input.toString()
        (clientCodegenCli / Compile / run).toTask(ip2).map(_ => Seq(outf, s0))
      }.value
    },
    // Testing consists of doing a code-gen run and then compiling the generated code
    Compile / sourceGenerators += codeGenForTest,
    Test / test := {
      val _ = (Compile / compile).value
      (Test / test).value
    }
  )
  .enablePlugins(NoPublishPlugin)

lazy val http4sClient = project
  .in(file("modules/client-http4s"))
  .dependsOn(core)
  .dependsOn(client)
  .settings(sharedSettings)
  .settings(
    name := "gql-client-http4s",
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-circe" % "1.0.0-M39",
      "org.http4s" %% "http4s-dsl" % "1.0.0-M39",
      "org.http4s" %% "http4s-client" % "1.0.0-M39"
    )
  )

lazy val serverNatchez = project
  .in(file("modules/server-natchez"))
  .settings(sharedSettings)
  .settings(
    name := "gql-natchez",
    libraryDependencies ++= Seq(
      "org.tpolecat" %% "natchez-core" % "0.3.4"
    )
  )
  .dependsOn(core)
  .dependsOn(server)

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
    name := "gql-server-goi",
    libraryDependencies ++= Seq(
      "com.beachape" %% "enumeratum" % "1.7.3",
      "org.typelevel" %% "twiddles-core" % "0.6.1"
    )
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
      "org.http4s" %% "http4s-server" % "1.0.0-M39",
      "org.http4s" %% "http4s-circe" % "1.0.0-M39",
      "org.http4s" %% "http4s-dsl" % "1.0.0-M39",
      "org.http4s" %% "http4s-client" % "1.0.0-M39" % Test
    )
  )
  .dependsOn(server)

lazy val relational = project
  .in(file("modules/relational"))
  .settings(sharedSettings)
  .dependsOn(server)
  .settings(
    name := "gql-relational"
  )

lazy val relationalSkunk = project
  .in(file("modules/relational-skunk"))
  .settings(sharedSettings)
  .dependsOn(server)
  .dependsOn(relational % "compile->compile;compile->test")
  .settings(
    name := "gql-relational-skunk",
    libraryDependencies ++= Seq(
      "org.tpolecat" %% "skunk-core" % "0.6.2",
      "org.tpolecat" %% "natchez-noop" % "0.3.4" % Test
    )
  )

lazy val relationalDoobie = project
  .in(file("modules/relational-doobie"))
  .settings(sharedSettings)
  .dependsOn(server)
  .dependsOn(relational % "compile->compile;compile->test")
  .settings(
    name := "gql-relational-doobie",
    libraryDependencies ++= Seq(
      "org.tpolecat" %% "doobie-core" % "1.0.0-RC4",
      "org.tpolecat" %% "doobie-postgres" % "1.0.0-RC4" % Test
    )
  )

lazy val mdocExt = project
  .in(file("modules/mdoc-ext"))
  .settings(sharedSettings)
  .enablePlugins(NoPublishPlugin)

lazy val readme = project
  .in(file("modules/readme"))
  .settings(
    moduleName := "gql-readme",
    mdocIn := file("readme/README.md"),
    mdocOut := file("./README.md"),
    mdocVariables ++= Map(
      "VERSION" -> tlLatestVersion.value.getOrElse(version.value)
    ),
    tlFatalWarnings := false
  )
  .dependsOn(server % "compile->compile;test->test;compile->test")
  .dependsOn(core % "compile->compile;compile->test")
  .dependsOn(serverHttp4s)
  .dependsOn(serverGraphqlWs)
  .dependsOn(serverNatchez)
  .dependsOn(clientCodegen)
  .dependsOn(clientCodegenCli)
  .dependsOn(serverGoi)
  .dependsOn(client)
  .dependsOn(http4sClient)
  .dependsOn(mdocExt)
  .enablePlugins(MdocPlugin, NoPublishPlugin)

lazy val docs = project
  .in(file("modules/docs"))
  .settings(
    moduleName := "gql-docs",
    mdocOut := file("website/docs"),
    mdocVariables ++= Map(
      "VERSION" -> tlLatestVersion.value.getOrElse(version.value)
    ),
    libraryDependencies ++= Seq(
      "com.47deg" %% "fetch" % "3.1.2",
      "org.tpolecat" %% "natchez-noop" % "0.3.4"
    ),
    tlFatalWarnings := false
  )
  .dependsOn(server % "compile->compile;test->test;compile->test")
  .dependsOn(core % "compile->compile;compile->test")
  .dependsOn(serverHttp4s)
  .dependsOn(serverGraphqlWs)
  .dependsOn(relational)
  .dependsOn(relationalSkunk)
  .dependsOn(relationalDoobie)
  .dependsOn(serverNatchez)
  .dependsOn(clientCodegen)
  .dependsOn(clientCodegenCli)
  .dependsOn(serverGoi)
  .dependsOn(client)
  .dependsOn(http4sClient)
  .dependsOn(mdocExt)
  .enablePlugins(MdocPlugin, DocusaurusPlugin, NoPublishPlugin)
