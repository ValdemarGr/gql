ThisBuild / scalaVersion := "2.13.8"
ThisBuild / organization := "io.valdemargr"

lazy val sharedSettings = Seq(
  autoCompilerPlugins := true,
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-effect" % "3.3.14",
    "org.typelevel" %% "cats-collections-core" % "0.9.4",
    "org.typelevel" %% "cats-mtl" % "1.3.0",
    "co.fs2" %% "fs2-core" % "3.2.14",
    "co.fs2" %% "fs2-io" % "3.2.14",
    "org.typelevel" %% "munit-cats-effect" % "2.0.0-M2",
    "org.typelevel" %% "cats-parse" % "0.3.8",
    "io.circe" %% "circe-core" % "0.14.1",
    "io.circe" %% "circe-generic" % "0.14.1",
    "io.circe" %% "circe-parser" % "0.14.1",
    "io.circe" %% "circe-generic-extras" % "0.14.1",
    "org.typelevel" %% "kittens" % "2.3.0",
    "org.tpolecat" %% "natchez-core" % "0.1.4",
    "org.tpolecat" %% "natchez-noop" % "0.1.4",
    "org.tpolecat" %% "typename" % "1.0.0",
    "org.tpolecat" %% "sourcepos" % "1.0.1",
    /* "ch.qos.logback" % "logback-classic" % "1.2.11", */
    "org.typelevel" %% "log4cats-core" % "2.4.0",
    "org.typelevel" %% "log4cats-slf4j" % "2.4.0",
    "org.sangria-graphql" %% "sangria" % "3.2.0",
  ),
  addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full),
  addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
)

lazy val core = project
  .in(file("modules/core"))
  .settings(sharedSettings)

lazy val http4s = project
  .in(file("modules/http4s"))
  .dependsOn(core)
  .settings(sharedSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-server" % "1.0.0-M36",
      "org.http4s" %% "http4s-blaze-server" % "1.0.0-M36",
      "org.http4s" %% "http4s-circe" % "1.0.0-M36",
      "org.http4s" %% "http4s-dsl" % "1.0.0-M36",
    ),
  )
