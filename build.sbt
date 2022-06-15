ThisBuild / scalaVersion := "2.13.6"
ThisBuild / organization := "io.valdemargr"

lazy val sharedSettings = Seq(
  autoCompilerPlugins := true,
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-effect" % "3.3.5",
    "org.typelevel" %% "cats-collections-core" % "0.9.3",
    "org.typelevel" %% "cats-mtl" % "1.2.0",
    "co.fs2" %% "fs2-core" % "3.2.5",
    "co.fs2" %% "fs2-io" % "3.2.5",
    "org.typelevel" %% "munit-cats-effect-3" % "1.0.0",
    "org.typelevel" %% "cats-parse" % "0.3.7",
    "io.circe" %% "circe-core" % "0.14.1",
    "io.circe" %% "circe-generic" % "0.14.1",
    "io.circe" %% "circe-parser" % "0.14.1",
    "io.circe" %% "circe-generic-extras" % "0.14.1",
    "org.typelevel" %% "kittens" % "2.3.0",
    "org.tpolecat" %% "natchez-core" % "0.1.4",
    "org.tpolecat" %% "natchez-noop" % "0.1.4",
    "com.beachape" %% "enumeratum" % "1.7.0",
    "ch.qos.logback" % "logback-classic" % "1.2.11",
    "org.http4s" %% "http4s-core" % "1.0.0-M32",
    "org.http4s" %% "http4s-dsl" % "1.0.0-M32",
    "org.http4s" %% "http4s-circe" % "1.0.0-M32",
    "org.typelevel" %% "log4cats-core" % "2.2.0",
    "org.typelevel" %% "log4cats-slf4j" % "2.2.0",
    "org.http4s" %% "http4s-jdk-http-client" % "1.0.0-M1",
    "org.typelevel" %% "spire" % "0.18.0-M3"
  ),
  addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full),
  addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
)

lazy val root = project
  .in(file("."))
  .settings(sharedSettings: _*)