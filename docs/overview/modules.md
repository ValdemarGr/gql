---
title: Modules
---

Gql is published as multiple modules, so you can include what you need.

The available modules are:
```scala
// core
libraryDependencies += "io.github.valdemargr" %% "gql-parser" % "@VERSION@",
libraryDependencies += "io.github.valdemargr" %% "gql-core" % "@VERSION@",

// server
libraryDependencies += "io.github.valdemargr" %% "gql-server" % "@VERSION@",
libraryDependencies += "io.github.valdemargr" %% "gql-server-http4s" % "@VERSION@",
libraryDependencies += "io.github.valdemargr" %% "gql-natchez" % "@VERSION@",
libraryDependencies += "io.github.valdemargr" %% "gql-server-graphqlws" % "@VERSION@",
libraryDependencies += "io.github.valdemargr" %% "gql-server-goi" % "@VERSION@",
libraryDependencies += "io.github.valdemargr" %% "gql-relational" % "@VERSION@",
libraryDependencies += "io.github.valdemargr" %% "gql-relational-skunk" % "@VERSION@",
libraryDependencies += "io.github.valdemargr" %% "gql-relational-doobie" % "@VERSION@",

// client
libraryDependencies += "io.github.valdemargr" %% "gql-client" % "@VERSION@",
libraryDependencies += "io.github.valdemargr" %% "gql-client-http4s" % "@VERSION@",

// shared
libraryDependencies += "io.github.valdemargr" %% "gql-graphqlws" % "@VERSION@",

// project/plugins.sbt
addSbtPlugin("io.github.valdemargr" % "gql-client-codegen-sbt" % "@VERSION@")
// and in build.sbt
myBuild
    .enablePlugins(GqlCodeGenPlugin)
```
