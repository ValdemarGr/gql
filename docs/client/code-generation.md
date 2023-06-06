---
title: Code generation
---
Writing queries in scala using the dsl is more concise and type-safe than writing out the types and codecs by hand, but still requires a lot of code for non-trivial queries.

gql also features a code generator that transforms a graphql schema file and a set of queries (or fragments) into dsl code.

## Setting up
The code generator comes as a stand-alone cli at the maven coordinates:
```scala
// build.sbt
"io.github.valdemargr" %% "gql-client-codegen-cli" % "@VERSION@"
```

The code generator can also be integrated into sbt for a smoother development experience:
```scala
// project/plugins.sbt
addSbtPlugin("io.github.valdemargr" % "gql-client-codegen-sbt" % "@VERSION@")
```

### Sbt integration
By default the sbt integration will look for a schema file in the resources directory at `.../resources/schema.graphql` and queries in the resources directory at `.../resources/queries`.

You can, however, override or add more sources at custom locations:
```scala
lazy val myBuild = 
    ...
        .settings(
            resourceGroups += Gql.resourceGroup(
                name="other_resources",
                schemaFile= file("path/to/schema.graphql"),
                file("path/to/query1.graphql"),
                file("path/to/query2.graphql")
            )
        )
```

## Usage
When the code-generator is invoked it will use the queries and fragments in combination with the schema to generate a set of scala files containing the equivalent query in scala code.

For this demonstration, the code generator will be invoked manually:
```scala mdoc
import gql.client.codegen.{ GeneratorCli => Gen }
import fs2.io.file.Files
import cats.effect._
import cats.implicits._
import cats.effect.unsafe.implicits.global

def runQuery(queryDef: String) =
    Files[IO].tempDirectory.use{ tmp => 
        val schemaFile = tmp / "schema.graphql"
        val queryFile = tmp / "query.graphql"
        val sharedOutFile = tmp / "shared.scala"
        val queryOutFile = tmp / "query.scala"

        val schemaDef = """
            enum HelloEnum {
                HELLO,
                WORLD
            }

            type A {
                a: String
            }

            type B {
                b: String
            }

            union HelloUnion = A | B

            type Query {
                helloEnum(name: String): HelloEnum,
                helloUnion(name2: String): HelloUnion
            }
        """

        val writeSchemaF = fs2.Stream(schemaDef)
            .through(fs2.text.utf8.encode)
            .through(Files[IO].writeAll(schemaFile))
            .compile
            .drain

        val writeQueryF = fs2.Stream(queryDef)
            .through(fs2.text.utf8.encode)
            .through(Files[IO].writeAll(queryFile))
            .compile
            .drain

        import io.circe._
        import io.circe.syntax._
        val jo = Json.obj(
            "schema" -> Json.fromString(schemaFile.toString),
            "shared" -> Json.fromString(sharedOutFile.toString),
            "queries" -> Json.arr(
                Json.obj(
                    "query" -> Json.fromString(queryFile.toString),
                    "output" -> Json.fromString(queryOutFile.toString)
                )
            )
        )

        writeSchemaF >>
            writeQueryF >>
            Gen.run(List("--validate", "--input",jo.spaces2)) >>
            Files[IO].readAll(queryOutFile)
                .through(fs2.text.utf8.decode)
                .compile
                .string
                .map(println)
    }.unsafeRunSync()

runQuery(
    """
        fragment HelloFragment on Query {
            helloEnum(name: $name)
        }

        query HelloQuery($name: String) {
            ...HelloFragment
            helloUnion(name2: "hey") {
                ... on A {
                    a
                }
                ... on B {
                    b
                }
            }
        }
    """
)
```

When supplying the `--validate` flag, gql will generate a stub implementation of the schema and run the same code as if running a gql server.

Lets construct a helper to show this:
```scala mdoc
import scala.util.{Try,Failure}
// We will also remove the ansii color codes from the output, since they don't render well in the docs
def runFail(q: String) = 
    Try {
        runQuery(q)
    } match {
        case Failure(ex) => println(ex.getMessage().replaceAll("\u001B\\[[;\\d]*m", ""))
    }
```

Now with a parsing error:
```scala mdoc
runFail(
    """
        query MyQuery {
            test.,test
        }
    """
)
```

And also with a query validation error:
```scala mdoc
runFail(
    """
        query MyQuery {
            helloEnum(name: 1)
        }
    """
)
```