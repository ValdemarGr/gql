package gql

import munit.CatsEffectSuite
import cats.effect._
import io.circe._
import munit.Clues

class IntrospectionTest extends CatsEffectSuite {
  val schema = StarWarsSchema.schema.unsafeRunSync()

  def query(q: String, variables: Map[String, Json] = Map.empty): IO[JsonObject] =
    Compiler[IO].compile(schema, q, variables = variables) match {
      case Left(err)                   => IO.pure(err.asGraphQL)
      case Right(Application.Query(q)) => q.map(_.asGraphQL)
      case _                           => ???
    }

  test("should be able to fire an introspection query") {
    val q = """
    query {
      __type(name: "Character") {
        kind
        name
        description
        fields {
          name
          args {
            name
            type {
              name
            }
            defaultValue
          }
        }
        interfaces {
          name
        }
        possibleTypes {
          name
        }
        enumValues {
          name
        }
        inputFields {
          name
          type {
            name
          }
          defaultValue
        }
        ofType {
          kind
        }
      }
      __schema {
        types {
          kind
          name
          description
          fields {
            name
            args {
              name
              type {
                name
              }
              defaultValue
            }
          }
          interfaces {
            name
          }
          possibleTypes {
            name
          }
          enumValues {
            name
          }
          inputFields {
            name
            type {
              name
            }
            defaultValue
          }
          ofType {
            kind
          }
        }
      }
    }
    """

    query(q)
  }
}
