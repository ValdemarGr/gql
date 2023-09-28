/*
 * Copyright 2023 Valdemar Grange
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package gql

import munit.CatsEffectSuite
import cats.effect._
import io.circe._
import io.circe.syntax._
import cats.effect.std.Semaphore

class IntrospectionTest extends CatsEffectSuite {
  val schema = StarWarsSchema.schema.unsafeRunSync()

  def query(q: String, variables: Map[String, Json] = Map.empty): IO[JsonObject] =
    Compiler[IO].compile(schema, q, variables = variables) match {
      case Left(err)                   => IO.pure(err.asJsonObject)
      case Right(Application.Query(q)) => q.map(_.asJsonObject)
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

  test("fail") {
    import cats.effect._
    import cats.effect.implicits._
    import cats.implicits._
    val realLog = (a: String) => println(a)
    var log = (_: String) => ()
    def go =
      IO((0 to 100000).toList.map(_ * 2)).timed.map { case (d, _) =>
        println("normal map")
        println(d.toMillis)
      } >>
         IO.defer((0 to 100000).toList.traverse(x => IO(x * 2))).timed.map { case (d, _) =>
          println("normal traverse (suspended effect)")
          println(d.toMillis)
        } >> 
       IO.defer((0 to 100000).toList.traverse(x => IO.pure(x * 2))).timed.map { case (d, _) =>
          println("normal traverse (pure)")
          println(d.toMillis)
        } >> Semaphore[IO](1).flatMap { sem =>
          IO.defer((0 to 100000).toList.traverse(x => sem.permit surround IO(x * 2))).timed.map { case (d, _) =>
            println("sem overhead")
            println(d.toMillis)
          }
        } >> Semaphore[IO](16).flatMap { sem =>
          IO.defer(
            (0 to 100000).toList
              .traverse(x => (sem.permit surround IO(x * 2)).start)
              .flatMap(_.traverse(_.join))
          ).timed
            .map { case (d, _) =>
              println("manual parallel")
              println(d.toMillis)
            }
        } >>
        IO.defer((0 to 100000).toList.parTraverse(x => IO(x * 2))).timed.map { case (d, _) =>
          println("parTraverse")
          println(d.toMillis)
        } >>
        IO.defer((0 to 100000).toList.parTraverseN(16)(x => IO(x * 2))).timed.map { case (d, _) =>
          println("parTraverseN(16)")
          println(d.toMillis)
        } >> IO(fail("oolol i fail"): Unit)

    import scala.concurrent.duration._
    (0 to 40).toList.traverse_(_ => go) >>
      IO { log = realLog } >> IO(System.gc()) >> IO.sleep(1.second) >> go
  }
}
