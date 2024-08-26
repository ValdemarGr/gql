/*
 * Copyright 2024 Valdemar Grange
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

import fs2.Stream
import cats.implicits._
import munit.CatsEffectSuite
import gql._
import gql.ast._
import gql.dsl.all._
import cats.effect._
import scala.concurrent.duration._
import fs2.concurrent.SignallingRef

class MemoryTest extends CatsEffectSuite {
  def parListenSignal[A]: fs2.Pipe[IO, fs2.Stream[IO, A], A] =
    streams =>
      Stream.eval(SignallingRef.of[IO, Option[A]](None)).flatMap { sig =>
        sig.discrete.unNone.concurrently {
          streams.parEvalMapUnorderedUnbounded { x =>
            x.evalMap(x => sig.set(Some(x))).compile.drain
          }
        }
      }

  case object Data
  implicit lazy val data: Type[IO, Data.type] = builder[IO, Data.type] { b =>
    b.tpe(
      "Data",
      "value" -> b(_.streamMap(_ => (fs2.Stream(1) ++ fs2.Stream(2)))),
      "strings" -> eff(_ => IO(List.fill[Int](10)(1).scanLeft[String]("") { case (z, x) => z + x.toString() })),
      "rec" -> eff(_ => IO.pure(Data))
    )
  }

  lazy val schemaShape = SchemaShape.unit[IO](
    fields("ping" -> lift(_ => "pong")),
    subscription = Some(
      fields(
        "data" -> build[IO, Unit](
          _.streamMap(_ => Stream.repeatEval(IO((0 to 500).toList.as(Data))).meteredStartImmediately(1.seconds))
        )
      )
    )
  )

  lazy val schema = Schema.simple(schemaShape).unsafeRunSync()

  override def munitIOTimeout: Duration = 10.minutes

  test("run for a while") {
    Compiler[IO]
      .compile(
        schema,
        """
        subscription {
          data {
            value
            strings
            rec {
              value
              strings
            }
          }
        }
      """
      )
      .toOption
      .get match {
      case Application.Subscription(run) =>
        fs2.Stream
          .repeatEval {
            IO.blocking {
              Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
            }
          }
          .metered(1.second)
          .evalMap(n => IO.blocking(System.gc()) *> IO.println(s"Memory: ${n / 1024 / 1024} MB"))
          .concurrently(
            run.interleaveAll(fs2.Stream.eval(IO.blocking(Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()).flatMap{ n =>
              if (n / 1024 / 1024 > 500) IO.sleep(10.seconds)
              else IO.unit
            }))
          )
          .compile.drain
      case _ => ???
    }
  }
}
