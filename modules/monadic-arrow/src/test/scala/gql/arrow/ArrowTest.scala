package gql.arrow

import munit.CatsEffectSuite
import cats.effect._
import cats.implicits._
import cats.free.Free
import gql.resolver.Resolver
import scala.util.Try

class ArrowTest extends CatsEffectSuite {
  val d = dsl[IO]
  import d._

  test("arrow compilation should work properly") {
    compile[Int] { i =>
      for {
        a <- i.apply(_.map(x => x * 2))
        b <- i.apply(_.map(x => x + 1))
        c <- (a, b).tupled.apply(_.evalMap { case (x, y) => IO(x + y) })
        _ <- i.apply(_.map(_ * 2))
      } yield c
    }
  }

  test("arrow evaluation should work properly") {
    object FunctionLang extends LanguageDsl[Function1]
    import FunctionLang._
    val f = compile[Int] { i =>
      for {
        a <- i(_.map(_ * 2))
        // var is an applicative
        b = a.map(_ + 1)
        c <- (a, b).tupled.apply(_.map { case (x, y) => x + y })
      } yield c
    }
    // i = 2
    // a = 2 * 2 = 4
    // b = a + 1 = 5
    // c = a + b = 9
    val x = f(2)
    assertEquals(x, 9)
  }

  test("should be able to invoke helper functions that do not compile the closure") {
    def helper(v: Var[Int]): Free[DeclAlg[Resolver[IO, *, *], *], Var[Int]] =
      dsl[IO].VarOps(v).apply(_.map(x => x * 2))

    compile[Int] { i =>
      for {
        a <- i.apply(_.map(x => x * 2))
        b <- i.apply(_.map(x => x + 1))
        c <- (helper(a), helper(b)).flatMapN((_, _).tupled.apply(_.map { case (aa, bb) => aa + bb }))
      } yield c
    }
  }

  test("should be able to nest compilations that do not close over any variables") {
    compile[Int] { i =>
      for {
        a <- i(_.map(_ * 2))
        b <- a(_.map(_ + 1))
        c <- b(_.andThen(compile[Int] { i2 =>
          for {
            d <- i2(_.map(_ * 2))
            e <- d(_.map(_ + 1))
          } yield e
        }))
      } yield c
    }
  }

  test("should fail when referencing variables that are not reachable from the compiled program") {
    val fa = Try {
      compile[Int] { i =>
        for {
          a <- i(_.map(_ * 2))
          b <- a(_.map(_ + 1))
          c <- b(_.andThen(compile[Int] { _ =>
            for {
              d <- i(_.map(_ * 2))
              e <- d(_.map(_ + 1))
            } yield e
          }))
        } yield c
      }
    }
    assert(clue(fa).isFailure)
  }
}
