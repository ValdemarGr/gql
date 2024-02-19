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
  object FunctionLang extends Language[Function1]

  test("arrow compilation should work properly") {
    proc[Int] { i =>
      for {
        a <- i.apply(_.map(x => x * 2))
        b <- i.apply(_.map(x => x + 1))
        c <- (a, b).tupled.apply(_.evalMap { case (x, y) => IO(x + y) })
        _ <- i.apply(_.map(_ * 2))
      } yield c
    }
  }

  test("arrow evaluation should work properly") {
    import FunctionLang._
    val f = proc[Int] { i =>
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

    proc[Int] { i =>
      for {
        a <- i.apply(_.map(x => x * 2))
        b <- i.apply(_.map(x => x + 1))
        c <- (helper(a), helper(b)).flatMapN((_, _).tupled.apply(_.map { case (aa, bb) => aa + bb }))
      } yield c
    }
  }

  test("should be able to nest compilations that do not close over any variables") {
    proc[Int] { i =>
      for {
        a <- i(_.map(_ * 2))
        b <- a(_.map(_ + 1))
        c <- b(_.andThen(proc[Int] { i2 =>
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
      proc[Int] { i =>
        for {
          a <- i(_.map(_ * 2))
          b <- a(_.map(_ + 1))
          c <- b(_.andThen(proc[Int] { _ =>
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

  test("choice should work") {
    import FunctionLang._
    val f = proc[Int] { i =>
      for {
        x <- i(_.map(_ * 2))
        y <- x
          .map(x => if (x > 5) Left(x) else Right(x))
          .choice(
            l => l.apply(_.map(_ + 1)),
            r =>
              for {
                x0 <- r.apply(_.map(_ - 1))
                o <- (x0, x).tupled.apply(_.map { case (z0, z1) => z0 + z1 })
              } yield o
          )
      } yield y
    }

    assertEquals(f(2), 3 + 4)
    assertEquals(f(3), 7)
  }

  test("arrow compilation syntax") {
    val r: Resolver[IO, Int, Int] =
      Resolver.lift[IO, Int](_.toString()).proc { str =>
        for {
          x <- str(_.map(_.toInt))
          y <- (x, x).tupled.apply(_.map { case (a, b) => a + b })
        } yield y
      }
    val _ = r

    val r2 = Resolver.lift[fs2.Pure, Int](_.toString()).proc { str =>
      str(_.map(identity))
    }
    val _ = r2
  }
}
