package mdoc

import cats.effect._

object IORunPrint {
  def apply[A](ioa: IO[A])(stringRepr: String): Unit = {
    println("```scala")
    println(stringRepr.split("\n").drop(1).mkString("\n"))
    import cats.effect.unsafe.implicits.global
    val a = ioa.unsafeRunSync()
    println(a.toString().split("\n").map(x => s"// $x").mkString("\n"))
    println("```")
  }
}
