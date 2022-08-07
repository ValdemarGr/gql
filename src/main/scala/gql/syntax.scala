package gql

import cats.data._
import cats._
import shapeless.Lazy
import scala.reflect.ClassTag

object syntax extends syntax

trait syntax {
  object out extends OutputSyntax
  object in extends InputSyntax
}
