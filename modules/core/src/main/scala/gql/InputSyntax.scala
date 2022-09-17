package gql

import gql.in._

abstract class InputSyntax {
  def obj[A](
      name: String,
      a: Arg[A]
  ): Input[A] = Obj(name, a)
}

object InputSyntax extends InputSyntax
