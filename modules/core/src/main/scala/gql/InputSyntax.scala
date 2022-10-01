package gql

import gql.ast._

abstract class InputSyntax {
  def obj[A](
      name: String,
      a: NonEmptyArg[A]
  ): In[A] = Input(name, a)
}

object InputSyntax extends InputSyntax
