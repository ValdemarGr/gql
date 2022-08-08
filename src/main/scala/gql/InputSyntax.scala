package gql

abstract class InputSyntax {
  def obj[A](
      name: String,
      a: Arg[A]
  ): Input[A] = Input.Obj(name, a)
}
