package gql

abstract class InputSyntax {
  def obj[A](
      name: String,
      a: Output.Fields.Arg[A]
  ): Input[A] = Input.Obj(name, a)
}
