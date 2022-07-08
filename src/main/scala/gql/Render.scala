package gql

import cats.Eval
import gql.Output.Interface
import gql.Output.Obj
import gql.Output.Union
import gql.Output.Arr
import gql.Output.Scalar
import gql.Output.Opt
import cats.data.NonEmptyList
import cats.implicits._

object Render {
  // def evalRenderFields[F[_]](xs: NonEmptyList[(String, Output.Fields.Field[F, _, _])]): Eval[String] = Eval.defer {
  //   xs
  //     .traverse { case (n, f) => s"$n: ${null}" }
  //     .map(_.mkString_(",\n"))
  // }

  // def evalRender[F[_]](todo: List[Output[F, _]]): Eval[String] = Eval.later {
  //   todo match {
  //     case ot :: xs =>
  //       ot match {
  //         case Output.Enum(name, encoder) => ???
  //         case Interface(name, instances, fields) =>
  //           evalRenderFields(fields).map { s =>
  //             s"""
  //           | interface $name {
  //           |   $s
  //           | }
  //           """
  //           }
  //         // case Obj(name, fields)     =>
  //         // case Union(name, types)    =>
  //         // case Arr(of)               =>
  //         // case Scalar(name, encoder) =>
  //         // case Opt(of)               =>
  //       }
  //     case Nil => Eval.now("")
  //   }
  // }

  // def render[F[_]](schema: Schema[F, _]): String =
  //   schema.query
}
