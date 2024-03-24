/*
 * Copyright 2023 Valdemar Grange
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

import gql.parser.{QueryAst => QA}
import gql.preparation.MergedFieldInfo
import gql.parser.QueryAst

/** A [[Directive]] takes an argument A and performs some context specific ast transformation.
  *
  * The [[Directive]] structure defines executable directives that modify execution
  * https://spec.graphql.org/draft/#ExecutableDirectiveLocation.
  */
final case class Directive[A](
    name: String,
    isRepeatable: Boolean,
    arg: EmptyableArg[A] = EmptyableArg.Empty
) {
  def repeatable = copy(isRepeatable = true)
  def unrepeatable = copy(isRepeatable = false)
}

/** Consider taking a look at the skip and include directives as an example.
  */
object Directive {
  val skipDirective = Directive("skip", false, EmptyableArg.Lift(gql.dsl.input.arg[Boolean]("if")))

  def skipPositions[F[_]]: List[Position[F, ?]] = {
    val field = Position.Field(
      skipDirective,
      new Position.FieldHandler[F, Boolean] {
        override def apply[F2[x] >: F[x], I, C](
            a: Boolean,
            field: ast.Field[F2, I, ?],
            mfa: MergedFieldInfo[F2, C]
        ): Either[String, List[(ast.Field[F2, I, ?], MergedFieldInfo[F2, C])]] =
          if (a) Right(Nil) else Right(List((field, mfa)))
      }
    )
    val fragmentSpread = Position.FragmentSpread[Boolean](
      skipDirective,
      new Position.QueryHandler[QA.FragmentSpread, Boolean] {
        override def apply[C](a: Boolean, query: QueryAst.FragmentSpread[C]): Either[String, List[QueryAst.FragmentSpread[C]]] =
          if (a) Right(Nil) else Right(List(query))
      }
    )
    val inlineFragmentSpread = Position.InlineFragmentSpread[Boolean](
      skipDirective,
      new Position.QueryHandler[QA.InlineFragment, Boolean] {
        override def apply[C](a: Boolean, query: QueryAst.InlineFragment[C]): Either[String, List[QueryAst.InlineFragment[C]]] =
          if (a) Right(Nil) else Right(List(query))
      }
    )

    List(field, fragmentSpread, inlineFragmentSpread)
  }

  val includeDirective = Directive("include", false, EmptyableArg.Lift(gql.dsl.input.arg[Boolean]("if")))

  def includePositions[F[_]]: List[Position[F, ?]] = {
    val field = Position.Field(
      includeDirective,
      new Position.FieldHandler[F, Boolean] {
        override def apply[F2[x] >: F[x], I, C](
            a: Boolean,
            field: ast.Field[F2, I, ?],
            mfa: MergedFieldInfo[F2, C]
        ): Either[String, List[(ast.Field[F2, I, ?], MergedFieldInfo[F2, C])]] =
          if (a) Right(List((field, mfa))) else Right(Nil)
      }
    )
    val fragmentSpread = Position.FragmentSpread[Boolean](
      includeDirective,
      new Position.QueryHandler[QA.FragmentSpread, Boolean] {
        override def apply[C](a: Boolean, query: QueryAst.FragmentSpread[C]): Either[String, List[QueryAst.FragmentSpread[C]]] =
          if (a) Right(List(query)) else Right(Nil)
      }
    )
    val inlineFragmentSpread = Position.InlineFragmentSpread[Boolean](
      includeDirective,
      new Position.QueryHandler[QA.InlineFragment, Boolean] {
        override def apply[C](a: Boolean, query: QueryAst.InlineFragment[C]): Either[String, List[QueryAst.InlineFragment[C]]] =
          if (a) Right(List(query)) else Right(Nil)
      }
    )

    List(field, fragmentSpread, inlineFragmentSpread)
  }

  val deprecatedDirective = Directive(
    "deprecated",
    false,
    EmptyableArg.Lift(
      gql.dsl.input.arg[String]("reason", gql.dsl.input.value.scalar("No longer supported"))
    )
  )

  def deprecated[F[_], Struct[_[_], _]]: Position.Schema[F, String, Struct] =
    Position.Schema[F, String, Struct](
      deprecatedDirective,
      new Position.SchemaHandler[F, String, Struct] {
        def apply[F2[x] >: F[x], B](a: String, struct: Struct[F2,B]): Either[String,Struct[F2,B]] = Right(struct)

      }
    )

  val deprecatedEnum: Position.Schema.Enum[String] = 
    deprecated[fs2.Pure, Position.PureStruct[ast.Enum]#T]
  val deprecatedType: Position.Schema.Type[fs2.Pure, String] = 
    deprecated[fs2.Pure, ast.Type]
  val deprecatedInterface: Position.Schema.Interface[fs2.Pure, String] = 
    deprecated[fs2.Pure, ast.Interface]
  val deprecatedInput: Position.Schema.Input[String] = 
    deprecated[fs2.Pure, Position.PureStruct[ast.Input]#T]
  val deprecatedUnion: Position.Schema.Union[fs2.Pure, String] = 
    deprecated[fs2.Pure, ast.Union]
  val deprecatedScalar = 
    deprecated[fs2.Pure, Position.PureStruct[ast.Scalar]#T]
  val deprecatedField = Position.Field[fs2.Pure, String](
    deprecatedDirective,
    new Position.FieldHandler[fs2.Pure, String] {
      def apply[F2[x] >: fs2.Pure[x], I, C](
          a: String,
          field: ast.Field[F2, I, ?],
          mfa: MergedFieldInfo[F2, C]
      ): Either[String, List[(ast.Field[F2, I, ?], MergedFieldInfo[F2, C])]] = Right(List((field, mfa)))
    }
  )
}

// Add as necessary
sealed trait Position[+F[_], A] {
  def directive: Directive[A]
}
object Position {
  trait FieldHandler[+F[_], A] {
    def apply[F2[x] >: F[x], I, C](
        a: A,
        field: ast.Field[F2, I, ?],
        mfa: MergedFieldInfo[F2, C]
    ): Either[String, List[(ast.Field[F2, I, ?], MergedFieldInfo[F2, C])]]
  }
  final case class Field[+F[_], A](
      directive: Directive[A],
      handler: FieldHandler[F, A]
  ) extends Position[F, A]

  trait QueryHandler[Struct[_], A] {
    def apply[C](a: A, query: Struct[C]): Either[String, List[Struct[C]]]
  }
  final case class FragmentSpread[A](
      directive: Directive[A],
      handler: QueryHandler[QA.FragmentSpread, A]
  ) extends Position[Nothing, A]
  final case class InlineFragmentSpread[A](
      directive: Directive[A],
      handler: QueryHandler[QA.InlineFragment, A]
  ) extends Position[Nothing, A]

  trait SchemaHandler[+F[_], A, Struct[_[_], _]] {
    def apply[F2[x] >: F[x], B](a: A, struct: Struct[F2, B]): Either[String, Struct[F2, B]]
  }

  final case class Schema[+F[_], A, Struct[_[_], _]](
      directive: Directive[A],
      handler: SchemaHandler[F, A, Struct]
  ) extends Position[F, A]

  type PureStruct[Struct[_]] = {
    type T[F[_], B] = Struct[B]
  }

  type PureSchema[A, Struct[_]] = Schema[fs2.Pure, A, PureStruct[Struct]#T]
  object PureSchema {
    def apply[A, Struct[_]](
        directive: Directive[A],
        handler: SchemaHandler[fs2.Pure, A, PureStruct[Struct]#T]
    ): PureSchema[A, Struct] =
      Schema(directive, handler)
  }

  object Schema {
    type Enum[A] = PureSchema[A, ast.Enum]
    type Scalar[A] = PureSchema[A, ast.Scalar]
    type Input[A] = PureSchema[A, ast.Input]
    type Interface[+F[_], A] = Schema[F, A, ast.Interface]
    type Type[+F[_], A] = Schema[F, A, ast.Type]
    type Union[+F[_], A] = Schema[F, A, ast.Union]
  }
}
