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
  */
final case class Directive[A](
    name: String,
    arg: DirectiveArg[A] = DirectiveArg.Empty
)

/** Consider taking a look at the skip and include directives as an example.
  */
object Directive {
  val skipDirective = Directive("skip", DirectiveArg.WithArg(gql.dsl.arg[Boolean]("if")))

  def skipPositions[F[_]]: List[Position[F, ?]] = {
    val field = Position.Field(
      skipDirective,
      new Position.FieldHandler[F, Boolean] {
        override def apply[I, C](
            a: Boolean,
            field: ast.Field[F, I, ?],
            mfa: MergedFieldInfo[F, C]
        ): Either[String, List[(ast.Field[F, I, ?], MergedFieldInfo[F, C])]] =
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

  val includeDirective = Directive("include", DirectiveArg.WithArg(gql.dsl.arg[Boolean]("if")))

  def includePositions[F[_]]: List[Position[F, ?]] = {
    val field = Position.Field(
      includeDirective,
      new Position.FieldHandler[F, Boolean] {
        override def apply[I, C](
            a: Boolean,
            field: ast.Field[F, I, ?],
            mfa: MergedFieldInfo[F, C]
        ): Either[String, List[(ast.Field[F, I, ?], MergedFieldInfo[F, C])]] =
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
}

// Ad-hoc Applicative (pure, empty structure) from Apply
sealed trait DirectiveArg[A]
object DirectiveArg {
  final case class WithArg[A](arg: Arg[A]) extends DirectiveArg[A]
  case object Empty extends DirectiveArg[Unit]
}

// Add as necessary
sealed trait Position[+F[_], A] {
  def directive: Directive[A]
}
object Position {
  trait FieldHandler[F[_], A] {
    def apply[I, C](
        a: A,
        field: ast.Field[F, I, ?],
        mfa: MergedFieldInfo[F, C]
    ): Either[String, List[(ast.Field[F, I, ?], MergedFieldInfo[F, C])]]
  }
  final case class Field[F[_], A](
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
}
