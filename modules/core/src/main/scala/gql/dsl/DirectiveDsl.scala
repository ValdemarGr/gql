/*
 * Copyright 2024 Valdemar Grange
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
package gql.dsl

import cats.data._
import gql.parser.{QueryAst => QA}
import gql._

trait DirectiveDsl[F[_]] {
  def directive(name: String): Directive[Unit] =
    Directive(name)

  def directive[A](name: String, arg: Arg[A]): Directive[A] =
    Directive(name, EmptyableArg.Lift(arg))

  def onField[A](directive: Directive[A], handler: Position.FieldHandler[F, A]): State[SchemaState[F], Position.Field[F, A]] =
    DirectiveDsl.onField(directive, handler)

  def onFragmentSpread[A](
      directive: Directive[A],
      handler: Position.QueryHandler[QA.FragmentSpread, A]
  ): State[SchemaState[F], Position.FragmentSpread[A]] =
    DirectiveDsl.onFragmentSpread(directive, handler)

  def onInlineFragmentSpread[A](
      directive: Directive[A],
      handler: Position.QueryHandler[QA.InlineFragment, A]
  ): State[SchemaState[F], Position.InlineFragmentSpread[A]] =
    DirectiveDsl.onInlineFragmentSpread(directive, handler)
}

trait DirectiveDslFull {
  protected def addPosition[F[_], A, Pos <: Position[F, A]](pos: Pos): State[SchemaState[F], Pos] =
    State(s => (s.copy(positions = pos :: s.positions), pos))

  def directive(name: String): Directive[Unit] =
    Directive(name)

  def directive[A](name: String, arg: Arg[A]): Directive[A] =
    Directive(name, EmptyableArg.Lift(arg))

  def onField[F[_], A](directive: Directive[A], handler: Position.FieldHandler[F, A]): State[SchemaState[F], Position.Field[F, A]] =
    addPosition[F, A, Position.Field[F, A]](Position.Field(directive, handler))

  def onFragmentSpread[F[_], A](
      directive: Directive[A],
      handler: Position.QueryHandler[QA.FragmentSpread, A]
  ): State[SchemaState[F], Position.FragmentSpread[A]] =
    addPosition[F, A, Position.FragmentSpread[A]](Position.FragmentSpread(directive, handler))

  def onInlineFragmentSpread[F[_], A](
      directive: Directive[A],
      handler: Position.QueryHandler[QA.InlineFragment, A]
  ): State[SchemaState[F], Position.InlineFragmentSpread[A]] =
    addPosition[F, A, Position.InlineFragmentSpread[A]](Position.InlineFragmentSpread(directive, handler))
}

object DirectiveDsl extends DirectiveDslFull {
  def apply[F[_]]: DirectiveDsl[F] = new DirectiveDsl[F] {}
}
