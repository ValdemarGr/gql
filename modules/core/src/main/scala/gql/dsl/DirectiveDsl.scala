package gql.dsl

import gql.ast._
import cats.data._
import cats._
import scala.reflect.ClassTag
import gql.parser.{Value => V, Const, QueryAst => QA}
import gql._

trait DirectiveDsl[F[_]] {
  def directive(name: String): Directive[Unit] =
    Directive(name)

  def directive[A](name: String, arg: Arg[A]): Directive[A] =
    Directive(name, DirectiveArg.WithArg(arg))

  def onField[A](directive: Directive[A], handler: Position.FieldHandler[F, A]): State[SchemaState[F], Position.Field[F, A]] =
    DirectiveDsl.onField(directive, handler)

  def onFragmentSpread[F[_], A](
      directive: Directive[A],
      handler: Position.QueryHandler[QA.FragmentSpread, A]
  ): State[SchemaState[F], Position.FragmentSpread[A]] =
    DirectiveDsl.onFragmentSpread(directive, handler)

  def onInlineFragmentSpread[F[_], A](
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
    Directive(name, DirectiveArg.WithArg(arg))

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
