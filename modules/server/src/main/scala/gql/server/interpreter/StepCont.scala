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
package gql.server.interpreter

import gql.preparation._
import io.circe._
import cats.data._

sealed trait StepCont[F[_], -I, +O]
object StepCont {
  final case class Done[F[_], I](prep: Prepared[F, I]) extends StepCont[F, I, Json]
  final case class Continue[F[_], I, C, O](
      step: PreparedStep[F, I, C],
      next: StepCont[F, C, O]
  ) extends StepCont[F, I, O]
  final case class Join[F[_], I, O](
      submit: Chain[IndexedData[F, I]] => F[Option[Chain[IndexedData[F, I]]]],
      next: StepCont[F, I, O]
  ) extends StepCont[F, I, O]
  final case class TupleWith[F[_], I, C, O](
      m: Map[Int, C],
      next: StepCont[F, (I, C), O]
  ) extends StepCont[F, I, O]

  trait Visitor[F[_]] {
    def visitContinue[I, C, O](step: Continue[F, I, C, O]): StepCont[F, I, O] = step
    def visitTupleWith[I, C, O](m: TupleWith[F, I, C, O]): StepCont[F, I, O] = m
    def visitJoin[I, O](p: Join[F, I, O]): StepCont[F, I, O] = p
  }

  def visit[F[_], I, O](cont: StepCont[F, I, O])(visitor: Visitor[F]): StepCont[F, I, O] =
    cont match {
      case x: Continue[F, i, c, o]  => visitor.visitContinue(x.copy(next = visit(x.next)(visitor)))
      case x: TupleWith[F, i, c, o] => visitor.visitTupleWith(x.copy(next = visit(x.next)(visitor)))
      case x: Join[F, ?, ?]         => visitor.visitJoin(x.copy(next = visit(x.next)(visitor)))
      case _: Done[?, ?]            => cont
    }
}
