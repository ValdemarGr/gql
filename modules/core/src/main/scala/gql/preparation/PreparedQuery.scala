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
package gql.preparation

import cats.data._
import cats.implicits._
import gql.resolver._
import io.circe._
import gql.parser.{QueryAst => QA}
import gql.parser.AnyValue
import gql.Arg
import cats.Eval
import gql.ast

sealed trait PreparedField[+F[_], A] extends Product with Serializable

sealed trait PreparedStep[+F[_], -I, +O] extends Product with Serializable
object PreparedStep {
  final case class Lift[F[_], I, O](f: I => O) extends AnyRef with PreparedStep[F, I, O]
  final case class EmbedEffect[F[_], I](stableUniqueEdgeName: UniqueEdgeCursor) extends AnyRef with PreparedStep[F, F[I], I]
  final case class EmbedStream[F[_], I](signal: Boolean, stableUniqueEdgeName: UniqueEdgeCursor)
      extends AnyRef
      with PreparedStep[F, fs2.Stream[F, I], I]
  final case class EmbedError[F[_], I]() extends AnyRef with PreparedStep[F, Ior[String, I], I]
  final case class Compose[F[_], I, A, O](left: PreparedStep[F, I, A], right: PreparedStep[F, A, O])
      extends AnyRef
      with PreparedStep[F, I, O]
  final case class GetMeta[F[_], I](meta: Eval[PreparedMeta[F]]) extends AnyRef with PreparedStep[Nothing, I, FieldMeta[F]]
  final case class First[F[_], I, O, C](step: PreparedStep[F, I, O]) extends AnyRef with PreparedStep[F, (I, C), (O, C)]
  final case class Batch[F[_], K, V](id: Step.BatchKey[K, V], globalEdgeId: UniqueBatchInstance[K, V])
      extends AnyRef
      with PreparedStep[F, Set[K], Map[K, V]]
  final case class InlineBatch[F[_], K, V](run: Set[K] => F[Map[K, V]], stableUniqueEdgeName: UniqueEdgeCursor)
      extends AnyRef
      with PreparedStep[F, Set[K], Map[K, V]]
  final case class Choose[F[_], A, B, C, D](
      fac: PreparedStep[F, A, C],
      fbc: PreparedStep[F, B, D]
  ) extends PreparedStep[F, Either[A, B], Either[C, D]]
}

sealed trait Prepared[+F[_], I]

final case class PreparedCont[+F[_], I, A](
    edges: PreparedStep[F, I, A],
    cont: Prepared[F, A]
)

final case class Selection[F[_], I](
    fields: List[PreparedField[F, I]],
    source: ast.Selectable[F, I]
) extends Prepared[F, I]

final case class PreparedList[F[_], A, C, B](of: PreparedCont[F, A, B], toSeq: C => Seq[A]) extends Prepared[F, C]

final case class PreparedOption[F[_], I, O](of: PreparedCont[F, I, O]) extends Prepared[F, Option[I]]

final case class PreparedLeaf[F[_], I](name: String, encode: I => Json) extends Prepared[F, I]

final case class PreparedDataField[+F[_], A, B](
    name: String,
    alias: Option[String],
    cont: PreparedCont[F, A, B],
    source: ast.Field[F, A, B],
    parsedArgs: Map[Arg[?], Any]
) extends PreparedField[F, A] {
  lazy val outputName = alias.getOrElse(name)

  def arg[A](a: Arg[A]): Option[A] =
    parsedArgs.get(a).asInstanceOf[Option[A]]
}

sealed trait Specialization[F[_], A, B] {
  def specify(a: A): Ior[String, Option[B]]
  def source: ast.Selectable[F, A]
  def target: ast.Type[F, B]
  def typename = target.name
}
object Specialization {
  final case class Type[F[_], A](
      source: ast.Type[F, A]
  ) extends Specialization[F, A, A] {
    def target: ast.Type[F, A] = source
    def specify(a: A): Ior[String, Option[A]] = a.rightIor.map(_.some)
  }
  final case class Union[F[_], A, B](
      source: ast.Union[F, A],
      variant: ast.Variant[F, A, B]
  ) extends Specialization[F, A, B] {
    def target = variant.tpe.value
    def specify(a: A): Ior[String, Option[B]] = variant.specify(a)
  }
  final case class Interface[F[_], A, B](
      target: ast.Type[F, B],
      impl: ast.Implementation[F, B, A]
  ) extends Specialization[F, A, B] {
    def source = impl.implementation.value
    def specify(a: A): Ior[String, Option[B]] = impl.specify(a)
  }
}

final case class PreparedSpecification[F[_], I, A](
    specialization: Specialization[F, I, A],
    selection: List[PreparedDataField[F, A, ?]]
) extends PreparedField[F, I]

final case class PreparedMeta[+F[_]](
    variables: VariableMap[Unit],
    args: Option[QA.Arguments[Unit, AnyValue]],
    pdf: PreparedDataField[F, ?, ?]
)

final case class UniqueBatchInstance[K, V](id: Int) extends AnyVal

final case class UniqueEdgeCursor(path: NonEmptyChain[String]) {
  def append(name: String): UniqueEdgeCursor = UniqueEdgeCursor(path append name)

  lazy val asString: String = path.mkString_(".")
}

object UniqueEdgeCursor {
  def apply(name: String): UniqueEdgeCursor = UniqueEdgeCursor(NonEmptyChain.one(name))
}
