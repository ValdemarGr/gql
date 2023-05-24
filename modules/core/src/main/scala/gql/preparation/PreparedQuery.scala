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

sealed trait PreparedField[F[_], A] extends Product with Serializable

sealed trait PreparedStep[F[_], -I, +O] extends Product with Serializable
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
  final case class GetMeta[F[_], I](meta: PreparedMeta) extends AnyRef with PreparedStep[F, I, Meta]
  final case class First[F[_], I, O, C](step: PreparedStep[F, I, O]) extends AnyRef with PreparedStep[F, (I, C), (O, C)]
  final case class Batch[F[_], K, V](id: Step.BatchKey[K, V], globalEdgeId: UniqueBatchInstance[K, V])
      extends AnyRef
      with PreparedStep[F, Set[K], Map[K, V]]
  final case class Choose[F[_], A, B, C, D](
      fac: PreparedStep[F, A, C],
      fbc: PreparedStep[F, B, D]
  ) extends PreparedStep[F, Either[A, B], Either[C, D]]
}

sealed trait Prepared[F[_], I]

final case class PreparedCont[F[_], I, A](
    edges: PreparedStep[F, I, A],
    cont: Prepared[F, A]
)

final case class Selection[F[_], I](fields: NonEmptyList[PreparedField[F, I]]) extends Prepared[F, I]

final case class PreparedList[F[_], A, C, B](of: PreparedCont[F, A, B], toSeq: C => Seq[A]) extends Prepared[F, C]

final case class PreparedOption[F[_], I, O](of: PreparedCont[F, I, O]) extends Prepared[F, Option[I]]

final case class PreparedLeaf[F[_], I](name: String, encode: I => Json) extends Prepared[F, I]

final case class PreparedDataField[F[_], A](
    name: String,
    alias: Option[String],
    cont: PreparedCont[F, A, ?]
) extends PreparedField[F, A] {
  lazy val outputName = alias.getOrElse(name)
}

final case class PreparedSpecification[F[_], I, A](
    typename: String,
    specify: I => Option[A],
    selection: NonEmptyList[PreparedDataField[F, A]]
) extends PreparedField[F, I]

final case class PreparedMeta(
    variables: VariableMap[Unit],
    alias: Option[String],
    args: Option[QA.Arguments[Unit, AnyValue]]
)

final case class UniqueBatchInstance[K, V](id: Int) extends AnyVal

final case class UniqueEdgeCursor(path: NonEmptyChain[String]) {
  def append(name: String): UniqueEdgeCursor = UniqueEdgeCursor(path append name)

  lazy val asString: String = path.mkString_(".")
}

object UniqueEdgeCursor {
  def apply(name: String): UniqueEdgeCursor = UniqueEdgeCursor(NonEmptyChain.one(name))
}
