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
import scala.collection.immutable._

sealed trait PreparedField[+F[_], A] extends Product with Serializable

final case class NodeId(id: NonEmptyList[Int]) extends AnyVal {
  def alpha(i: Int): NodeId = NodeId(i :: id)
}

object NodeId {
  def apply(i: Int): NodeId = NodeId(NonEmptyList.one(i))
}

final case class StepEffectId(
    nodeId: NodeId,
    edgeId: UniqueEdgeCursor
) {
  def alpha(i: Int): StepEffectId = StepEffectId(nodeId.alpha(i), edgeId)
}

sealed trait PreparedStep[+F[_], -I, +O] extends Product with Serializable {
  def nodeId: NodeId
}
object PreparedStep {
  final case class Lift[F[_], I, O](
      nodeId: NodeId,
      f: I => O
  ) extends AnyRef
      with PreparedStep[F, I, O]
  final case class EmbedEffect[F[_], I](
      sei: StepEffectId
  ) extends AnyRef
      with PreparedStep[F, F[I], I] {
    def nodeId: NodeId = sei.nodeId
  }
  final case class EmbedStream[F[_], I](
      sei: StepEffectId
  ) extends AnyRef
      with PreparedStep[F, fs2.Stream[F, I], I] {
    def nodeId: NodeId = sei.nodeId
  }
  final case class EmbedError[F[_], I](
      nodeId: NodeId
  ) extends AnyRef
      with PreparedStep[F, Ior[String, I], I]
  final case class Compose[F[_], I, A, O](
      nodeId: NodeId,
      left: PreparedStep[F, I, A],
      right: PreparedStep[F, A, O]
  ) extends AnyRef
      with PreparedStep[F, I, O]
  final case class GetMeta[F[_], I](
      nodeId: NodeId,
      meta: Eval[PreparedMeta[F]]
  ) extends AnyRef
      with PreparedStep[Nothing, I, FieldMeta[F]]
  final case class First[F[_], I, O, C](
      nodeId: NodeId,
      step: PreparedStep[F, I, O]
  ) extends AnyRef
      with PreparedStep[F, (I, C), (O, C)]
  final case class Batch[F[_], K, V](
      id: Step.BatchKey[K, V],
      ubi: UniqueBatchInstance[K, V]
  ) extends AnyRef
      with PreparedStep[F, Set[K], Map[K, V]] {
    def nodeId: NodeId = ubi.id
  }
  final case class InlineBatch[F[_], K, V](
      run: Set[K] => F[Map[K, V]],
      sei: StepEffectId
  ) extends PreparedStep[F, Set[K], Map[K, V]] {
    def nodeId: NodeId = sei.nodeId
  }
  final case class Choose[F[_], A, B, C, D](
      nodeId: NodeId,
      fac: PreparedStep[F, A, C],
      fbd: PreparedStep[F, B, D]
  ) extends PreparedStep[F, Either[A, B], Either[C, D]]
}

sealed trait Prepared[+F[_], I]

final case class PreparedCont[+F[_], I, A](
    edges: PreparedStep[F, I, A],
    cont: Prepared[F, A]
)

final case class Selection[F[_], I](
    nodeId: NodeId,
    fields: List[PreparedField[F, I]],
    source: ast.Selectable[F, I]
) extends Prepared[F, I]

final case class PreparedList[F[_], A, C, B](
    id: NodeId,
    of: PreparedCont[F, A, B],
    toSeq: C => Seq[A]
) extends Prepared[F, C]

final case class PreparedOption[F[_], I, O](
    id: NodeId,
    of: PreparedCont[F, I, O]
) extends Prepared[F, Option[I]]

final case class PreparedLeaf[F[_], I](
    nodeId: NodeId,
    name: String,
    encode: I => Json
) extends Prepared[F, I]

final case class PreparedDataField[+F[_], A, B](
    nodeId: NodeId,
    name: String,
    alias: Option[String],
    cont: PreparedCont[F, A, B],
    source: ast.Field[F, A, B],
    parsedArgs: Map[Arg[?], Any]
) extends PreparedField[F, A] {
  lazy val outputName = alias.getOrElse(name)

  def arg[C](a: Arg[C]): Option[C] =
    parsedArgs.get(a).asInstanceOf[Option[C]]
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
    nodeId: NodeId,
    specialization: Specialization[F, I, A],
    selection: List[PreparedDataField[F, A, ?]]
) extends PreparedField[F, I]

final case class PreparedMeta[+F[_]](
    variables: VariableMap[Unit],
    args: Option[QA.Arguments[Unit, AnyValue]],
    pdf: PreparedDataField[F, ?, ?]
)

final case class UniqueBatchInstance[K, V](id: NodeId) extends AnyRef {
  def alpha(i: Int): UniqueBatchInstance[K, V] = UniqueBatchInstance(id.alpha(i))
}

final case class UniqueEdgeCursor(path: NonEmptyChain[String]) {
  def append(name: String): UniqueEdgeCursor = UniqueEdgeCursor(path append name)

  lazy val asString: String = path.mkString_(".")
}

object UniqueEdgeCursor {
  def apply(name: String): UniqueEdgeCursor = UniqueEdgeCursor(NonEmptyChain.one(name))
}
