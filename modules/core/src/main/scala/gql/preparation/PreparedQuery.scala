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

sealed trait Stage
object Stage {
  sealed trait Compilation[+C] extends Stage
  sealed trait Execution extends Compilation[Nothing]
}

sealed trait PreparedField[+F[_], A, +S <: Stage] extends Product with Serializable

final case class NodeId(
    id: Alg.UniqueId,
    sub: Option[Int]
) {
  def alpha(i: Int): NodeId = NodeId(id, Some(i))
}

final case class StepEffectId(
    nodeId: NodeId,
    edgeId: UniqueEdgeCursor
) {
  def alpha(i: Int): StepEffectId = StepEffectId(nodeId.alpha(i), edgeId)
}

sealed trait PreparedStep[+F[_], -I, +O, +S <: Stage] extends Product with Serializable {
  def nodeId: NodeId
}
object PreparedStep {
  final case class Lift[F[_], I, O](
      nodeId: NodeId,
      f: I => O
  ) extends AnyRef
      with PreparedStep[F, I, O, Stage.Execution]
  final case class EmbedEffect[F[_], I](
      sei: StepEffectId
  ) extends AnyRef
      with PreparedStep[F, F[I], I, Stage.Execution] {
    def nodeId: NodeId = sei.nodeId
  }
  final case class EmbedStream[F[_], I](
      sei: StepEffectId
  ) extends AnyRef
      with PreparedStep[F, fs2.Stream[F, I], I, Stage.Execution] {
    def nodeId: NodeId = sei.nodeId
  }
  final case class EmbedError[F[_], I](
      nodeId: NodeId
  ) extends AnyRef
      with PreparedStep[F, Ior[String, I], I, Stage.Execution]
  final case class Compose[F[_], I, A, O, +S <: Stage](
      nodeId: NodeId,
      left: PreparedStep[F, I, A, S],
      right: PreparedStep[F, A, O, S]
  ) extends AnyRef
      with PreparedStep[F, I, O, S]
  final case class First[F[_], I, O, C, +S <: Stage](
      nodeId: NodeId,
      step: PreparedStep[F, I, O, S]
  ) extends AnyRef
      with PreparedStep[F, (I, C), (O, C), S]
  final case class Batch[F[_], K, V](
      id: Step.BatchKey[K, V],
      ubi: UniqueBatchInstance[K, V]
  ) extends AnyRef
      with PreparedStep[F, Set[K], Map[K, V], Stage.Execution] {
    def nodeId: NodeId = ubi.id
  }
  final case class InlineBatch[F[_], K, V](
      run: Set[K] => F[Map[K, V]],
      sei: StepEffectId
  ) extends PreparedStep[F, Set[K], Map[K, V], Stage.Execution] {
    def nodeId: NodeId = sei.nodeId
  }
  final case class Choose[F[_], A, B, C, D, +S <: Stage](
      nodeId: NodeId,
      fac: PreparedStep[F, A, C, S],
      fbd: PreparedStep[F, B, D, S]
  ) extends PreparedStep[F, Either[A, B], Either[C, D], S]
  final case class SubstVars[F[_], I, A, C](
      nodeId: NodeId,
      sub: Alg[C, A]
  ) extends PreparedStep[F, I, A, Stage.Compilation[C]]

  // first pass requires variables
  final case class PrecompileMeta[F[_], I, C](
      nodeId: NodeId,
      meta: Eval[Alg[C, PreparedMeta[F, Stage]]]
  ) extends AnyRef
      with PreparedStep[Nothing, I, FieldMeta[F], Stage.Compilation[C]]

  // second pass has substituted variables
  final case class EvalMeta[F[_], I](
      nodeId: NodeId,
      meta: Eval[PreparedMeta[F, Stage]]
  ) extends AnyRef
      with PreparedStep[Nothing, I, FieldMeta[F], Stage.Execution]
}

sealed trait Prepared[+F[_], I, +S <: Stage]

final case class PreparedCont[+F[_], I, A, +S <: Stage](
    edges: PreparedStep[F, I, A, S],
    cont: Prepared[F, A, S]
)

final case class Selection[F[_], I, +S <: Stage](
    nodeId: NodeId,
    fields: List[PreparedField[F, I, S]],
    source: ast.Selectable[F, I]
) extends Prepared[F, I, S]

final case class PreparedList[F[_], A, C, B, +S <: Stage](
    id: NodeId,
    of: PreparedCont[F, A, B, S],
    toSeq: C => Seq[A]
) extends Prepared[F, C, S]

final case class PreparedOption[F[_], I, O, +S <: Stage](
    id: NodeId,
    of: PreparedCont[F, I, O, S]
) extends Prepared[F, Option[I], S]

final case class PreparedLeaf[F[_], I, +S <: Stage](
    nodeId: NodeId,
    name: String,
    encode: I => Json
) extends Prepared[F, I, S]

sealed trait ParsedArgs[+S <: Stage]
object ParsedArgs {
  final case class Compilation[C]() extends ParsedArgs[Stage.Compilation[C]]
  final case class Execution(
      args: Map[Arg[?], Any]
  ) extends ParsedArgs[Stage.Execution]
}

final case class PreparedDataField[+F[_], A, B, +S <: Stage](
    nodeId: NodeId,
    name: String,
    alias: Option[String],
    cont: PreparedCont[F, A, B, S],
    source: ast.Field[F, A, B],
    parsedArgs: ParsedArgs[S]
    // parsedArgs: Map[Arg[?], Any]
) extends PreparedField[F, A, S] {
  lazy val outputName = alias.getOrElse(name)

  // def arg[C](a: Arg[C]): Option[C] =
  //   parsedArgs.get(a).asInstanceOf[Option[C]]
}

sealed trait Specialization[F[_], A, B, +S <: Stage] {
  def specify(a: A): Ior[String, Option[B]]
  def source: ast.Selectable[F, A]
  def target: ast.Type[F, B]
  def typename = target.name
}
object Specialization {
  final case class Type[F[_], A, +S <: Stage](
      source: ast.Type[F, A]
  ) extends Specialization[F, A, A, S] {
    def target: ast.Type[F, A] = source
    def specify(a: A): Ior[String, Option[A]] = a.rightIor.map(_.some)
  }
  final case class Union[F[_], A, B, +S <: Stage](
      source: ast.Union[F, A],
      variant: ast.Variant[F, A, B]
  ) extends Specialization[F, A, B, S] {
    def target = variant.tpe.value
    def specify(a: A): Ior[String, Option[B]] = variant.specify(a)
  }
  final case class Interface[F[_], A, B, +S <: Stage](
      target: ast.Type[F, B],
      impl: ast.Implementation[F, B, A]
  ) extends Specialization[F, A, B, S] {
    def source = impl.implementation.value
    def specify(a: A): Ior[String, Option[B]] = impl.specify(a)
  }
}

final case class PreparedSpecification[F[_], I, A, +S <: Stage](
    nodeId: NodeId,
    specialization: Specialization[F, I, A, S],
    selection: List[PreparedDataField[F, A, ?, S]]
) extends PreparedField[F, I, S]

final case class PreparedMeta[+F[_], +S <: Stage](
    variables: VariableMap[Unit],
    args: Option[QA.Arguments[Unit, AnyValue]],
    pdf: PreparedDataField[F, ?, ?, S]
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
