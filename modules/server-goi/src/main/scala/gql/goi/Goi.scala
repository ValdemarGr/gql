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
package gql.goi

import gql.ast._
import cats.effect._
import java.util.Base64
import java.nio.charset.StandardCharsets
import cats.implicits._
import cats.data._
import cats._
import gql.SchemaShape
import gql.resolver.Resolver
import gql.dsl.all._
import cats.mtl._

trait Node {
  def value: Any = this
  def typename: String
}

object Node {
  def apply(value: Any, typename: String): Node = NodeImpl(value, typename)

  def unapply(node: Node): Option[Any] = Some(node.value)

  private case class NodeImpl(override val value: Any, typename: String) extends Node

  implicit val nodeInterface: Interface[fs2.Pure, Node] = interface[fs2.Pure, Node](
    "Node",
    "id" -> abst[fs2.Pure, ID[String]]
  )
}

object Goi {
  def makeId[F[_]](typename: String, id: String)(implicit F: Sync[F]): F[String] =
    F.delay(new String(Base64.getEncoder.encode(s"$typename:$id".getBytes()), StandardCharsets.UTF_8))

  def makeImpl[A](specify: Node => Option[A]) =
    gql.ast.Implementation(Eval.now(Node.nodeInterface))(specify.andThen(_.rightIor))

  def addIdWith[F[_], A, B](
      t: Type[F, A],
      resolver: Resolver[F, A, B],
      fromIds: NonEmptyList[B] => IorT[F, String, Map[B, Ior[String, A]]],
      specify: Node => Option[A]
  )(implicit F: Sync[F], idCodec: IDCodec[B]): Type[F, A] =
    t
      .copy(implementations = makeImpl[A](specify) :: t.implementations)
      .addFields(
        "id" -> build.from(resolver.evalMap(b => makeId[F](t.name, encodeString[B](b)).map(ID(_))))
      )
      .addAttributes(GoiAttribute(idCodec, fromIds))

  def addId[F[_], A, B](
      t: Type[F, A],
      resolver: Resolver[F, A, B],
      fromIds: NonEmptyList[B] => IorT[F, String, Map[B, Ior[String, A]]]
  )(implicit
      F: Sync[F],
      idCodec: IDCodec[B]
  ): Type[F, A] =
    addIdWith[F, A, B](
      t,
      resolver,
      fromIds,
      x =>
        if (x.typename === t.name) Some(x.value.asInstanceOf[A])
        else None
    )

  def addId[F[_], A](t: Interface[F, A]): Interface[F, A] =
    t.addFields("id" -> abst[F, ID[String]])

  def decodeInput[A](typename: String, codec: IDCodec[A], elems: Array[String]) = {
    val xs = codec.codecs
    if (xs.size =!= elems.size.toLong)
      s"Invalid Global object identifier size. Expected ${xs.size + 1} id parts seperated by :, but got ${elems.size + 1} parts. The types must be ${xs
          .prepend("typename(string)")
          .mkString_(":")}. The provided id parts were ${(typename :: elems.toList).mkString(":")}.".invalidNec
    else codec.decode(elems)
  }

  def encodeString[A](a: A)(implicit idCodec: IDCodec[A]): String = idCodec.encode(a).mkString_(":")

  def getId[F[_]](id: String)(implicit F: Sync[F]): IorT[F, String, (String, List[String])] = IorT {
    F.delay(new String(Base64.getDecoder().decode(id), StandardCharsets.UTF_8)).map { fullId =>
      if (fullId.isEmpty) s"Empty id".leftIor
      else
        fullId.split(":").toList match {
          case Nil            => s"Empty id".leftIor
          case typename :: ys => (typename -> ys).rightIor
        }
    }
  }

  final case class DecodedIds[F[_], V, K](gid: CollectedAttribute[F, V, K], keys: NonEmptyList[(String, K)])
  def decodeIds[F[_]: Sync, G[_]: NonEmptyTraverse](
      ids: G[String],
      lookup: Map[String, CollectedAttribute[F, ?, ?]],
      schemaTypes: Set[String]
  ): IorT[F, String, List[DecodedIds[F, ?, ?]]] =
    ids
      .nonEmptyTraverse[IorT[F, String, *], (String, (String, List[String]))](id => getId(id) tupleLeft id)
      .subflatMap { outs =>
        val m = outs.toNonEmptyList.groupMap { case (_, (k, _)) => k } { case (id, (_, v)) => (id, v) }
        m.toList.traverse { case (typename, keys) =>
          lookup.get(typename) match {
            case None if schemaTypes.contains(typename) =>
              s"Typename `$typename` does not have a global object identitifaction defined for it.".leftIor
            case None => s"Typename `$typename` does not exist in this schema.".leftIor
            case Some(gid) =>
              gid match {
                case gid: CollectedAttribute[F, v, k] =>
                  keys
                    .traverse[Ior[String, *], (String, k)] { case (id, key) =>
                      decodeInput(typename, gid.attribute.codec, key.toArray).toEither
                        .leftMap(_.mkString_("\n"))
                        .toIor
                        .map(id -> _)
                    }
                    .map(DecodedIds[F, v, k](gid, _))
              }
          }
        }
      }

  def runIds[F[_]: Parallel, G[_]: NonEmptyTraverse](
      ids: G[String],
      lookup: Map[String, CollectedAttribute[F, ?, ?]],
      schemaTypes: Set[String]
  )(implicit F: Sync[F]): IorT[F, String, G[Option[Ior[String, Node]]]] =
    decodeIds[F, G](ids, lookup, schemaTypes)
      .flatMap(_.parFlatTraverse { case di: DecodedIds[F, v, k] =>
        val ks = (di.keys: NonEmptyList[(String, k)])
        val reverseMapping: Map[k, String] = ks.map { case (id, key) => key -> id }.toList.toMap
        val resultsF: IorT[F, String, Map[k, Ior[String, v]]] = di.gid.attribute.fromIds(ks.map { case (_, key) => key })
        resultsF.map(_.toList.mapFilter { case (k, v) => reverseMapping.get(k) tupleRight v.map(Node(_, di.gid.typename)) })
      })
      .map(_.toMap)
      .map(lookup => ids.map(lookup.get))

  def node[F[_]: Parallel, Q, M, S](shape: SchemaShape[F, Q, M, S], xs: List[CollectedAttribute[F, ?, ?]])(implicit
      F: Sync[F]
  ): SchemaShape[F, Q, M, S] = {
    val lookup = xs.map(x => x.typename -> x).toMap
    val sts = shape.discover.toplevels.keySet
    builder[F, Q] { b =>
      implicit lazy val optIorNode: Out[F, Option[Ior[String, Node]]] =
        OutOpt[F, Ior[String, Node], Node](Node.nodeInterface, Resolver.id[F, Ior[String, Node]].rethrow)

      def nodeField[G[_]](out: Out[F, G[Option[Ior[String, Node]]]])(implicit
          a: In[G[ID[String]]],
          G: NonEmptyTraverse[G]
      ): Field[F, Q, G[Option[Ior[String, Node]]]] =
        b.from(
          arged[F, Q, G[ID[String]]](arg[G[ID[String]]]("id"))
            .evalMap(id => runIds[F, G](id.map(_.value), lookup, sts).value)
            .rethrow
        )(out)

      shape.copy(query =
        shape.query.copy(
          fields = shape.query.fields ::: fields[F, Q](
            "node" -> nodeField[Id](optIorNode),
            "nodes" ->
              nodeField[NonEmptyList](gql.ast.gqlOutArrForTraversable(NonEmptyTraverse[NonEmptyList], optIorNode))
          )
        )
      )
    }
  }

  final case class CollectedAttribute[F[_], A, B](
      typename: String,
      attribute: GoiAttribute[F, A, B]
  )
  def collectAttributes[F[_]](
      shape: SchemaShape[F, ?, ?, ?]
  ): Either[NonEmptyChain[String], List[CollectedAttribute[F, _, _]]] = {
    type State = List[CollectedAttribute[F, ?, ?]]
    type Effect[A] = EitherT[Eval, NonEmptyChain[String], A]
    def go[H[_]: Defer](implicit H: Monad[H], R: Raise[H, NonEmptyChain[String]]): H[State] =
      shape.visitOnce[H, State] { case SchemaShape.VisitNode.OutNode(t: Type[F, a]) =>
        val fas = t.attributes.collect { case g: GoiAttribute[F, a, ?] => g }
        val n = t.name

        Some {
          fas match {
            case Nil => H.pure(Nil)
            case (x: GoiAttribute[F, a, id]) :: xs =>
              if (xs.nonEmpty) R.raise(NonEmptyChain.one(s"More than one goi attribute found on `$n`"))
              else H.pure(List(CollectedAttribute[F, a, id](n, x)))
          }
        }
      }

    go[Effect].value.value
  }

  def addSchemaGoi[F[_]: Parallel: Sync, Q, M, S](schema: SchemaShape[F, Q, M, S]): SchemaShape[F, Q, M, S] =
    node(schema, collectAttributes(schema).getOrElse(Nil))

  def validateFor[F[_], Q, M, S](shape: SchemaShape[F, Q, M, S], instances: List[CollectedAttribute[F, ?, ?]]): List[String] = {
    val instanceSet = instances.map(_.typename).toSet
    val all = shape.discover.outputs.keySet
    // Parameters that have typenames do not occur in the schema
    val notImplemented = instanceSet -- all
    // Paramerters that do occur in the schema
    val implemented = instanceSet -- notImplemented

    // Parameters that are duplicated
    val duplicates = instances.groupBy(_.typename).toList.collect { case (k, v) if v.size > 1 => (k, v.size) }

    val nodeInstances = shape.discover.implementations.get(Node.nodeInterface.name)
    // All the typenames that implement the Node interface
    val implementing = nodeInstances.map(_.keySet).getOrElse(Set.empty)
    // Does occur but does not extend the Node interface
    val doesNotExtend = implemented -- implementing
    // Does extend the Node interface but does not occur in the parameters
    val missing = implementing -- implemented

    notImplemented.toList.map(n =>
      s"Type `$n` does not occur in the schema. Hint: GlobalID provides smart constructors for creating types that satisfy the rules of GOI."
    ) ++
      doesNotExtend.toList.map(n => s"Type `$n` was declared as a node GlobalID instance but did not extend the `Node` type.") ++
      missing.toList.map(n =>
        s"Type `$n` extends the `Node` type but was not declared as a node GlobalID instance. Hint: You might have forgot to include your GlobalID instance for `$n`."
      ) ++
      duplicates.toList.map { case (n, size) =>
        s"Type `$n` was declared $size times as a GlobalID instance. Hint: Ensure that the GlobalID instance for `$n` is only added once."
      }
  }

}
