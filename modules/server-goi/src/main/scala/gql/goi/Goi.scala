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
import gql.dsl._

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
    gql.ast.Implementation(Eval.now(Node.nodeInterface))(specify)

  def addIdWith[F[_], A](resolver: Resolver[F, A, String], t: Type[F, A], specify: Node => Option[A])(implicit
      F: Sync[F]
  ): Type[F, A] =
    t
      .copy(implementations = makeImpl[A](specify) :: t.implementations)
      .addFields("id" -> build.from(resolver.evalMap(s => makeId[F](t.name, s).map(ID(_)))))

  def addId[F[_], A, B](resolver: Resolver[F, A, B], t: Type[F, A])(implicit
      F: Sync[F],
      idCodec: IDCodec[B]
  ): Type[F, A] =
    addIdWith[F, A](
      resolver.map(encodeString[B]),
      t,
      x =>
        if (x.typename === t.name) Some(x.value.asInstanceOf[A])
        else None
    )

  def addId[F[_], A](t: Interface[F, A]): Interface[F, A] =
    t.addFields("id" -> abst[F, ID[String]])

  def decodeInput[A](codec: IDCodec[A], elems: Array[String]) = {
    val xs = codec.codecs
    if (xs.size =!= elems.size.toLong)
      s"Invalid Global object identifier size expected size ${xs.size} but got ${elems.size}: ${xs
        .mkString_(":")}.".invalidNec
    else codec.decode(elems)
  }

  def encodeString[A](a: A)(implicit idCodec: IDCodec[A]): String = idCodec.encode(a).mkString_(":")

  def getId[F[_]](id: String)(implicit F: Sync[F]): IorT[F, String, (String, NonEmptyList[String])] = IorT {
    F.delay(new String(Base64.getDecoder().decode(id), StandardCharsets.UTF_8)).map { fullId =>
      fullId.split(":").toList match {
        case typename :: y :: ys => (typename -> NonEmptyList(y, ys)).rightIor
        case xs                  => s"Invalid id parts ${xs.map(s => s"'$s'").mkString(", ")}".leftIor
      }
    }
  }

  final case class DecodedIds[F[_], V, K](gid: GlobalID[F, V, K], keys: NonEmptyList[(String, K)])
  def decodeIds[F[_]: Sync, G[_]: NonEmptyTraverse](
      ids: G[String],
      lookup: Map[String, GlobalID[F, ?, ?]]
  ): IorT[F, String, List[DecodedIds[F, ?, ?]]] =
    ids
      .nonEmptyTraverse[IorT[F, String, *], (String, (String, NonEmptyList[String]))](id => getId(id) tupleLeft id)
      .subflatMap { outs =>
        val m = outs.toNonEmptyList.groupMap { case (_, (k, _)) => k } { case (id, (_, v)) => (id, v) }
        m.toList.traverse { case (typename, keys) =>
          lookup.get(typename) match {
            case None => s"Typename `$typename` does not have a getter.".leftIor
            case Some(gid) =>
              gid match {
                case gid: GlobalID[F, v, k] =>
                  keys
                    .traverse[Ior[String, *], (String, k)] { case (id, key) =>
                      decodeInput(gid.codec, key.toList.toArray).toEither
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
      lookup: Map[String, GlobalID[F, ?, ?]]
  )(implicit F: Sync[F]): IorT[F, String, G[Option[Node]]] =
    decodeIds[F, G](ids, lookup)
      .semiflatMap(_.parFlatTraverse { case di: DecodedIds[F, v, k] =>
        val reverseMapping: Map[k, String] = di.keys.map { case (id, key: k) => key -> id }.toList.toMap
        val resultsF: F[Map[k, v]] = di.gid.fromIds(di.keys.map { case (_, key: k) => key })
        resultsF.map(_.toList.mapFilter { case (k, v) => reverseMapping.get(k) tupleRight Node(v, di.gid.typename) })
      })
      .map(_.toMap)
      .map(lookup => ids.map(lookup.get))

  def node[F[_]: Parallel, Q, M, S](shape: SchemaShape[F, Q, M, S], xs: List[GlobalID[F, ?, ?]])(implicit
      F: Sync[F]
  ): SchemaShape[F, Q, M, S] = {
    val lookup = xs.map(x => x.typename -> x).toMap
    builder[F, Q] { b =>
      def nodeField[G[_]: NonEmptyTraverse](implicit a: In[G[ID[String]]]) = b.from(
        arged(arg[G[ID[String]]]("id"))
          .evalMap(id => runIds[F, G](id.map(_.value), lookup).value)
          .rethrow
      )

      shape.copy(query =
        shape.query.copy(
          fields = shape.query.fields ::: fields[F, Q](
            "node" -> nodeField[Id],
            "nodes" -> nodeField[NonEmptyList]
          )
        )
      )
    }
  }

  def validate[F[_], Q, M, S](shape: SchemaShape[F, Q, M, S], instances: List[GlobalID[F, ?, ?]]): List[String] = {
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
