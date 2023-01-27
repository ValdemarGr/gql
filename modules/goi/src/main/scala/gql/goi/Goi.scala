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
import gql.dsl._
import cats.effect._
import java.util.Base64
import java.nio.charset.StandardCharsets
import cats.implicits._
import cats.data._
import cats._
import gql.SchemaShape
import gql.resolver.Resolver

trait Node {
  def value: Any = this
  def typename: String
}

object Node {
  def apply(value: Any, typename: String): Node = NodeImpl(value, typename)

  def unapply(node: Node): Option[Any] = Some(node.value)

  private case class NodeImpl(override val value: Any, typename: String) extends Node

  val nodeInterface = interface[cats.Id, Node](
    "Node",
    "id" -> abst[cats.Id, ID[String]]
  )

  implicit def nodeInterfaceF[F[_]]: Interface[F, Node] = nodeInterface.asInstanceOf[Interface[F, Node]]
}

object Goi {
  def makeId[F[_]](typename: String, id: String)(implicit F: Sync[F]): F[String] =
    F.delay(new String(Base64.getEncoder.encode(s"$typename:$id".getBytes()), StandardCharsets.UTF_8))

  def makeImpl[F[_], A](specify: Node => Option[A]) =
    gql.ast.Implementation(Eval.now(Node.nodeInterfaceF[F]))(specify)

  def addIdWith[F[_], A](resolver: Resolver[F, A, String], tpe: Type[F, A], specify: Node => Option[A])(implicit
      F: Sync[F]
  ): Type[F, A] =
    tpe
      .copy(implementations = makeImpl[F, A](specify) :: tpe.implementations)
      .addFields("id" -> field(resolver.evalMap(s => makeId[F](tpe.name, s).map(ID(_)))))

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
    t.addAbstractFields("id" -> abst[F, ID[String]])

  def decodeInput[A](codec: IDCodec[A], elems: Array[String]) = {
    val xs = codec.codecs
    if (xs.size =!= elems.size.toLong)
      s"Invalid Global object identifier size expected size ${xs.size} but got ${elems.size}: ${xs
        .mkString_(":")}.".invalidNec
    else codec.decode(elems)
  }

  def encodeString[A](a: A)(implicit idCodec: IDCodec[A]): String = idCodec.encode(a).mkString_(":")

  def node[F[_], Q, M, S](shape: SchemaShape[F, Q, M, S], xs: List[GlobalID[F, ?, ?]])(implicit
      F: Sync[F]
  ): SchemaShape[F, Q, M, S] = {
    val lookup = xs.map(x => x.typename -> x).toMap
    shape.copy(query =
      shape.query.copy(
        fields = shape.query.fields.append[(String, Field[F, Q, ?])](
          "node" -> field {
            Resolver
              .argument(arg[ID[String]]("id"))
              .evalMap { id =>
                F.delay(new String(Base64.getDecoder().decode(id.value), StandardCharsets.UTF_8))
                  .flatMap[Ior[String, Option[Node]]] { fullId =>
                    fullId.split(":").toList match {
                      case typename :: xs =>
                        lookup.get(typename) match {
                          case None => F.pure(s"Typename `$typename` with id '$id' does not have a getter.".leftIor)
                          case Some(gid) =>
                            decodeInput(gid.codec, xs.toArray).toEither
                              .leftMap(_.mkString_("\n"))
                              .toIor
                              .traverse(gid.fromId)
                              .map(_.map(_.map(x => Node(x, fullId))))
                        }
                      case xs => F.pure(s"Invalid id parts ${xs.map(s => s"'$s'").mkString(", ")}".leftIor)
                    }
                  }
              }
              .rethrow
          }
        )
      )
    )
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
