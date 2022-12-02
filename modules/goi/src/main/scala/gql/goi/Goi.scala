/*
 * Copyright 2022 Valdemar Grange
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
import scala.reflect.ClassTag
import cats.data._
import cats._
import gql.SchemaShape

trait Node {
  def value: Any = this
}

object Node {
  def apply(value: Any): Node = NodeImpl(value)

  def unapply(node: Node): Option[Any] = Some(node.value)

  private case class NodeImpl(override val value: Any) extends Node

  val nodeInterface = interface[cats.Id, Node](
    "Node",
    "id" -> pure(_ => ID("root"))
  )

  implicit def nodeInterfaceF[F[_]]: Interface[F, Node] = nodeInterface.asInstanceOf[Interface[F, Node]]

}

object Goi {
  def makeId[F[_]](typename: String, id: String)(implicit F: Sync[F]): F[String] =
    F.delay(new String(Base64.getEncoder.encode(s"$typename:$id".getBytes()), StandardCharsets.UTF_8))

  def addIdWith[F[_], A](encode: A => String, ol: ObjectLike[F, A], specify: Node => Option[A])(implicit
      F: Sync[F]
  ): ObjectLike[F, A] = {
    val impl = gql.ast.Implementation(Eval.now(Node.nodeInterface.asInstanceOf[Interface[F, Node]]))(specify)
    def encEffect(a: A): F[String] = makeId[F](ol.name, encode(a))

    ol match {
      case t @ gql.ast.Type(_, _, _, _) =>
        t.copy(implementations = impl :: t.implementations)
          .addFields("id" -> eff[A](x => encEffect(x).map(ID(_))))
      case i @ gql.ast.Interface(_, _, _, _) =>
        i.copy(implementations = impl :: i.implementations)
          .addFields("id" -> eff[A](x => encEffect(x).map(ID(_))))
    }
  }

  def addId[F[_], A: ClassTag](encode: A => String, ol: ObjectLike[F, A])(implicit
      F: Sync[F]
  ): ObjectLike[F, A] = {
    val specify: PartialFunction[Node, A] = { case Node(x: A) => x }
    addIdWith[F, A](encode, ol, specify.lift)
  }

  def node[F[_], Q, M, S](shape: SchemaShape[F, Q, M, S], xs: (String, String => F[Option[?]])*)(implicit
      F: Sync[F]
  ): SchemaShape[F, Q, M, S] = {
    val lookup = xs.toMap
    shape.copy(query =
      shape.query.copy(
        fields = shape.query.fields.append[(String, Field[F, Q, ?, ?])](
          "node" -> fallible[Q](arg[ID[String]]("id")) { case (_, id) =>
            F.delay(new String(Base64.getDecoder().decode(id.value), StandardCharsets.UTF_8))
              .map(_.split(":").toList)
              .flatMap[Ior[String, Option[Node]]] {
                case typename :: id :: Nil =>
                  lookup.get(typename) match {
                    case None    => F.pure(s"Typename `$typename` with id '$id' does not have a getter.".leftIor)
                    case Some(f) => f(id).map(_.map(x => Node(x))).map(_.rightIor)
                  }
                case xs => F.pure(s"Invalid id parts ${xs.map(s => s"'$s'").mkString(", ")}".leftIor)
              }
          }
        )
      )
    )
  }

  def instance[F[_]: Functor, A](ot: ObjectLike[F, A])(f: String => F[Option[A]]): (String, String => F[Option[?]]) =
    (ot.name, f.map(_.map(_.map(x => x))))

  def validate[F[_], Q, M, S](shape: SchemaShape[F, Q, M, S], instances: List[(String, String => F[Option[?]])]): List[String] = {
    val instanceSet = instances.map { case (name, _) => name }.toSet
    val nodeInstances = shape.discover.implementations.get(Node.nodeInterface.name)
    val implementing = nodeInstances.map(_.keySet).getOrElse(Set.empty)
    val tooMany = instanceSet -- implementing
    val missing = implementing -- instanceSet

    tooMany.toList.map(n => s"Type `$n` was declared as a node GOI instance but did not extend the `Node` type.") ++
      missing.toList.map(n => s"Type `$n` extends the `Node` type but was not declared as a node GOI instance.")
  }

  /*
   * Notes and ideas:
   *
   * "Debtor:abcd-4ecj-18eho9ou-o8eug" -> "ethOhero.3hore"
   * "ethOhero.3hore" -> "Debtor:abcd-4ecj-18eho9ou-o8eug" -> Debtor(...)
   * (Typename, NodeId => F[Option[Debtor]], Debtor => NodeId)
   *
   * final case class Debtor(
   *  entityId: UUID,
   *  name: String,
   *  email: String
   * )
   *
   * Goi[F, Debtor](
   *   debtorId => F.fromTry(Try(UUID.fromString(debtorId))).flatMap(debtorService.getDebtor),
   *   debtor => debtor.entityId.toString
   * )(
   *   tpe[F, Debtor](
   *     "Debtor",
   *     "name" -> pure(_.name),
   *     "email" -> pure(_.email),
   *   )
   * ).map{ implicit debtor =>
   *   ...
   * }
   *
   * trait GoiConnstructor[A] {
   *  def apply(...)
   * }
   *
   * objec Goi {
   *  def apply[A](...): Writer[GoiState, GoiConstructor[A]] = ???
   * }
   *
   * final case class LoanAndLeaseConstructors(
   *  debtor: GoiConstructor[Debtor],
   *  contract: GoiConstructor[Contract],
   *  )
   *
   * object LoanAndLeaseConstructors {
   *   def apply(...): Writer[GoiState, LoanAndLeaseConstructors] = {
   *    val d = Goi[F, Debtor](
   *      "Debtor",
   *      debtorId => F.fromTry(Try(UUID.fromString(debtorId))).flatMap(debtorService.getDebtor),
   *      debtor => debtor.entityId.toString
   *    )
   *
   *    val c = Goi[F, Contract](
   *      "Contract",
   *      ...
   *    )
   *
   *    (d, c).mapN(LoanAndLeaseConstructors.apply)
   *  }
   * }
   *
   * ...
   *
   * Goi[F, Debtor](
   *   "Debtor",
   *   debtorId => F.fromTry(Try(UUID.fromString(debtorId))).flatMap(debtorService.getDebtor),
   *   debtor => debtor.entityId.toString
   * ).map{ mkDebtor =>
   *   implicit val debtor = mkDebtor(
   *     "name" -> pure(_.name),
   *     "email" -> pure(_.email),
   *   )
   * }
   *
   * ```
   * interface Node {
   *   id: ID!
   * }
   *
   * type Debtor implements Node {
   *   id: ID!
   *   name: String!
   *   email: String!
   * }
   *
   * type Query {
   *   node(id: ID!): Node
   * }
   * ```
   *
   * {
   *   implicit val debtor = goi(_.entityId.toString)(tpe[F, Debtor](
   *     "Debtor",
   *     "name" -> pure(_.name),
   *     "email" -> pure(_.email),
   *   ))
   *
   *   ...
   *
   *   val schema = makeSchema[F]
   *   addGoi(schema){
   *     case ("Debtor", id) => F.fromTry(Try(UUID.fromString(id))).flatMap(debtorService.getDebtor)
   *   }
   * }
   *
   * {
   *   implicit val debtor = goi(_.entityId.toString)(tpe[F, Debtor](
   *     "Debtor",
   *     "name" -> pure(_.name),
   *     "email" -> pure(_.email),
   *   ))
   *
   *   ...
   *
   *   val schema = makeSchema[F]
   *   schema
   *      .addGoi(debtor, id => F.fromTry(Try(UUID.fromString(id))).flatMap(debtorService.getDebtor))
   * }
   *

     trait LoanAndLease[F[_]] {
       implicit def debtor: Type[F, Debtor]

       implicit def contract: Type[F, Contract]
     }

     class LoanAndLease[F[_]](dep1: Dep1[F]) {
       implicit lazy val debtor: Type[F, Debtor] = tpe[F, Debtor](
        "Debtor"
        ...
      )
     }
   *
   */
}
