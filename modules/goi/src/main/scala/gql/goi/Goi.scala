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
import cats.mtl.Tell
import cats.data._
import cats._
import gql.SchemaShape

trait Node {
  def value: Any
}

object Node {
  def apply(value: Any): Node = NodeImpl(value)

  def unapply(node: Node): Option[Any] = Some(node.value)

  private case class NodeImpl(value: Any) extends Node
}

object Goi {
  val nodeInterface = interface[cats.Id, Node](
    "Node",
    "id" -> pure(_ => ID("root"))
  )

  implicit def node[F[_]]: Interface[F, Node] = nodeInterface.asInstanceOf[Interface[F, Node]]

  def makeId[F[_]](typename: String, id: String)(implicit F: Sync[F]): F[String] =
    F.delay(new String(Base64.getEncoder.encode(s"$typename:$id".getBytes()), StandardCharsets.UTF_8))

  def tell[G[_], F[_], A: ClassTag](get: String => F[Option[A]], encode: A => String, ol: ObjectLike[F, A])(implicit
      F: Sync[F],
      T: Tell[G, List[GoiNode[F]]]
  ): G[ObjectLike[F, A]] = {
    val specify: PartialFunction[Any, A] = { case x: A => x }
    val impl = gql.ast.Implementation(Eval.now(node.asInstanceOf[Interface[F, Any]]))(specify.lift)
    def encEffect(a: A): F[String] = makeId[F](ol.name, encode(a))

    val newType = ol match {
      case t @ gql.ast.Type(_, _, _, _) =>
        t.copy(implementations = impl :: t.implementations)
          .addFields("id" -> eff[A](x => encEffect(x).map(ID(_))))
      case i @ gql.ast.Interface(_, _, _, _) =>
        i.copy(implementations = impl :: i.implementations)
          .addFields("id" -> eff[A](x => encEffect(x).map(ID(_))))
    }
    val goiNode = GoiNode[F, A](newType.name, get)

    T.writer(newType, List(goiNode))
  }

  // def tellFrom[G[_], F[_], A <: Node: ClassTag](get: String => F[Option[A]], encode: A => String)(
  //     ol: ObjectLike[F, A]
  // )(implicit F: Sync[F], T: Tell[G, List[GoiNode[F]]]): G[ObjectLike[F, A]] = {

  //   def encEffect(a: A): F[String] = makeId[F](ol.name, encode(a))

  //   val newType = ol match {
  //     case t @ gql.ast.Type(_, _, _, _) =>
  //       t.subtypeOf[Node].addFields("id" -> eff[A](x => encEffect(x).map(ID(_))))
  //     case i @ gql.ast.Interface(_, _, _, _) =>
  //       i.subtypeOf[Node].addFields("id" -> eff[A](x => encEffect(x).map(ID(_))))
  //   }
  //   val goiNode = GoiNode[F, A](newType.name, get)

  //   T.writer(newType, List(goiNode))
  // }

  def injectG[G[_]: Applicative, F[_]: Sync, A: ClassTag](get: String => F[Option[A]], encode: A => String, ol: ObjectLike[F, A]) =
    tell[WriterT[G, List[GoiNode[F]], *], F, A](get, encode, ol)

  def inject[F[_]: Sync, A: ClassTag](get: String => F[Option[A]], encode: A => String, ol: ObjectLike[F, A]) =
    tell[Writer[List[GoiNode[F]], *], F, A](get, encode, ol)

  def materializeG[G[_]: Functor, F[_], Q, M, S](
      schema: WriterT[G, List[GoiNode[F]], SchemaShape[F, Q, M, S]]
  )(implicit F: Sync[F]): G[SchemaShape[F, Q, M, S]] = {
    schema.run.map { case (state, shape) =>
      val stateMap = state.map(x => (x.typename, x)).toMap
      shape.copy(query =
        shape.query.copy(
          fields = shape.query.fields.append[(String, Field[F, Q, _, _])](
            "node" -> fallible[Q](arg[ID[String]]("id")) { case (_, id) =>
              F.delay(new String(Base64.getDecoder().decode(id.value), StandardCharsets.UTF_8))
                .map(_.split(":").toList)
                .flatMap[Ior[String, Option[Node]]] {
                  case typename :: id :: Nil =>
                    stateMap.get(typename) match {
                      case None    => F.pure(s"Typename `$typename` with id '$id' does not have a getter.".leftIor)
                      case Some(x) => x.get(id).map(_.map(x => Node(x))).map(_.rightIor)
                    }
                  case xs => F.pure(s"Invalid id parts ${xs.map(s => s"'$s'").mkString(", ")}".leftIor)
                }
            }
          )
        )
      )
    }
  }

  def materialize[F[_], Q, M, S](schema: Writer[List[GoiNode[F]], SchemaShape[F, Q, M, S]])(implicit F: Sync[F]): SchemaShape[F, Q, M, S] =
    materializeG[cats.Id, F, Q, M, S](schema)

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
   * Goi[F, Debtor](
   *   debtorId => F.fromTry(Try(UUID.fromString(debtorId))).flatMap(debtorService.getDebtor),
   *   debtor => debtor.entityId.toString
   * ).map{ debtorGoi: (Type[F, Debtor] => Type[F, Debtor]) =>
   *   implicit val debtor = debtorGoi(tpe[F, Debtor](
   *     "Debtor",
   *     "name" -> pure(_.name),
   *     "email" -> pure(_.email),
   *   ))
   * }
   *
   * Goi[F, Debtor]("Debtor", debtorId => F.fromTry(Try(UUID.fromString(debtorId))).flatMap(debtorService.getDebtor)).as{
   *   implicit val debtor = tpe[F, Debtor](
   *     "Debtor",
   *     "id" -> eff(x => mkId("Debtor", x.entityId.toString)),
   *     "name" -> pure(_.name),
   *     "email" -> pure(_.email),
   *   ).subtypeOf[Node]
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
