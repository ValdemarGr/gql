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
import cats.Eval
import scala.reflect.ClassTag
import cats.data.Writer

trait Node {
  def id: String
}

object Node {
  // final case class Base64(id: String) extends AnyVal

  trait GoiNode[F[_]] {
    type Value

    def typename: String

    def get(id: String): F[Option[Value]]

    def encodeId(value: Value): String
  }

  def make[F[_], A](typename: String, get: String => F[Option[A]], encode: A => String): GoiNode[F] = {
    val typename0 = typename
    val get0 = x => get(x)
    val encode0 = x => encode(x)
    new GoiNode[F] {
      type Value = A

      def typename: String = typename0

      def get(id: String): F[Option[A]] = get0(id)

      def encodeId(value: A): String = encode0(value)
    }
  }

  val nodeInterface = interface[cats.Id, Node](
    "Node",
    "id" -> pure(x => ID(x.id))
  )

  implicit def node[F[_]] = nodeInterface.asInstanceOf[Interface[F, Node]]

  /*
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
   */

  def apply[F[_], A: ClassTag](get: String => F[Option[A]], encode: A => String)(ol: ObjectLike[F, A])(implicit F: Sync[F]) = {
    val specify: PartialFunction[Any, A] = { case x: A => x }
    val impl = gql.ast.Implementation(Eval.now(node[F].asInstanceOf[Interface[F, Any]]))(specify.lift)
    def encEffect(a: A): F[String] =
      F.delay(new String(Base64.getEncoder.encode(s"${ol.name}:${encode(a)}".getBytes()), StandardCharsets.UTF_8))

    val newType = ol match {
      case t @ gql.ast.Type(_, _, _, _) =>
        t.copy(implementations = impl :: t.implementations)
          .addFields("id" -> eff[A](x => encEffect(x).map(ID(_))))
      case i @ gql.ast.Interface(_, _, _, _) =>
        i.copy(implementations = impl :: i.implementations)
          .addFields("id" -> eff[A](x => encEffect(x).map(ID(_))))
    }
    val goiNode = make[F, A](newType.name, get, encode)

    newType.writer(goiNode)
  }

  def apply2[F[_], A <: Node: ClassTag](get: String => F[Option[A]], encode: A => String)(ol: ObjectLike[F, A])(implicit F: Sync[F]) = {
    def encEffect(a: A): F[String] =
      F.delay(new String(Base64.getEncoder.encode(s"${ol.name}:${encode(a)}".getBytes()), StandardCharsets.UTF_8))

    val newType = ol match {
      case t @ gql.ast.Type(_, _, _, _) =>
        t.subtypeOf[Node]
          .addFields("id" -> eff[A](x => encEffect(x).map(ID(_))))
      case i @ gql.ast.Interface(_, _, _, _) =>
        i.subtypeOf[Node]
          .addFields("id" -> eff[A](x => encEffect(x).map(ID(_))))
    }
    val goiNode = make[F, A](newType.name, get, encode)

    newType.writer(goiNode)
  }

  // def fromTypename[F[_], A](typename: String, get: String => F[Option[A]], id: A => String)(implicit F: Sync[F]) = {
  //   def get0(id: String) =
  //     F.delay(new String(Base64.getDecoder.decode(id), StandardCharsets.UTF_8)).map(_.split(':').toList).map {
  //       case x :: xs if x == typename => Some(get(xs.mkString(":")))
  //       case _                        => None
  //     }
  //   def id0(a: A) =
  //     F.delay(new String(Base64.getEncoder.encode(s"$typename:${id(a)}".getBytes()), StandardCharsets.UTF_8))
  //   (get0 _, id0 _)
  // }

  // def fromType[F[_]: Sync, A <: Node](get: String => F[Option[A]], id: A => String)(tpe: Type[F, A]) = {
  //   val (get0, id0) = fromTypename(tpe.name, get, id)
  //   // tpe.imple
  // }
}
