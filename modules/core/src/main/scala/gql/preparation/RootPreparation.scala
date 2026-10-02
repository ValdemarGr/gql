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
import gql.InverseModifier
import gql.InverseModifierStack
import gql.ModifierStack
import gql.SchemaShape
import gql.parser.Const
import gql.parser.{QueryAst => QA}
import gql.parser.{Value => V}
import io.circe._

class RootPreparation[F[_], C] {
  type G[A] = Alg[C, A]
  val G = Alg.Ops[C]

  def pickRootOperation(
      ops: List[(QA.OperationDefinition[C], C)],
      operationName: Option[String]
  ): G[QA.OperationDefinition[C]] = {
    lazy val applied = ops.map { case (x, _) => x }

    lazy val positions = ops.map { case (_, x) => x }

    lazy val possible = applied
      .collect { case d: QA.OperationDefinition.Detailed[C] => d.name }
      .collect { case Some(x) => s"'$x'" }
      .mkString(", ")

    (applied, operationName) match {
      case (Nil, _)      => G.raise(s"No operations provided.", Nil)
      case (x :: Nil, _) => G.pure(x)
      case (_, _) if applied.exists {
            case _: QA.OperationDefinition.Simple[C]                     => true
            case x: QA.OperationDefinition.Detailed[C] if x.name.isEmpty => true
            case _                                                       => false
          } =>
        G.raise(s"Exactly one operation must be suplied if the operations include at least one unnamed operation.", positions)
      case (_, None) =>
        G.raise(s"Operation name must be supplied when supplying multiple operations, provided operations are $possible.", positions)
      case (_, Some(name)) =>
        val o = applied.collectFirst { case d: QA.OperationDefinition.Detailed[C] if d.name.contains(name) => d }
        G.raiseOpt(o, s"Unable to find operation '$name', provided possible operations are $possible.", positions)
    }
  }

  def variableTypes(
      op: QA.OperationDefinition[C],
      schema: SchemaShape[F, ?, ?, ?]
  ): Alg[C, TypeMap] = {
    op match {
      case QA.OperationDefinition.Simple(_) => G.pure(Map.empty)
      case QA.OperationDefinition.Detailed(_, _, variableDefinitions, _, _) =>
        val allVariables = variableDefinitions.toList.flatMap(_.nel.toList)
        allVariables
          .groupBy(_.name)
          .toList
          .collect { case (name, defs) if defs.length > 1 => (name, defs) }
          .parTraverse_ { case (name, defs) =>
            val distinctTypes = defs.map(_.tpe).distinct.map(ModifierStack.fromType)
            val showTypes = distinctTypes.map(x => s"\'${x.show(identity)}\'").mkString(", ")
            G.raise(
              s"Variable '$$${name}' is defined multiple times (${showTypes}).",
              defs.map(_.c)
            )
          } >>
          allVariables
            .parTraverse[G, (String, gql.parser.Type)] { pvd =>
              val pos = pvd.c
              val vd = pvd
              val ms = ModifierStack.fromType(vd.tpe)
              schema.stubInputs.get(ms.inner) match {
                case None =>
                  G.raise(
                    s"Variable '$$${vd.name}' referenced type `${ms.inner}`, but `${ms.inner}` does not exist in the schema.",
                    List(pos)
                  )
                case Some(_) => G.pure((vd.name, vd.tpe))
              }
            }
            .map(_.toMap)
    }
  }

  def variables(
      op: QA.OperationDefinition[C],
      schema: SchemaShape[F, ?, ?, ?]
  ): Alg[C, VariableMap[C]] = {
    val ap = new ArgParsing[C](Map.empty)
    /*
     * Convert the variable signature into a gql arg and parse both the default value and the provided value
     * Then save the provided getOrElse default into a map along with the type
     */
    op match {
      case QA.OperationDefinition.Simple(_) => G.pure(Map.empty)
      case QA.OperationDefinition.Detailed(_, _, variableDefinitions, _, _) =>
        variableDefinitions.toList
          .flatMap(_.nel.toList)
          .parTraverse[G, (String, Variable[C])] { pvd =>
            val pos = pvd.c
            val vd = pvd

            val ms = ModifierStack.fromType(vd.tpe)

            schema.stubInputs.get(ms.inner) match {
              case None =>
                G.raise(
                  s"Variable '$$${vd.name}' referenced type `${ms.inner}`, but `${ms.inner}` does not exist in the schema.",
                  List(pos)
                )
              case Some(stubTLArg) =>
                val t = InverseModifierStack.toIn(ms.copy(inner = stubTLArg).invert)
                G.getVariables.flatMap { supplied =>
                  val value = supplied.get(vd.name).map(_.value).orElse(vd.defaultValue.map(_.asRight[Json]))
                  val selected: G[Either[Json, V[Const, C]]] = value match {
                    case Some(x) => G.pure(x)
                    case None if ms.invert.modifiers.headOption.contains(InverseModifier.Optional) =>
                      G.pure(Right(V.NullValue(pos)))
                    case None => G.raise(s"Variable '$$${vd.name}' is required but was not provided.", List(pos))
                  }
                  selected.flatMap { x =>
                    G.ambientField(vd.name) {
                      t match {
                        case in: gql.ast.In[a] =>
                          val (v, amb) = x match {
                            case Left(j)  => (V.fromJson(j).as(pos), true)
                            case Right(v) => (v, false)
                          }
                          ap.decodeIn(in, v.map(List(_)), ambigiousEnum = amb).void
                      }
                    }.as(vd.name -> Variable(vd.tpe, x))
                  }
                }
            }
          }
          .map(_.toMap)
    }
  }

  def prepareRoot[Q, M, S](
      od: QA.OperationDefinition[C],
      frags: List[QA.FragmentDefinition[C]],
      schema: SchemaShape[F, Q, M, S],
      types: TypeMap
  ): G[PreparedRoot[F, Q, M, S]] = {
    val (ot, ss) = od match {
      case QA.OperationDefinition.Simple(ss)                => (QA.OperationType.Query, ss)
      case QA.OperationDefinition.Detailed(ot, _, _, _, ss) => (ot, ss)
    }

    def runWith[A](o: gql.ast.Type[F, A]): G[Selection[F, A]] = {
      val ap = new ArgParsing[C](types)
      val da = new DirectiveAlg[F, C](schema.discover.positions, ap)
      val fc = new FieldCollection[F, C](schema.discover.implementations, frags.map(x => x.name -> x).toMap, ap, da)
      val fm = new FieldMerging[C]
      val qp = new QueryPreparation[F, C](ap, da, schema.discover.implementations)

      val build = fc.collectSelectionInfo(o, ss).flatMap {
        case x :: xs =>
          val selections = NonEmptyList(x, xs)
          fm.checkSelectionsMerge(selections).parProductR(qp.prepareSelectable(o, selections))
        case _ => G.nextId.map(NodeId(_)).map(Selection(_, Nil, o))
      }
      // Both passes run independently; validation owns shared decoding errors.
      val prepared = fc
        .validateSelectionSet(o, ss)
        .parProductR(build.attempt)
        .flatMap(_.fold(Alg.raiseErrors[C], G.pure(_)))

      prepared.flatMap { result =>
        G.usedVariables.flatMap { used =>
          val unused = types.keySet -- used
          if (unused.nonEmpty) G.raise(s"Unused variables: ${unused.map(str => s"'$str'").mkString(", ")}", Nil)
          else G.pure(result)
        }
      }
    }

    ot match {
      case QA.OperationType.Query =>
        val i: NonEmptyList[(String, gql.ast.Field[F, Unit, ?])] = schema.introspection
        val q = schema.query
        val full = q.copy(fields = i.map { case (k, v) => k -> v.contramap[F, Q](_ => ()) } concatNel q.fields)
        runWith[Q](full).map(PreparedRoot.Query(_))
      case QA.OperationType.Mutation =>
        G.raiseOpt(schema.mutation, "No `Mutation` type defined in this schema.", Nil)
          .flatMap(runWith[M])
          .map(PreparedRoot.Mutation(_))
      case QA.OperationType.Subscription =>
        G.raiseOpt(schema.subscription, "No `Subscription` type defined in this schema.", Nil)
          .flatMap(runWith[S])
          .map(PreparedRoot.Subscription(_))
    }
  }
}

object RootPreparation {
  def prepareCacheable[F[_], C, Q, M, S](
      executabels: NonEmptyList[QA.ExecutableDefinition[C]],
      schema: SchemaShape[F, Q, M, S],
      operationName: Option[String]
  ): EitherNec[PositionalError[C], Map[String, Json] => EitherNec[PositionalError[C], PreparedRoot[F, Q, M, S]]] = {
    val rp = new RootPreparation[F, C]
    val (ops, frags) = executabels.toList.partitionEither {
      case QA.ExecutableDefinition.Operation(op, c)  => Left((op, c))
      case QA.ExecutableDefinition.Fragment(frag, _) => Right(frag)
    }
    for {
      pick <- rp.pickRootOperation(ops, operationName).run
      od <- pick(Map.empty)
      declared <- rp.variableTypes(od, schema).run
      types <- declared(Map.empty)
      normalize <- rp.variables(od, schema).run
      bind <- rp.prepareRoot(od, frags, schema, types).run
    } yield { supplied =>
      val raw = supplied.toList.flatMap { case (name, value) => types.get(name).map(tpe => name -> Variable[C](tpe, Left(value))) }.toMap
      normalize(raw).flatMap(bind)
    }
  }

  def prepareRun[F[_], C, Q, M, S](
      executabels: NonEmptyList[QA.ExecutableDefinition[C]],
      schema: SchemaShape[F, Q, M, S],
      variableMap: Map[String, Json],
      operationName: Option[String]
  ): EitherNec[PositionalError[C], PreparedRoot[F, Q, M, S]] =
    prepareCacheable(executabels, schema, operationName).flatMap(_(variableMap))
}

sealed trait PreparedRoot[G[_], Q, M, S]
object PreparedRoot {
  final case class Query[G[_], Q, M, S](query: Selection[G, Q]) extends PreparedRoot[G, Q, M, S]
  final case class Mutation[G[_], Q, M, S](mutation: Selection[G, M]) extends PreparedRoot[G, Q, M, S]
  final case class Subscription[G[_], Q, M, S](subscription: Selection[G, S]) extends PreparedRoot[G, Q, M, S]
}
