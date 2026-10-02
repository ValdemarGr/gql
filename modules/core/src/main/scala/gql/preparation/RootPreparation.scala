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
import gql.ast

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
  ): Alg[C, Map[String, (gql.parser.Type, ast.InToplevel[?])]] = {
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
            .parTraverse[G, (String, (gql.parser.Type, ast.InToplevel[?]))] { pvd =>
              val pos = pvd.c
              val vd = pvd
              val ms = ModifierStack.fromType(vd.tpe)
              schema.stubInputs.get(ms.inner) match {
                case None =>
                  G.raise(
                    s"Variable '$$${vd.name}' referenced type `${ms.inner}`, but `${ms.inner}` does not exist in the schema.",
                    List(pos)
                  )
                case Some(stubInput) => G.pure((vd.name, (vd.tpe, stubInput)))
              }
            }
            .map(_.toMap)
    }
  }

  def variables(
      op: QA.OperationDefinition[C],
      variableMap: Map[String, Json],
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

            val oe: Option[Either[Json, V[Const, C]]] = (variableMap.get(vd.name).map(_.asLeft) orElse vd.defaultValue.map(_.asRight))

            val fo: G[Either[Json, V[Const, C]]] = oe match {
              case None =>
                if (ms.invert.modifiers.headOption.contains(InverseModifier.Optional)) G.pure(Right(V.NullValue(pos)))
                else G.raise(s"Variable '$$${vd.name}' is required but was not provided.", List(pos))
              case Some(x) =>
                schema.stubInputs.get(ms.inner) match {
                  case None =>
                    G.raise(
                      s"Variable '$$${vd.name}' referenced type `${ms.inner}`, but `${ms.inner}` does not exist in the schema.",
                      List(pos)
                    )
                  case Some(stubTLArg) =>
                    val t = InverseModifierStack.toIn(ms.copy(inner = stubTLArg).invert)

                    G.ambientField(vd.name) {
                      t match {
                        case in: gql.ast.In[a] =>
                          val (v, amb) = x match {
                            case Left(j)  => (V.fromJson(j).as(pos), true)
                            case Right(v) => (v, false)
                          }
                          ap.decodeIn[a](in, v.map(List(_)), ambigiousEnum = amb).void
                      }
                    } as x
                }
            }

            fo.map(e => vd.name -> Variable(vd.tpe, e))
          }
          .map(_.toMap)
    }
  }

  def prepareRoot[Q, M, S](
      executabels: NonEmptyList[QA.ExecutableDefinition[C]],
      schema: SchemaShape[F, Q, M, S],
      operationName: Option[String]
  ): G[RootPrep[F, Q, M, S, C]] = {
    val (ops, frags) = executabels.toList.partitionEither {
      case QA.ExecutableDefinition.Operation(op, c)  => Left((op, c))
      case QA.ExecutableDefinition.Fragment(frag, _) => Right(frag)
    }

    pickRootOperation(ops, operationName).flatMap(prepareOperation(_, frags, schema)).flatMap { case (root, validation) =>
      validation.as(root)
    }
  }

  private def prepareOperation[Q, M, S](
      od: QA.OperationDefinition[C],
      frags: List[QA.FragmentDefinition[C]],
      schema: SchemaShape[F, Q, M, S]
  ): G[(RootPrep[F, Q, M, S, C], G[Unit])] = {
    val (ot, ss) = od match {
      case QA.OperationDefinition.Simple(ss)                => (QA.OperationType.Query, ss)
      case QA.OperationDefinition.Detailed(ot, _, _, _, ss) => (ot, ss)
    }
    variableTypes(od, schema).flatMap { tm =>
      def runWith[A](o: gql.ast.Type[F, A]): G[(Selection[F, A, Stage.Compilation[C]], G[Unit])] = {
        val ap = new ArgParsing[C](tm.fmap { case (t, _) => t })
        val da = new DirectiveAlg[F, C](schema.discover.positions, ap)

        val fragMap = frags.map(x => x.name -> x).toMap
        val fc = new FieldCollection[F, C](
          schema.discover.implementations,
          fragMap,
          ap,
          da
        )
        val fm = new FieldMerging[C]
        val qp = new QueryPreparation[F, C](ap, da, schema.discover.implementations)

        // implicit val AP: ArgParsing[F, C] = ArgParsing[F, C](vm)
        // implicit val DA: DirectiveAlg[F, G, C] = DirectiveAlg.forPositions[F, G, C](schema.discover.positions)
        // val FC: FieldCollection[F, G, C] = FieldCollection[F, G, C](
        //   schema.discover.implementations,
        //   fragMap
        // )
        // val FM = FieldMerging[F, C]
        // val QP = QueryPreparation[F, G, C](vm, schema.discover.implementations)
        val prog = fc.collectSelectionInfo(o, ss).flatMap {
          case x :: xs =>
            val r = NonEmptyList(x, xs)
            fm.checkSelectionsMerge(r) >> qp.prepareSelectable(o, r).map(_ -> fc.validateSelections(r.toList))
          case _ => G.nextId.map(NodeId(_)).map(nid => Selection(nid, Nil, o) -> G.unit)
        }
        prog
      }

      ot match {
        case QA.OperationType.Query =>
          val i: NonEmptyList[(String, gql.ast.Field[F, Unit, ?])] = schema.introspection
          val q = schema.query
          val full = q.copy(fields = i.map { case (k, v) => k -> v.contramap[F, Q](_ => ()) } concatNel q.fields)
          runWith[Q](full).map { case (selection, validation) => RootPrep.Query[F, Q, M, S, C](selection) -> validation }
        case QA.OperationType.Mutation =>
          G.raiseOpt(schema.mutation, "No `Mutation` type defined in this schema.", Nil)
            .flatMap(runWith[M])
            .map { case (selection, validation) => RootPrep.Mutation[F, Q, M, S, C](selection) -> validation }
        case QA.OperationType.Subscription =>
          G.raiseOpt(schema.subscription, "No `Subscription` type defined in this schema.", Nil)
            .flatMap(runWith[S])
            .map { case (selection, validation) => RootPrep.Subscription[F, Q, M, S, C](selection) -> validation }
      }
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
    val G = rp.G
    val (operations, fragments) = executabels.toList.partitionEither {
      case QA.ExecutableDefinition.Operation(op, c)  => Left((op, c))
      case QA.ExecutableDefinition.Fragment(frag, _) => Right(frag)
    }
    rp.pickRootOperation(operations, operationName).runToCompletion(Map.empty).flatMap { operation =>
      val declared = operation match {
        case QA.OperationDefinition.Simple(_) => Set.empty[String]
        case QA.OperationDefinition.Detailed(_, _, definitions, _, _) =>
          definitions.toList.flatMap(_.nel.toList).map(_.name).toSet
      }
      val program = rp
        .prepareOperation(operation, fragments, schema)
        .flatMap {
          case (RootPrep.Query(selection), validation) =>
            G.pause(SubstitueVariables.substSel(selection))
              .flatMap(validation *> _)
              .map(s => PreparedRoot.Query[F, Q, M, S](s): PreparedRoot[F, Q, M, S])
          case (RootPrep.Mutation(selection), validation) =>
            G.pause(SubstitueVariables.substSel(selection))
              .flatMap(validation *> _)
              .map(s => PreparedRoot.Mutation[F, Q, M, S](s): PreparedRoot[F, Q, M, S])
          case (RootPrep.Subscription(selection), validation) =>
            G.pause(SubstitueVariables.substSel(selection))
              .flatMap(validation *> _)
              .map(s => PreparedRoot.Subscription[F, Q, M, S](s): PreparedRoot[F, Q, M, S])
        }
        .flatMap { prepared =>
          G.usedVariables.flatMap { used =>
            val unused = declared -- used
            if (unused.nonEmpty) G.raise(s"Unused variables: ${unused.map(str => s"'$str'").mkString(", ")}", Nil)
            else G.pure(prepared)
          }
        }
      val staged = Alg.eval(program)
      val initial: EitherNec[PositionalError[C], Unit] = staged match {
        case done: Alg.Staged.Done[C, PreparedRoot[F, Q, M, S]] => done.result.void
        case _                                                  => Right(())
      }
      initial.as { (variables: Map[String, Json]) =>
        rp.variables(operation, variables, schema).runToCompletion(Map.empty).flatMap(staged.runToCompletion)
      }
    }
  }

  def prepare[F[_], C, Q, M, S](
      executabels: NonEmptyList[QA.ExecutableDefinition[C]],
      schema: SchemaShape[F, Q, M, S],
      operationName: Option[String]
  ): EitherNec[PositionalError[C], RootQuery[F, Q, M, S, C]] = {
    val rp = new RootPreparation[F, C]
    val fa = rp.prepareRoot(executabels, schema, operationName)
    val RUQ = RunnableQuery
    val RP = RootPrep
    def pq[A](contstruct: Selection[F, A, Stage.Execution] => RunnableQuery[F, Q, M, S, C])(
        sel: Selection[F, A, Stage.Compilation[C]]
    ): PreparedQuery[F, Q, M, S, C] =
      PreparedQuery(vm => Alg.runToCompletion(SubstitueVariables.substSel(sel), vm).map(contstruct))
    def cacheable[A](construct: Selection[F, A, Stage.Execution] => RunnableQuery[F, Q, M, S, C])(
        sel: Selection[F, A, Stage.Compilation[C]]
    ): PreparedQuery[F, Q, M, S, C] =
      pq(construct)(sel)

    Alg.eval(fa) match {
      case done: Alg.Staged.Done[C, RootPrep[F, Q, M, S, C]] =>
        val value = done.result
        value.map { rp =>
          val out = rp match {
            case RP.Query(q)        => cacheable[Q](RUQ.Query(_))(q)
            case RP.Mutation(m)     => cacheable[M](RUQ.Mutation(_))(m)
            case RP.Subscription(s) => cacheable[S](RUQ.Subscription(_))(s)
          }
          RootQuery.Cacheable[F, Q, M, S, C](rp, out)
        }
      case staged: Alg.Staged.Deferred[C, RootPrep[F, Q, M, S, C]] =>
        Right {
          RootQuery.RequiresVariables[F, Q, M, S, C] {
            PreparedQuery[F, Q, M, S, C] { vm =>
              staged.runToCompletion(vm).flatMap {
                case RP.Query(q)        => pq[Q](RUQ.Query(_))(q).run(vm)
                case RP.Mutation(m)     => pq[M](RUQ.Mutation(_))(m).run(vm)
                case RP.Subscription(s) => pq[S](RUQ.Subscription(_))(s).run(vm)
              }
            }
          }
        }
    }
  }

  def prepareRun[F[_], C, Q, M, S](
      executabels: NonEmptyList[QA.ExecutableDefinition[C]],
      schema: SchemaShape[F, Q, M, S],
      variableMap: Map[String, Json],
      operationName: Option[String]
  ): EitherNec[PositionalError[C], PreparedRoot[F, Q, M, S]] = {
    prepareCacheable(executabels, schema, operationName).flatMap(_(variableMap))
  }
}

sealed trait RunnableQuery[F[_], Q, M, S, C]
object RunnableQuery {
  final case class Query[F[_], Q, M, S, C](
      query: Selection[F, Q, Stage.Execution]
  ) extends RunnableQuery[F, Q, M, S, C]
  final case class Mutation[F[_], Q, M, S, C](
      mutation: Selection[F, M, Stage.Execution]
  ) extends RunnableQuery[F, Q, M, S, C]
  final case class Subscription[F[_], Q, M, S, C](
      subscription: Selection[F, S, Stage.Execution]
  ) extends RunnableQuery[F, Q, M, S, C]
}

final case class PreparedQuery[F[_], Q, M, S, C](
    run: VariableMap[C] => EitherNec[PositionalError[C], RunnableQuery[F, Q, M, S, C]]
)

sealed trait RootQuery[F[_], Q, M, S, C]
object RootQuery {
  final case class Cacheable[F[_], Q, M, S, C](
      prep: RootPrep[F, Q, M, S, C],
      query: PreparedQuery[F, Q, M, S, C]
  ) extends RootQuery[F, Q, M, S, C]
  final case class RequiresVariables[F[_], Q, M, S, C](
      run: PreparedQuery[F, Q, M, S, C]
  ) extends RootQuery[F, Q, M, S, C]
}

sealed trait RootPrep[G[_], Q, M, S, C]
object RootPrep {
  final case class Query[G[_], Q, M, S, C](
      query: Selection[G, Q, Stage.Compilation[C]]
  ) extends RootPrep[G, Q, M, S, C]
  final case class Mutation[G[_], Q, M, S, C](
      mutation: Selection[G, M, Stage.Compilation[C]]
  ) extends RootPrep[G, Q, M, S, C]
  final case class Subscription[G[_], Q, M, S, C](
      subscription: Selection[G, S, Stage.Compilation[C]]
  ) extends RootPrep[G, Q, M, S, C]
}

sealed trait PreparedRoot[G[_], Q, M, S]
object PreparedRoot {
  final case class Query[G[_], Q, M, S](query: Selection[G, Q, Stage.Execution]) extends PreparedRoot[G, Q, M, S]
  final case class Mutation[G[_], Q, M, S](mutation: Selection[G, M, Stage.Execution]) extends PreparedRoot[G, Q, M, S]
  final case class Subscription[G[_], Q, M, S](subscription: Selection[G, S, Stage.Execution]) extends PreparedRoot[G, Q, M, S]
}
