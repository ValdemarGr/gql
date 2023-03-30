package gql.preparation

import cats.implicits._
import io.circe._
import cats._
import cats.data._
import gql.parser.{QueryAst => QA, Value => V, AnyValue, Const}
import gql.SchemaShape
import gql.ModifierStack
import gql.InverseModifierStack
import gql.Modifier
import gql.InverseModifier
import cats.mtl.Local
import gql.Cursor
import cats.mtl.Stateful
import java.lang
import scala.collection.immutable
import cats.mtl.Listen

trait RootPreparation[F[_], G[_], P[_]] {
  def pickRootOperation(
      ops: List[P[QA.OperationDefinition[P]]],
      operationName: Option[String]
  ): F[QA.OperationDefinition[P]]

  def variables(
      op: QA.OperationDefinition[P],
      variableMap: Map[String, Json]
  ): F[VariableMap]

  def prepareRoot[Q, M, S](
      executabels: NonEmptyList[QA.ExecutableDefinition[P]],
      schema: SchemaShape[G, Q, M, S],
      variableMap: Map[String, Json],
      operationName: Option[String]
  ): F[PreparedRoot[G, Q, M, S]]
}

object RootPreparation {
  type IdGen[F[_], A] = StateT[F, Int, A]
  type UsedVars[F[_], A] = WriterT[F, ArgParsing.UsedVariables, A]
  type CycleF[F[_], A] = Kleisli[F, FieldCollection.CycleSet, A]
  type CursorF[F[_], A] = Kleisli[F, Cursor, A]
  type ErrF[F[_], C, A] = EitherT[F, NonEmptyChain[PositionalError[C]], A]

  type Stack[C, A] = ErrF[CycleF[CursorF[UsedVars[IdGen[Eval, *], *], *], *], C, A]

  object Stack {
    def runK[C] = new (Stack[C, *] ~> EitherNec[PositionalError[C], *]) {
      override def apply[A](fa: Stack[C, A]): EitherNec[PositionalError[C], A] =
        fa.value.run(Set.empty).run(Cursor.empty).value.runA(0).value
    }
  }

  def prepareRun[G[_], P[_], C, Q, M, S](
      executabels: NonEmptyList[QA.ExecutableDefinition[P]],
      schema: SchemaShape[G, Q, M, S],
      variableMap: Map[String, Json],
      operationName: Option[String]
  )(implicit P: Positioned[P, C]): EitherNec[PositionalError[C], PreparedRoot[G, Q, M, S]] = Stack.runK[C] {
    apply[Stack[C, *], G, P, C].prepareRoot(executabels, schema, variableMap, operationName)
  }

  def apply[F[_]: Parallel, G[_], P[_], C](implicit
      F: MonadError[F, NonEmptyChain[PositionalError[C]]],
      P: Positioned[P, C],
      C: Local[F, Cursor],
      L: Local[F, FieldCollection.CycleSet],
      S: Stateful[F, Int],
      LI: Listen[F, ArgParsing.UsedVariables]
  ) = {
    implicit val EA: ErrorAlg[F, C] = ErrorAlg.errorAlgForHandle[F, NonEmptyChain, C]
    implicit val PA: PathAlg[F] = PathAlg.pathAlgForLocal[F]
    import EA._
    import PA._

    new RootPreparation[F, G, P] {
      override def pickRootOperation(
          ops: List[P[QA.OperationDefinition[P]]],
          operationName: Option[String]
      ): F[QA.OperationDefinition[P]] = {
        lazy val applied = ops.map(P(_))

        lazy val positions = ops.map(P.position(_))

        lazy val possible = applied
          .collect { case d: QA.OperationDefinition.Detailed[P] => d.name }
          .collect { case Some(x) => s"'$x'" }
          .mkString(", ")
        (ops, operationName) match {
          case (Nil, _)      => raise(s"No operations provided.", Nil)
          case (x :: Nil, _) => F.pure(P(x))
          case (_, _) if applied.exists {
                case _: QA.OperationDefinition.Simple[P]                     => true
                case x: QA.OperationDefinition.Detailed[P] if x.name.isEmpty => true
                case _                                                       => false
              } =>
            raise(s"Exactly one operation must be suplied if the operations include at least one unnamed operation.", positions)
          case (_, None) =>
            raise(s"Operation name must be supplied when supplying multiple operations, provided operations are $possible.", positions)
          case (_, Some(name)) =>
            val o = applied.collectFirst { case d: QA.OperationDefinition.Detailed[P] if d.name.contains(name) => d }
            raiseOpt(o, s"Unable to find operation '$name', provided possible operations are $possible.", positions)
        }
      }

      override def variables(
          op: QA.OperationDefinition[P],
          variableMap: Map[String, Json]
      ): F[VariableMap] = {
        val AP = ArgParsing[F, C](Map.empty)
        /*
         * Convert the variable signature into a gql arg and parse both the default value and the provided value
         * Then save the provided getOrElse default into a map along with the type
         */
        op match {
          case QA.OperationDefinition.Simple(_) => F.pure(Map.empty)
          case QA.OperationDefinition.Detailed(_, _, variableDefinitions, _) =>
            variableDefinitions.toList
              .flatMap(_.nel.toList)
              .parTraverse[F, (String, Variable)] { pvd =>
                val pos = P.position(pvd)
                val vd = P(pvd)

                val ms = ModifierStack.fromType(vd.tpe)

                val oe: Option[Either[Json, V[Const]]] = (variableMap.get(vd.name).map(_.asLeft) orElse vd.defaultValue.map(_.asRight))

                val fo: F[Either[Json, V[Const]]] = oe match {
                  case None =>
                    if (ms.invert.modifiers.headOption.contains(InverseModifier.Optional)) F.pure(Right(V.NullValue()))
                    else raise(s"Variable '$$${vd.name}' is required but was not provided.", List(pos))
                  case Some(x) =>
                    val stubTLArg: gql.ast.InToplevel[Unit] = gql.ast.Scalar[Unit](ms.inner, _ => V.NullValue(), _ => Right(()))

                    val t = InverseModifierStack.toIn(ms.copy(inner = stubTLArg).invert)

                    ambientField(vd.name) {
                      t match {
                        case in: gql.ast.In[a] =>
                          val (v, amb) = x match {
                            case Left(j)  => (V.fromJson(j), true)
                            case Right(v) => (v, false)
                          }
                          AP.decodeIn[a](in, v, ambigiousEnum = amb).void
                      }
                    } as x
                }

                fo.map(e => vd.name -> Variable(vd.tpe, e))
              }
              .map(_.toMap)
        }
      }

      override def prepareRoot[Q, M, S](
          executabels: NonEmptyList[QA.ExecutableDefinition[P]],
          schema: SchemaShape[G, Q, M, S],
          variableMap: Map[String, Json],
          operationName: Option[String]
      ): F[PreparedRoot[G, Q, M, S]] = {
        val (ops, frags) = executabels.toList.partitionEither {
          case QA.ExecutableDefinition.Operation(op)  => Left(op)
          case QA.ExecutableDefinition.Fragment(frag) => Right(frag)
        }

        pickRootOperation(ops, operationName).flatMap { od =>
          variables(od, variableMap)
          val (ot, ss) = od match {
            case QA.OperationDefinition.Simple(ss)             => (QA.OperationType.Query, ss)
            case QA.OperationDefinition.Detailed(ot, _, _, ss) => (ot, ss)
          }

          def runWith[A](o: gql.ast.Type[G, A]): F[NonEmptyList[PreparedSpecification[G, A, _]]] =
            variables(od, variableMap).flatMap { vm =>
              implicit val AP: ArgParsing[F] = ArgParsing[F, C](vm)
              val fragMap = frags.map(x => P(x).name -> x).toMap
              val FC: FieldCollection[F, G, P, C] = FieldCollection[F, G, P, C](schema.discover.implementations, fragMap)
              val FM = FieldMerging[F, C]
              val QP = QueryPreparation[F, G, C](vm, schema.discover.implementations)
              val prog = FC.collectSelectionInfo(o, ss).flatMap { root =>
                FM.checkSelectionsMerge(root) >> QP.prepareSelectable(o, root)
              }
              LI.listen(prog).flatMap { case (res, used) =>
                val unused = vm.keySet -- used
                if (unused.nonEmpty) raise(s"Unused variables: ${unused.map(str => s"'$str'").mkString(", ")}", Nil)
                else F.pure(res)
              }
            }

          ot match {
            case QA.OperationType.Query =>
              val i: NonEmptyList[(String, gql.ast.Field[G, Unit, ?])] = schema.introspection
              val q = schema.query
              val full = q.copy(fields = i.map { case (k, v) => k -> v.contramap[G, Q](_ => ()) } concatNel q.fields)
              runWith[Q](full).map(PreparedRoot.Query(_))
            case QA.OperationType.Mutation =>
              raiseOpt(schema.mutation, "No `Mutation` type defined in this schema.", Nil)
                .flatMap(runWith[M])
                .map(PreparedRoot.Mutation(_))
            case QA.OperationType.Subscription =>
              raiseOpt(schema.subscription, "No `Subscription` type defined in this schema.", Nil)
                .flatMap(runWith[S])
                .map(PreparedRoot.Subscription(_))
          }
        }
      }
    }
  }
}

sealed trait PreparedRoot[G[_], Q, M, S]
object PreparedRoot {
  final case class Query[G[_], Q, M, S](query: NonEmptyList[PreparedField[G, Q]]) extends PreparedRoot[G, Q, M, S]
  final case class Mutation[G[_], Q, M, S](mutation: NonEmptyList[PreparedField[G, M]]) extends PreparedRoot[G, Q, M, S]
  final case class Subscription[G[_], Q, M, S](subscription: NonEmptyList[PreparedField[G, S]]) extends PreparedRoot[G, Q, M, S]
}
