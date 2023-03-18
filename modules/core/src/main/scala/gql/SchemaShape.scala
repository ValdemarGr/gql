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
package gql

import cats._
import cats.implicits._
import cats.mtl._
import cats.data._
import gql.ast._
import org.typelevel.paiges.Doc
import gql.parser.{Value => V, AnyValue}

final case class SchemaShape[F[_], Q, M, S](
    query: Type[F, Q],
    mutation: Option[Type[F, M]] = Option.empty[Type[F, Unit]],
    subscription: Option[Type[F, S]] = Option.empty[Type[F, Unit]],
    outputTypes: List[OutToplevel[F, ?]] = Nil,
    inputTypes: List[InToplevel[?]] = Nil
) {
  def addOutputTypes(t: OutToplevel[F, ?]*): SchemaShape[F, Q, M, S] =
    copy(outputTypes = t.toList ++ outputTypes)

  def addInputTypes(t: InToplevel[?]*): SchemaShape[F, Q, M, S] =
    copy(inputTypes = t.toList ++ inputTypes)

  lazy val discover = SchemaShape.discover[F](this)

  lazy val validate = Validation.validate[F](this)

  lazy val render = SchemaShape.render[F](this)

  lazy val introspection = SchemaShape.introspect[F](this)
}

object SchemaShape {
  final class PartiallyAppliedSchemaShape[F[_]](val dummy: Boolean = false) extends AnyVal {
    def apply[Q, M, S](
        query: Type[F, Q],
        mutation: Option[Type[F, M]] = Option.empty[Type[F, Unit]],
        subscription: Option[Type[F, S]] = Option.empty[Type[F, Unit]],
        outputTypes: List[OutToplevel[F, ?]] = Nil,
        inputTypes: List[InToplevel[?]] = Nil
    ): SchemaShape[F, Q, M, S] =
      SchemaShape(query, mutation, subscription, outputTypes, inputTypes)
  }

  def make[F[_]] = new PartiallyAppliedSchemaShape[F]

  type Specify[A, B] = A => Option[B]
  sealed trait InterfaceImpl[+F[_], A]
  object InterfaceImpl {
    final case class OtherInterface[F[_], A](i: Interface[F, A]) extends InterfaceImpl[F, A]
    final case class TypeImpl[F[_], A, B](t: Type[F, B], specify: A => Option[B]) extends InterfaceImpl[F, A]
  }

  // Key is the interface
  // Values:
  //   Key is the typename of the object
  //   Values:
  //     1. The object like type that extends the interface
  //     2. The function to map from the interface to the object)
  //
  // There is no implicit interface implementations; all transitive implementations should be explicit
  // (https://spec.graphql.org/draft/#IsValidImplementation())
  type Implementations[F[_]] = Map[String, Map[String, InterfaceImpl[F, ?]]]

  final case class DiscoveryState[F[_]](
      inputs: Map[String, InToplevel[?]],
      outputs: Map[String, OutToplevel[F, ?]],
      implementations: Implementations[F]
  )

  def discover[F[_]](shape: SchemaShape[F, ?, ?, ?]): DiscoveryState[F] = {
    def inputNotSeen[G[_], A](
        tl: InToplevel[?]
    )(ga: G[A])(implicit G: Monad[G], S: Stateful[G, DiscoveryState[F]], M: Monoid[A]): G[A] =
      S.get.flatMap { s =>
        if (s.inputs.contains(tl.name)) G.pure(M.empty)
        else S.modify(_.copy(inputs = s.inputs + (tl.name -> tl))) *> ga
      }

    def outputNotSeen[G[_], A](
        tl: OutToplevel[F, ?]
    )(ga: G[A])(implicit G: Monad[G], S: Stateful[G, DiscoveryState[F]], M: Monoid[A]): G[A] =
      S.get.flatMap { s =>
        if (s.outputs.contains(tl.name)) G.pure(M.empty)
        else S.modify(_.copy(outputs = s.outputs + (tl.name -> tl))) *> ga
      }

    type InterfaceName = String
    def addValues[G[_]](values: List[(InterfaceName, InterfaceImpl[F, ?])])(implicit S: Stateful[G, DiscoveryState[F]]): G[Unit] = {
      S.modify { s =>
        val withNew = values.foldLeft(s.implementations) { case (accum, (interfaceName, next)) =>
          val name = next match {
            case InterfaceImpl.OtherInterface(i) => i.name
            case InterfaceImpl.TypeImpl(t, _)    => t.name
          }
          val entry = name -> next
          accum.get(interfaceName) match {
            case None    => accum + (interfaceName -> Map(entry))
            case Some(m) => accum + (interfaceName -> (m + entry))
          }
        }

        s.copy(implementations = withNew)
      }
    }

    def goOutput[G[_]](out: Out[F, ?])(implicit G: Monad[G], S: Stateful[G, DiscoveryState[F]]): G[Unit] =
      out match {
        case o: OutArr[?, ?, ?, ?] => goOutput[G](o.of)
        case o: OutOpt[?, ?, ?]    => goOutput[G](o.of)
        case t: OutToplevel[F, ?] =>
          outputNotSeen(t) {
            def handleFields(o: ObjectLike[F, ?]): G[Unit] =
              o.abstractFields.traverse_ { case (_, x) =>
                goOutput[G](x.output.value) >>
                  x.arg.traverse_(_.entries.traverse_(x => goInput[G](x.input.value)))
              }

            t match {
              case ol: ObjectLike[F, ?] =>
                val values: List[(Interface[F, ?], InterfaceImpl[F, ?])] = ol match {
                  case t: Type[F, a] =>
                    t.implementations.map(x => (x.implementation.value -> InterfaceImpl.TypeImpl(t, x.specify)))
                  case i: Interface[F, ?] =>
                    i.implementations.map(x => (x.value -> InterfaceImpl.OtherInterface(i)))
                }
                addValues[G](values.map { case (i, ii) => i.name -> ii }) >>
                  handleFields(ol) >>
                  values.traverse_ { case (i, _) => goOutput[G](i) }
              case Union(_, instances, _) =>
                instances.toList.traverse_(inst => goOutput[G](inst.tpe.value))
              case _ => G.unit
            }
          }
      }

    def goInput[G[_]](inp: In[?])(implicit G: Monad[G], S: Stateful[G, DiscoveryState[F]]): G[Unit] =
      inp match {
        case InArr(of, _) => goInput[G](of)
        case InOpt(of)    => goInput[G](of)
        case t: InToplevel[?] =>
          inputNotSeen(t) {
            t match {
              case Input(_, fields, _) =>
                fields.entries.traverse_(x => goInput[G](x.input.value))
              case _ => G.unit
            }
          }
      }

    val outs = (shape.query :: (shape.mutation ++ shape.subscription).toList ++ shape.outputTypes)
      .traverse_(goOutput[State[DiscoveryState[F], *]])

    val ins = shape.inputTypes.traverse_(goInput[State[DiscoveryState[F], *]])

    (ins, outs).tupled
      .runS(DiscoveryState(Map.empty, Map.empty, Map.empty))
      .value
  }

  def renderValueDoc(v: V[AnyValue]): Doc = {
    import V._
    v match {
      case IntValue(v)     => Doc.text(v.toString)
      case StringValue(v)  => Doc.text(s""""$v"""")
      case FloatValue(v)   => Doc.text(v.toString)
      case NullValue()     => Doc.text("null")
      case BooleanValue(v) => Doc.text(v.toString)
      case ListValue(v) =>
        Doc.intercalate(Doc.comma + Doc.line, v.map(renderValueDoc)).tightBracketBy(Doc.char('['), Doc.char(']'))
      case ObjectValue(fields) =>
        Doc
          .intercalate(
            Doc.comma + Doc.line,
            fields.map { case (k, v) => Doc.text(k) + Doc.text(": ") + renderValueDoc(v) }
          )
          .bracketBy(Doc.char('{'), Doc.char('}'))
      case EnumValue(v)     => Doc.text(v)
      case VariableValue(v) => Doc.text(v)
    }
  }
  def render[F[_]](shape: SchemaShape[F, ?, ?, ?]) = {
    lazy val triple = Doc.text("\"\"\"")

    def doc(d: Option[String]) =
      d match {
        case None => Doc.empty
        case Some(x) =>
          val o =
            if (x.contains("\n")) {
              triple + Doc.hardLine + Doc.text(x) + Doc.hardLine + triple
            } else {
              Doc.text("\"") + Doc.text(x) + Doc.text("\"")
            }
          o + Doc.hardLine
      }

    def renderModifierStack(ms: ModifierStack[Toplevel[?]]) =
      ms.modifiers.foldLeft(Doc.text(ms.inner.name)) {
        case (accum, Modifier.List)    => accum.tightBracketBy(Doc.char('['), Doc.char(']'))
        case (accum, Modifier.NonNull) => accum + Doc.char('!')
      }

    def renderArgValueDoc(av: ArgValue[?]): Doc = {
      val o = av.defaultValue.map(dv => Doc.text(" = ") + renderValueDoc(dv)).getOrElse(Doc.empty)
      doc(av.description) +
        Doc.text(av.name) + Doc.text(": ") + renderModifierStack(ModifierStack.fromIn(av.input.value)) + o
    }

    def renderFieldDoc[G[_]](name: String, field: AbstractField[G, ?]): Doc = {
      val args = field.arg
        .map(_.entries)
        .map(nec =>
          Doc.intercalate(Doc.comma + Doc.lineOrSpace, nec.toList.map(renderArgValueDoc)).tightBracketBy(Doc.char('('), Doc.char(')'))
        )
        .getOrElse(Doc.empty)

      doc(field.description) +
        Doc.text(name) + args + Doc.text(": ") + renderModifierStack(ModifierStack.fromOut(field.output.value))
    }

    lazy val discovery: DiscoveryState[F] = shape.discover

    lazy val all = discovery.inputs ++ discovery.outputs

    lazy val exclusion = Set("String", "Int", "Float", "ID", "Boolean")

    val docs =
      all.values.toList
        .filterNot(x => exclusion.contains(x.name))
        .map { tl =>
          tl match {
            case e: Enum[?] =>
              doc(e.description) +
                Doc.text(s"enum ${e.name} {") + Doc.hardLine +
                Doc.intercalate(
                  Doc.hardLine,
                  e.mappings.toList.map { case (name, value) => doc(value.description) + Doc.text(name) }
                ) +
                Doc.hardLine + Doc.text("}")
            case Input(name, fields, desc) =>
              doc(desc) +
                Doc.text(s"input $name") + (Doc.text(" {") + Doc.hardLine + Doc
                  .intercalate(Doc.hardLine, fields.entries.toList.map(renderArgValueDoc))
                  .indent(2) + Doc.hardLine + Doc.text("}"))
            // Dont render built-in scalars
            case Scalar(name, _, _, desc) => doc(desc) + Doc.text(s"scalar $name")
            case ol @ Interface(name, fields, _, desc) =>
              val fieldsDoc = Doc
                .intercalate(
                  Doc.hardLine,
                  fields.toList.map { case (name, field) => renderFieldDoc(name, field) }
                )
                .indent(2)

              val interfaces = ol.implementsMap.keySet.toList.toNel
                .map { nel => Doc.text(" implements ") + Doc.intercalate(Doc.text(" & "), nel.toList.map(Doc.text)) }
                .getOrElse(Doc.empty)

              doc(desc) +
                (Doc.text(s"interface $name") + interfaces + Doc.text(" {") + Doc.hardLine +
                  fieldsDoc +
                  Doc.hardLine + Doc.text("}"))
            case ol @ Type(name, fields, _, desc) =>
              val fieldsDoc = Doc
                .intercalate(
                  Doc.hardLine,
                  fields.toList.map { case (name, field) => renderFieldDoc(name, field.asAbstract) }
                )
                .indent(2)

              val interfaces = ol.implementsMap.keySet.toList.toNel
                .map { nel => Doc.text(" implements ") + Doc.intercalate(Doc.text(" & "), nel.toList.map(Doc.text)) }
                .getOrElse(Doc.empty)

              doc(desc) +
                Doc.text(s"type $name") + interfaces + (Doc.text(" {") + Doc.hardLine +
                  fieldsDoc +
                  Doc.hardLine + Doc.text("}"))
            case Union(name, types, desc) =>
              val names = types.toList.map(x => Doc.text(x.tpe.value.name))
              val xs =
                if (names.size <= 3) Doc.intercalate(Doc.text(" | "), names)
                else Doc.hardLine + Doc.intercalate(Doc.hardLine, names.map(d => Doc.text("| ").indent(2) + d))

              doc(desc) + (Doc.text(s"union $name = ") + xs)
          }
        }

    Doc.intercalate(Doc.hardLine + Doc.hardLine, docs).render(80)
  }

  sealed trait __TypeKind extends Product with Serializable
  object __TypeKind {
    case object SCALAR extends __TypeKind
    case object OBJECT extends __TypeKind
    case object INTERFACE extends __TypeKind
    case object UNION extends __TypeKind
    case object ENUM extends __TypeKind
    case object INPUT_OBJECT extends __TypeKind
    case object LIST extends __TypeKind
    case object NON_NULL extends __TypeKind
  }

  def introspect[F[_]](ss: SchemaShape[F, ?, ?, ?]): NonEmptyList[(String, Field[F, Unit, ?])] = {
    import dsl._

    // We do a little lazy evaluation trick to include the introspection schema in itself
    lazy val d = {
      val ds = ss.discover
      val introspectionDiscovery = discover[F](
        SchemaShape.make[F](
          tpe[F, Unit](
            "Query",
            rootFields.head,
            rootFields.tail: _*
          )
        )
      )
      // Omit Query
      val withoutQuery = introspectionDiscovery.copy(outputs = introspectionDiscovery.outputs - "Query")
      val out = DiscoveryState[F](
        ds.inputs ++ withoutQuery.inputs,
        ds.outputs ++ withoutQuery.outputs,
        ds.implementations ++ withoutQuery.implementations
      )
      // Omit duplicate types
      out.copy(inputs = out.inputs -- ds.outputs.keySet)
    }

    implicit lazy val __typeKind: Enum[__TypeKind] = enumType[__TypeKind](
      "__TypeKind",
      "SCALAR" -> enumVal(__TypeKind.SCALAR),
      "OBJECT" -> enumVal(__TypeKind.OBJECT),
      "INTERFACE" -> enumVal(__TypeKind.INTERFACE),
      "UNION" -> enumVal(__TypeKind.UNION),
      "ENUM" -> enumVal(__TypeKind.ENUM),
      "INPUT_OBJECT" -> enumVal(__TypeKind.INPUT_OBJECT),
      "LIST" -> enumVal(__TypeKind.LIST),
      "NON_NULL" -> enumVal(__TypeKind.NON_NULL)
    )

    implicit lazy val __inputValue: Type[F, ArgValue[?]] = tpe[F, ArgValue[?]](
      "__InputValue",
      "name" -> lift(_.name),
      "description" -> lift(_.description),
      "type" -> lift(x => TypeInfo.fromInput(x.input.value)),
      "defaultValue" -> lift(x => x.defaultValue.map(renderValueDoc(_).render(80))),
      "isDeprecated" -> lift(_ => false),
      "deprecationReason" -> lift(_ => Option.empty[String])
    )

    final case class NamedField(
        name: String,
        field: AbstractField[F, ?]
    )

    def inclDeprecated = arg[Boolean]("includeDeprecated", value.scalar(false))

    implicit lazy val namedField: Type[F, NamedField] = tpe[F, NamedField](
      "__Field",
      "name" -> lift(_.name),
      "description" -> lift(_.field.description),
      "args" -> lift(inclDeprecated)((_, x) => x.field.arg.toList.flatMap(_.entries.toList)),
      "type" -> lift(x => TypeInfo.fromOutput(x.field.output.value)),
      "isDeprecated" -> lift(_ => false),
      "deprecationReason" -> lift(_ => Option.empty[String])
    )

    sealed trait TypeInfo extends Product with Serializable {
      def asToplevel: Option[Toplevel[?]]
      def next: Option[TypeInfo]
    }
    sealed trait InnerTypeInfo extends TypeInfo {
      def next: Option[TypeInfo] = None
    }
    object TypeInfo {
      final case class OutInfo(t: OutToplevel[F, ?]) extends InnerTypeInfo {
        def asToplevel: Option[Toplevel[?]] = Some(t)
      }
      final case class InInfo(t: InToplevel[?]) extends InnerTypeInfo {
        def asToplevel: Option[Toplevel[?]] = Some(t)
      }

      final case class NEModifierStack(modifiers: NonEmptyList[Modifier], inner: InnerTypeInfo) extends TypeInfo {
        def asToplevel = None
        def head = modifiers.head
        def next = Some {
          modifiers.tail match {
            case Nil    => inner
            case h :: t => NEModifierStack(NonEmptyList(h, t), inner)
          }
        }
      }

      def fromOutput(o: Out[F, ?]): TypeInfo = {
        val ms = ModifierStack.fromOut[F](o)
        ms.modifiers match {
          case Nil    => OutInfo(ms.inner)
          case h :: t => NEModifierStack(NonEmptyList(h, t), OutInfo(ms.inner))
        }
      }

      def fromInput(i: In[?]): TypeInfo = {
        val ms = ModifierStack.fromIn(i)
        ms.modifiers match {
          case Nil    => InInfo(ms.inner)
          case h :: t => NEModifierStack(NonEmptyList(h, t), InInfo(ms.inner))
        }
      }
    }

    implicit lazy val __type: Type[F, TypeInfo] = tpe[F, TypeInfo](
      "__Type",
      "kind" -> lift {
        case m: TypeInfo.NEModifierStack =>
          m.head match {
            case Modifier.List    => __TypeKind.LIST
            case Modifier.NonNull => __TypeKind.NON_NULL
          }
        case oi: TypeInfo.OutInfo =>
          oi.t match {
            case _: Scalar[?]       => __TypeKind.SCALAR
            case _: Enum[?]         => __TypeKind.ENUM
            case _: Type[?, ?]      => __TypeKind.OBJECT
            case _: Interface[?, ?] => __TypeKind.INTERFACE
            case _: Union[?, ?]     => __TypeKind.UNION
          }
        case ii: TypeInfo.InInfo =>
          ii.t match {
            case Scalar(_, _, _, _) => __TypeKind.SCALAR
            case Enum(_, _, _)      => __TypeKind.ENUM
            case _: Input[?]        => __TypeKind.INPUT_OBJECT
          }
      },
      "name" -> lift(_.asToplevel.map(_.name)),
      "description" -> lift(_.asToplevel.flatMap(_.description)),
      "fields" -> lift(inclDeprecated) {
        case (_, oi: TypeInfo.OutInfo) =>
          oi.t match {
            case Type(_, fields, _, _)      => Some(fields.toList.map { case (k, v) => NamedField(k, v.asAbstract) })
            case Interface(_, fields, _, _) => Some(fields.toList.map { case (k, v) => NamedField(k, v) })
            case _                          => None
          }
        case _ => None
      },
      "interfaces" -> lift {
        case oi: TypeInfo.OutInfo =>
          oi.t match {
            case Type(_, _, impls, _)      => impls.map[TypeInfo](impl => TypeInfo.OutInfo(impl.implementation.value)).some
            case Interface(_, _, impls, _) => impls.map[TypeInfo](impl => TypeInfo.OutInfo(impl.value)).some
            case _                         => None
          }
        case _ => None
      },
      "possibleTypes" -> lift {
        case oi: TypeInfo.OutInfo =>
          oi.t match {
            case Interface(name, _, _, _) =>
              d.implementations
                .get(name)
                .toList
                .flatMap(_.values.toList)
                .map {
                  case InterfaceImpl.TypeImpl(ol, _)   => TypeInfo.OutInfo(ol)
                  case InterfaceImpl.OtherInterface(i) => TypeInfo.OutInfo(i)
                }
                .some
            case Union(_, instances, _) => instances.toList.map[TypeInfo](x => TypeInfo.OutInfo(x.tpe.value)).some
            case _                      => None
          }
        case _ => None
      },
      "enumValues" -> lift(inclDeprecated) { case (_, ti) =>
        ti.asToplevel.collect { case Enum(_, m, _) => m.toList.map { case (k, v) => NamedEnumValue(k, v) } }
      },
      "inputFields" -> lift(inclDeprecated) {
        case (_, ii: TypeInfo.InInfo) =>
          ii.t match {
            case Input(_, fields, _) => Some(fields.entries.toList)
            case _                   => None
          }
        case _ => None
      },
      "ofType" -> lift(_.next)
    )

    final case class NamedEnumValue(
        name: String,
        value: EnumValue[?]
    )
    implicit lazy val enumValue: Type[F, NamedEnumValue] = tpe[F, NamedEnumValue](
      "__EnumValue",
      "name" -> lift(_.name),
      "description" -> lift(_.value.description),
      "isDeprecated" -> lift(_ => false),
      "deprecationReason" -> lift(_ => Option.empty[String])
    )

    sealed trait DirectiveLocation
    object DirectiveLocation {
      case object QUERY extends DirectiveLocation
      case object MUTATION extends DirectiveLocation
      case object SUBSCRIPTION extends DirectiveLocation
      case object FIELD extends DirectiveLocation
      case object FRAGMENT_DEFINITION extends DirectiveLocation
      case object FRAGMENT_SPREAD extends DirectiveLocation
      case object INLINE_FRAGMENT extends DirectiveLocation
      case object VARIABLE_DEFINITION extends DirectiveLocation
      case object SCHEMA extends DirectiveLocation
      case object SCALAR extends DirectiveLocation
      case object OBJECT extends DirectiveLocation
      case object FIELD_DEFINITION extends DirectiveLocation
      case object ARGUMENT_DEFINITION extends DirectiveLocation
      case object INTERFACE extends DirectiveLocation
      case object UNION extends DirectiveLocation
      case object ENUM extends DirectiveLocation
      case object ENUM_VALUE extends DirectiveLocation
      case object INPUT_OBJECT extends DirectiveLocation
      case object INPUT_FIELD_DEFINITION extends DirectiveLocation
    }

    implicit lazy val directiveLocation: Enum[DirectiveLocation] = enumType[DirectiveLocation](
      "__DirectiveLocation",
      "QUERY" -> enumVal(DirectiveLocation.QUERY),
      "MUTATION" -> enumVal(DirectiveLocation.MUTATION),
      "SUBSCRIPTION" -> enumVal(DirectiveLocation.SUBSCRIPTION),
      "FIELD" -> enumVal(DirectiveLocation.FIELD),
      "FRAGMENT_DEFINITION" -> enumVal(DirectiveLocation.FRAGMENT_DEFINITION),
      "FRAGMENT_SPREAD" -> enumVal(DirectiveLocation.FRAGMENT_SPREAD),
      "INLINE_FRAGMENT" -> enumVal(DirectiveLocation.INLINE_FRAGMENT),
      "VARIABLE_DEFINITION" -> enumVal(DirectiveLocation.VARIABLE_DEFINITION),
      "SCHEMA" -> enumVal(DirectiveLocation.SCHEMA),
      "SCALAR" -> enumVal(DirectiveLocation.SCALAR),
      "OBJECT" -> enumVal(DirectiveLocation.OBJECT),
      "FIELD_DEFINITION" -> enumVal(DirectiveLocation.FIELD_DEFINITION),
      "ARGUMENT_DEFINITION" -> enumVal(DirectiveLocation.ARGUMENT_DEFINITION),
      "INTERFACE" -> enumVal(DirectiveLocation.INTERFACE),
      "UNION" -> enumVal(DirectiveLocation.UNION),
      "ENUM" -> enumVal(DirectiveLocation.ENUM),
      "ENUM_VALUE" -> enumVal(DirectiveLocation.ENUM_VALUE),
      "INPUT_OBJECT" -> enumVal(DirectiveLocation.INPUT_OBJECT),
      "INPUT_FIELD_DEFINITION" -> enumVal(DirectiveLocation.INPUT_FIELD_DEFINITION)
    )

    case object PhantomDirective
    implicit lazy val directive: Type[F, PhantomDirective.type] = tpe[F, PhantomDirective.type](
      "__Directive",
      "name" -> lift(_ => ""),
      "description" -> lift(_ => Option.empty[String]),
      "locations" -> lift(_ => List.empty[DirectiveLocation]),
      "args" -> lift(inclDeprecated)((_, _) => List.empty[ArgValue[?]]),
      "isRepeatable" -> lift(_ => false)
    )

    case object PhantomSchema
    implicit lazy val schema: Type[F, PhantomSchema.type] = tpe[F, PhantomSchema.type](
      "__Schema",
      "description" -> lift(_ => Option.empty[String]),
      "types" -> lift { _ =>
        val outs = d.outputs.values.toList.map(TypeInfo.OutInfo(_))
        val ins = d.inputs.values.toList.map(TypeInfo.InInfo(_))
        (outs ++ ins): List[TypeInfo]
      },
      "queryType" -> lift(_ => TypeInfo.OutInfo(ss.query): TypeInfo),
      "mutationType" -> lift(_ => ss.mutation.map[TypeInfo](TypeInfo.OutInfo(_))),
      "subscriptionType" -> lift(_ => ss.subscription.map[TypeInfo](TypeInfo.OutInfo(_))),
      "directives" -> lift(_ => List.empty[PhantomDirective.type])
    )

    lazy val rootFields: NonEmptyList[(String, Field[F, Unit, ?])] =
      fields(
        "__schema" -> lift(_ => PhantomSchema),
        "__type" -> lift(arg[String]("name")) { case (name, _) =>
          d.inputs
            .get(name)
            .map[TypeInfo](TypeInfo.InInfo(_))
            .orElse(d.outputs.get(name).map[TypeInfo](TypeInfo.OutInfo(_)))
        }
      )

    rootFields
  }
}
