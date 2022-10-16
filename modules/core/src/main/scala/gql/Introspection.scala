package gql

import cats._
import cats.implicits._
import gql.dsl._
import gql.ast._
import cats.Eval
import cats.Defer
import cats.data.NonEmptyList
import cats.data.Chain

object Introspection {
  sealed trait __TypeKind
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

  def fromSchemaShape[F[_]](ss: SchemaShape[F, _, _, _]): NonEmptyList[(String, Field[Id, Unit, _, _])] = {
    val d = ss.discover

    implicit lazy val __typeKind = enum[Id, __TypeKind](
      "__TypeKind",
      enumInst("SCALAR", __TypeKind.SCALAR),
      enumInst("OBJECT", __TypeKind.OBJECT),
      enumInst("INTERFACE", __TypeKind.INTERFACE),
      enumInst("UNION", __TypeKind.UNION),
      enumInst("ENUM", __TypeKind.ENUM),
      enumInst("INPUT_OBJECT", __TypeKind.INPUT_OBJECT),
      enumInst("LIST", __TypeKind.LIST),
      enumInst("NON_NULL", __TypeKind.NON_NULL)
    )

    implicit lazy val __inputValue: Type[Id, ArgValue[_]] = tpe[Id, ArgValue[_]](
      "__InputValue",
      "name" -> pure(_.name),
      "description" -> pure(_.description),
      "type" -> pure(x => (TypeInfo.InInfo(x.input.value): TypeInfo)),
      "isDeprecated" -> pure(_ => false),
      "deprecationReason" -> pure(_ => Option.empty[String])
    )

    final case class NamedField(
        name: String,
        field: Field[F, _, _, _]
    )

    implicit lazy val namedField = tpe[Id, NamedField](
      "__Field",
      "name" -> pure(_.name),
      "description" -> pure(_.field.description),
      "args" -> pure(_.field.args.entries.toList),
      "type" -> pure(x => (TypeInfo.OutInfo(x.field.output.value): TypeInfo)),
      "isDeprecated" -> pure(_ => false),
      "deprecationReason" -> pure(_ => Option.empty[String])
    )

    sealed trait TypeInfo {
      def asToplevel: Option[Toplevel[_]]
    }
    object TypeInfo {
      // TODO unify this and the schema shape modifier stack code
      final case class OutInfo(t: Out[F, _]) extends TypeInfo {
        lazy val inner: OutToplevel[F, _] = {
          val (ot, _) = partition
          ot
        }

        override lazy val asToplevel: Option[Toplevel[_]] = Some(inner)

        lazy val partition: (OutToplevel[F, _], Option[ModifierStack]) = {
          def go(t: Out[F, _], inOption: Boolean = false): (OutToplevel[F, _], Chain[Modifier]) = {
            val suffix = if (inOption) Chain.empty else Chain(Modifier.NonNull)
            t match {
              case t: OutToplevel[F, _] => (t, suffix)
              case OutArr(x) =>
                val (t, stack) = go(x, inOption = false)
                (t, stack append Modifier.List concat suffix)
              case OutOpt(x) =>
                val (t, stack) = go(x, inOption = true)
                (t, stack append Modifier.NonNull)
            }
          }
          val (ot, stack) = go(t)
          (ot, stack.toList.toNel.map(ModifierStack(_)))
        }
      }
      final case class InInfo(t: In[_]) extends TypeInfo {
        lazy val inner: InToplevel[_] = {
          val (ot, _) = partition
          ot
        }

        override lazy val asToplevel: Option[Toplevel[_]] = Some(inner)

        lazy val partition: (InToplevel[_], Option[ModifierStack]) = {
          def go(t: In[_], inOption: Boolean = false): (InToplevel[_], Chain[Modifier]) = {
            val suffix = if (inOption) Chain.empty else Chain(Modifier.NonNull)
            t match {
              case t: InToplevel[_] => (t, suffix)
              case InArr(x) =>
                val (t, stack) = go(x, inOption = false)
                (t, stack append Modifier.List concat suffix)
              case InOpt(x) =>
                val (t, stack) = go(x, inOption = true)
                (t, stack append Modifier.NonNull)
            }
          }
          val (ot, stack) = go(t)
          (ot, stack.toList.toNel.map(ModifierStack(_)))
        }
      }

      sealed trait Modifier
      object Modifier {
        case object List extends Modifier
        case object NonNull extends Modifier
      }
      final case class ModifierStack(t: NonEmptyList[Modifier]) extends TypeInfo {
        override val asToplevel = None
      }
    }
    def inclDeprecated = arg[Boolean]("includeDeprecated", false)

    implicit lazy val __type: Type[Id, TypeInfo] = tpe[Id, TypeInfo](
      "__Type",
      "kind" -> pure[Id, TypeInfo, __TypeKind] {
        case TypeInfo.ModifierStack(x) =>
          x.head match {
            case TypeInfo.Modifier.List    => __TypeKind.LIST
            case TypeInfo.Modifier.NonNull => __TypeKind.NON_NULL
          }
        case oi: TypeInfo.OutInfo =>
          oi.inner match {
            case _: Scalar[F, _]    => __TypeKind.SCALAR
            case _: Enum[F, _]      => __TypeKind.ENUM
            case _: Type[F, _]      => __TypeKind.OBJECT
            case _: Interface[F, _] => __TypeKind.INTERFACE
            case _: Union[F, _]     => __TypeKind.UNION
          }
        case ii: TypeInfo.InInfo =>
          ii.inner match {
            case Scalar(_, _, _, _) => __TypeKind.SCALAR
            case Enum(_, _, _)      => __TypeKind.ENUM
            case _: Input[_]        => __TypeKind.INPUT_OBJECT
          }
      },
      "name" -> pure(_.asToplevel.map(_.name)),
      "description" -> pure(_.asToplevel.map(_.description)),
      "fields" -> pure(inclDeprecated) {
        case (oi: TypeInfo.OutInfo, _) =>
          oi.inner match {
            case Type(_, fields, _)         => Some(fields.toList.map { case (k, v) => NamedField(k, v) })
            case Interface(_, _, fields, _) => Some(fields.toList.map { case (k, v) => NamedField(k, v) })
            case _                          => None
          }
        case _ => None
      },
      "interfaces" -> pure {
        case oi: TypeInfo.OutInfo =>
          oi.inner match {
            case Type(name, _, _) =>
              val interfaces = d.interfaceImplementations.get(name).toList.flatMap(_.toList)
              interfaces
                .map(d.outputs.apply)
                .collect[TypeInfo] {
                  case x: Interface[F, _] => TypeInfo.OutInfo(x)
                  case x: Type[F, _]      => TypeInfo.OutInfo(x)
                }
                .some
            case Interface(name, _, _, _) =>
              d.interfaceImplementations.get(name)
              val interfaces = d.interfaceImplementations.get(name).toList.flatMap(_.toList)
              interfaces
                .map(d.outputs.apply)
                .collect[TypeInfo] {
                  case x: Interface[F, _] => TypeInfo.OutInfo(x)
                  case x: Type[F, _]      => TypeInfo.OutInfo(x)
                }
                .some
            case _ => None
          }
        case _ => None
      },
      "possibleTypes" -> pure {
        case oi: TypeInfo.OutInfo =>
          oi.inner match {
            case Interface(_, instances, _, _) => instances.map[TypeInfo](x => TypeInfo.OutInfo(x.ol.value)).some
            case Union(_, instances, _)        => instances.toList.map[TypeInfo](x => TypeInfo.OutInfo(x.ol.value)).some
            case _                             => None
          }
        case _ => None
      },
      "enumValues" -> pure(inclDeprecated) { case (ti, _) =>
        ti.asToplevel.collect { case Enum(_, m, _) => m.toList }
      },
      "inputFields" -> pure {
        case ii: TypeInfo.InInfo =>
          ii.inner match {
            case Input(_, fields, _) => Some(fields.entries.toList)
            case _                   => None
          }
        case _ => None
      },
      "ofType" -> pure {
        case TypeInfo.ModifierStack(NonEmptyList(_, tl)) =>
          tl.toNel.map[TypeInfo](TypeInfo.ModifierStack(_))
        case _ => None
      }
    )

    implicit lazy val enumValue: Type[Id, EnumInstance[_]] = tpe[Id, EnumInstance[_]](
      "__EnumValue",
      "name" -> pure(_.encodedName),
      "description" -> pure(_.description),
      "isDeprecated" -> pure(_ => false),
      "deprecationReason" -> pure(_ => Option.empty[String])
    )

    case object PhantomSchema
    implicit lazy val schema: Type[Id, PhantomSchema.type] = tpe[Id, PhantomSchema.type](
      "__Schema",
      "types" -> pure(_ => d.outputs.values.toList.map[TypeInfo](TypeInfo.OutInfo(_))),
      "queryType" -> pure(_ => TypeInfo.OutInfo(ss.query): TypeInfo),
      "mutationType" -> pure(_ => ss.mutation.map[TypeInfo](TypeInfo.OutInfo(_))),
      "subscriptionType" -> pure(_ => ss.subscription.map[TypeInfo](TypeInfo.OutInfo(_))),
      "directives" -> pure(_ => List.empty[String])
    )

    lazy val rootFields: NonEmptyList[(String, Field[Id, Unit, _, _])] =
      NonEmptyList.of(
        "__schema" -> pure(_ => PhantomSchema),
        "__type" -> pure(arg[String]("name")) { case (_, name) =>
          d.inputs
            .get(name)
            .map[TypeInfo](TypeInfo.InInfo(_))
            .orElse(d.outputs.get(name).map[TypeInfo](TypeInfo.OutInfo(_)))
        }
      )

    rootFields
  }
}
