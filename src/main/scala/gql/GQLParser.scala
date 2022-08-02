package gql

import cats.implicits._
import cats.parse.{Parser => P}
import cats.parse.Rfc5234
import cats.parse.Numbers
import cats.parse.Parser0
import cats.data.NonEmptyList
import io.circe.JsonNumber
import io.circe.Json

// https://spec.graphql.org/June2018/#sec-Source-Text
object GQLParser {
  val whiteSpace = Rfc5234.wsp

  val lineTerminator = Rfc5234.lf | Rfc5234.crlf | Rfc5234.cr

  def w[A](p: P[A]): P[A] = p.surroundedBy((whiteSpace | lineTerminator).rep0)
  def t(c: Char): P[Unit] = w(P.char(c))
  def s(s: String): P[Unit] = w(P.string(s))

  // if this is slow, use charWhile
  val sourceCharacter: P[Char] =
    P.charIn(('\u0020' to '\uFFFF') :+ '\u0009' :+ '\u000A' :+ '\u000D')

  val unicodeBOM = P.char('\uFEFF')

  val commentChar = (!lineTerminator *> sourceCharacter).void

  val comment = (P.char('#') | Rfc5234.sp | commentChar).void

  val comma = t(',')

  val token =
    punctuator |
      name |
      intValue |
      floatValue |
      stringValue

  val ignored =
    unicodeBOM |
      whiteSpace |
      lineTerminator |
      comment |
      comma

  sealed trait Punctuator
  object Punctuator {
    case object `!` extends Punctuator
    case object `$` extends Punctuator
    case object `(` extends Punctuator
    case object `)` extends Punctuator
    case object `...` extends Punctuator
    case object `:` extends Punctuator
    case object `=` extends Punctuator
    case object `@` extends Punctuator
    case object `[` extends Punctuator
    case object `]` extends Punctuator
    case object `{` extends Punctuator
    case object `|` extends Punctuator
    case object `}` extends Punctuator
  }

  lazy val punctuator: P[Punctuator] = w {
    import Punctuator._
    P.char('!').as(`!`) |
      P.char('$').as(`$`) |
      P.char('(').as(`(`) |
      P.char(')').as(`)`) |
      P.char('.').as(`...`) |
      P.char(':').as(`:`) |
      P.char('=').as(`=`) |
      P.char('@').as(`@`) |
      P.char('[').as(`[`) |
      P.char(']').as(`]`) |
      P.char('{').as(`{`) |
      P.char('|').as(`|`)
  }

  lazy val name: P[String] = w {
    val rng = ('a' to 'z') :++ ('A' to 'Z') :+ '_'
    val begin = P.charIn(rng)

    val tl = begin | Numbers.digit

    (begin ~ tl.rep0).map { case (c, s) => c + s.mkString }
  }

  final case class Document(nel: NonEmptyList[Definition])
  val document = definition.rep

  sealed trait Definition
  object Definition {
    final case class ExecutableDef(x: ExecutableDefinition) extends Definition
    final case class TypeSystemDefinition(t: Any) extends Definition
    final case class TypeSystemExt(t: Any) extends Definition
  }
  lazy val definition = {
    import Definition._
    executableDefinition.map(ExecutableDef(_)) |
      typeSystemDefinition.map(TypeSystemDefinition(_)) |
      typeSystemExtension.map(TypeSystemExt(_))
  }

  sealed trait ExecutableDefinition
  object ExecutableDefinition {
    final case class Operation(o: OperationDefinition) extends ExecutableDefinition
    final case class Fragment(f: FragmentDefinition) extends ExecutableDefinition
  }
  lazy val executableDefinition = {
    import ExecutableDefinition._
    fragmentDefinition.map(Fragment(_)) |
      operationDefinition.map(Operation(_))
  }

  sealed trait OperationDefinition
  object OperationDefinition {
    final case class Detailed(
        tpe: OperationType,
        name: Option[String],
        variableDefinitions: Option[VariableDefinitions],
        directives: Option[Directives],
        selectionSet: SelectionSet
    ) extends OperationDefinition

    final case class Simple(selectionSet: SelectionSet) extends OperationDefinition
  }
  lazy val operationDefinition = {
    import OperationDefinition._
    (operationType ~ name.? ~ variableDefinitions.? ~ directives.? ~ selectionSet).map { case ((((opt, name), vars), ds), ss) =>
      Detailed(opt, name, vars, ds, ss)
    } |
      selectionSet.map(Simple(_))
  }

  sealed trait OperationType
  object OperationType {
    case object Query extends OperationType
    case object Mutation extends OperationType
    case object Subscription extends OperationType
  }

  lazy val operationType = w {
    import OperationType._

    P.string("query").as(Query) |
      P.string("mutation").as(Mutation) |
      P.string("subscription").as(Subscription)
  }

  final case class SelectionSet(selections: NonEmptyList[Selection])
  lazy val selectionSet: P[SelectionSet] = P.defer {
    selection.rep.between(t('{'), t('}')).map(SelectionSet(_))
  }

  sealed trait Selection
  object Selection {
    final case class FieldSelection(field: Field) extends Selection
    final case class FragmentSpreadSelection(fragmentSpread: FragmentSpread) extends Selection
    final case class InlineFragmentSelection(inlineFragment: InlineFragment) extends Selection
  }
  lazy val selection: P[Selection] = {
    import Selection._
    field.map(FieldSelection(_)) |
      // expects on, backtrack on failure
      P.backtrack(inlineFragment).map(InlineFragmentSelection(_)) |
      fragmentSpread.map(FragmentSpreadSelection(_))
  }

  final case class Field(
      alias: Option[String],
      name: String,
      arguments: Option[Arguments],
      directives: Option[Directives],
      selectionSet: Option[SelectionSet]
  )
  lazy val field: P[Field] = P.defer {
    (P.backtrack(alias).?.with1 ~ name ~ arguments.? ~ directives.? ~ selectionSet.?).map { case ((((a, n), args), d), s) =>
      Field(a, n, args, d, s)
    }
  }

  lazy val alias = name <* t(':')

  final case class Arguments(nel: NonEmptyList[Argument])
  lazy val arguments =
    argument.repSep(w(P.char(','))).between(t('('), t(')')).map(Arguments)

  final case class Argument(name: String, value: Value)
  lazy val argument =
    (name ~ (t(':') *> value)).map { case (n, v) => Argument(n, v) }

  final case class FragmentSpread(fragmentName: String, directives: Option[Directives])
  lazy val fragmentSpread =
    s("...") *> (fragmentName ~ directives.?).map { case (n, d) => FragmentSpread(n, d) }

  final case class InlineFragment(typeCondition: Option[String], directives: Option[Directives], selectionSet: SelectionSet)
  lazy val inlineFragment =
    (s("...") *> typeCondition.? ~ directives.? ~ selectionSet).map { case ((t, d), s) => InlineFragment(t, d, s) }

  final case class FragmentDefinition(
      name: String,
      typeCnd: String,
      directives: Option[Directives],
      selectionSet: SelectionSet
  )
  lazy val fragmentDefinition =
    (P.string("fragment") *> fragmentName ~ typeCondition ~ directives.? ~ selectionSet).map { case (((n, t), d), s) =>
      FragmentDefinition(n, t, d, s)
    }

  lazy val fragmentName: P[String] =
    (!P.string("on")).with1 *> name

  lazy val typeCondition: P[String] =
    s("on") *> name

  sealed trait Value
  object Value {
    final case class VariableValue(v: String) extends Value
    final case class IntValue(v: BigInt) extends Value
    final case class FloatValue(v: JsonNumber) extends Value
    final case class StringValue(v: String) extends Value
    final case class BooleanValue(v: Boolean) extends Value
    case object NullValue extends Value
    final case class EnumValue(v: String) extends Value
    final case class ListValue(v: List[Value]) extends Value
    final case class ObjectValue(v: List[(String, Value)]) extends Value
  }
  lazy val value: P[Value] = P.defer {
    import Value._
    variable.map(VariableValue(_)) |
      intValue.map(IntValue(_)) |
      floatValue.map(FloatValue(_)) |
      stringValue.map(StringValue(_)) |
      booleanValue.map(BooleanValue(_)) |
      nullValue.as(NullValue) |
      enumValue.map(EnumValue(_)) |
      listValue.map(ListValue(_)) |
      objectValue.map(ObjectValue(_))
  }

  lazy val booleanValue =
    P.string("true").as(true) |
      P.string("false").as(false)

  lazy val nullValue: P[Unit] =
    P.string("null")

  lazy val enumValue: P[String] =
    (!(booleanValue | nullValue)).with1 *> name

  lazy val listValue =
    (value <* comma.?).rep.between(t('['), t(']')).map(_.toList) |
      (t('[') *> t(']')).as(Nil)

  lazy val objectValue =
    (objectField <* comma.?).rep.between(t('{'), t('}')).map(_.toList) |
      (t('{') *> t('}')).as(Nil)

  lazy val objectField =
    name ~ (t(':') *> value)

  final case class VariableDefinitions(nel: NonEmptyList[VariableDefinition])
  lazy val variableDefinitions =
    variableDefinition.rep.between(t('('), t(')')).map(VariableDefinitions(_))

  final case class VariableDefinition(name: String, tpe: Type, defaultValue: Option[Value])
  lazy val variableDefinition =
    (variable ~ (t(':') *> `type`) ~ defaultValue.?).map { case ((n, t), d) => VariableDefinition(n, t, d) }

  lazy val variable =
    t('$') *> name

  lazy val defaultValue = value

  lazy val namedType = name.map(Type.Named(_))

  lazy val nonNullType: P[Type] =
    namedType <* t('!') |
      listType <* t('!')

  final case class Directives(nel: NonEmptyList[Directive])
  lazy val directives: P[Directives] = directive.rep.map(Directives(_))

  final case class Directive(name: String, arguments: Option[Arguments])
  lazy val directive: P[Directive] = s("@") *> (name ~ arguments.?).map { case (n, a) => Directive(n, a) }

  lazy val typeSystemDefinition =
    schemaDefinition |
      typeDefinition |
      directiveDefinition

  lazy val typeSystemExtension =
    schemaExtension |
      typeExtension

  lazy val schemaDefinition =
    P.string("schema") *> directives.? ~ operationTypeDefinition.rep.between(t('{'), t('}'))

  lazy val schemaExtension =
    P.string("extend") *> P.string("schema") *> directives.? ~ operationTypeDefinition.rep.between(t('{'), t('}')) |
      P.string("extend") *> P.string("schema") *> directives.?

  lazy val operationTypeDefinition =
    operationType ~ (t(':') *> namedType)

  lazy val description = stringValue

  lazy val typeDefinition =
    scalarTypeDefinition |
      objectTypeDefinition |
      interfaceTypeDefinition |
      unionTypeDefinition |
      enumTypeDefinition |
      inputObjectTypeDefinition

  lazy val typeExtension =
    scalarTypeExtension |
      objectTypeExtension |
      interfaceTypeExtension |
      unionTypeExtension |
      enumTypeExtension |
      inputObjectTypeExtension

  lazy val directiveDefinition =
    (defaultValue.?.with1 ~ (P.string("directive") *> t('@') *> name) ~ argumentsDefinition.? ~ (P.string("on") *> directiveLocations))

  lazy val scalarTypeDefinition =
    description.?.with1 ~ (P.string("scalar") *> name) ~ directives.?

  lazy val scalarTypeExtension =
    P.string("extend") *> P.string("scalar") *> (name ~ directives.?)

  lazy val objectTypeDefinition =
    description.?.with1 ~ (P.string("type") *> name) ~ implementsInterface.? ~ directives.? ~ fieldsDefinition.?

  lazy val objectTypeExtension =
    P.string("extend") *> P.string("type") *> name ~ implementsInterface.? ~ directives.? ~ fieldsDefinition |
      P.string("extend") *> P.string("type") *> name ~ implementsInterface.? ~ directives |
      P.string("extend") *> P.string("type") *> name ~ implementsInterface

  lazy val implementsInterface =
    P.string("implements") *> t('&').? *> namedType.rep.between(t('{'), t('}'))

  lazy val fieldsDefinition =
    fieldDefinition.rep.between(t('{'), t('}'))

  final case class FieldDefinition(
      description: Option[String],
      name: String,
      argumentsDefinition: Option[ArgumentsDefinition],
      `type`: Type,
      directives: Option[Directives]
  )
  lazy val fieldDefinition =
    (description.?.with1 ~ name ~ argumentsDefinition.? ~ (t(':') *> `type`) ~ directives.?).map { case ((((d, n), a), t), ds) =>
      FieldDefinition(d, n, a, t, ds)
    }

  final case class ArgumentsDefinition(nel: NonEmptyList[InputValueDefinition])
  lazy val argumentsDefinition =
    inputValueDefinition.rep.between(t('('), t(')')).map(ArgumentsDefinition(_))

  final case class InputValueDefinition(
      description: Option[String],
      name: String,
      `type`: Type,
      defaultValue: Option[Value],
      directives: Option[Directives]
  )
  lazy val inputValueDefinition =
    (description.?.with1 ~ name ~ (t(':') *> `type`) ~ defaultValue.? ~ directives.?).map { case ((((d, n), t), v), dirs) =>
      InputValueDefinition(d, n, t, v, dirs)
    }

  lazy val interfaceTypeDefinition =
    description.?.with1 ~ (P.string("interface") *> name) ~ directives.? ~ fieldsDefinition.?

  lazy val interfaceTypeExtension =
    P.string("extend") *> P.string("interface") *> name ~ directives.? ~ fieldsDefinition |
      P.string("extend") *> P.string("interface") *> name ~ directives.?

  lazy val unionTypeDefinition =
    description.?.with1 ~ (P.string("union") *> name) ~ directives.? ~ unionMemberTypes.?

  lazy val unionMemberTypes: P[NonEmptyList[Type.Named]] = P.defer {
    t('=') *> t('|').? *> namedType.map(NonEmptyList.one(_)) |
      (unionMemberTypes ~ (t('|') *> namedType)).map { case (xs, x) => xs :+ x }
  }

  lazy val unionTypeExtension =
    P.string("extend") *> P.string("union") *> name ~ directives.? ~ unionMemberTypes |
      P.string("extend") *> P.string("union") *> name ~ directives

  lazy val enumTypeDefinition =
    description.?.with1 ~ (P.string("enum") *> name) ~ directives.? ~ enumValuesDefinition.?

  lazy val enumValuesDefinition =
    enumValueDefinition.rep.between(t('{'), t('}'))

  lazy val enumValueDefinition =
    description.?.with1 ~ name ~ directives.?

  lazy val enumTypeExtension =
    P.string("extend") *> P.string("enum") *> name ~ directives.? ~ enumValuesDefinition |
      P.string("extend") *> P.string("enum") *> name ~ directives

  lazy val inputObjectTypeDefinition =
    description.?.with1 ~ (P.string("input") *> name) ~ directives.? ~ inputFieldsDefinition.?

  lazy val inputFieldsDefinition =
    inputValueDefinition.rep.between(t('{'), t('}'))

  lazy val inputObjectTypeExtension =
    P.string("extend") *> P.string("input") *> name ~ directives.? ~ inputFieldsDefinition |
      P.string("extend") *> P.string("input") *> name ~ directives

  final case class DirectivesDefinition(
      description: Option[String],
      name: String,
      // arguments: Option[ArgumentsDefinition],
      directiveLocations: NonEmptyList[DirectiveLocation]
  )
  lazy val directivesDefinition =
    description.?.with1 ~ (P.string("directive") *> t('@') *> name) ~ argumentsDefinition.? ~ (P.string("on") *> directiveLocations)

  lazy val directiveLocations: P[NonEmptyList[DirectiveLocation]] = P.defer {
    t('|').?.with1 *> directiveLocation.map(NonEmptyList.one(_)) |
      (directiveLocations ~ (t('|') *> directiveLocation)).map { case (xs, x) => xs :+ x }
  }

  sealed trait DirectiveLocation
  object DirectiveLocation {
    final case class Executable(x: ExecutableDirectiveLocation) extends DirectiveLocation
    final case class Type(x: TypeSystemDirectiveLocation) extends DirectiveLocation
  }
  lazy val directiveLocation: P[DirectiveLocation] =
    executableDirectiveLocation.map(DirectiveLocation.Executable(_)) |
      typeSystemDirectiveLocation.map(DirectiveLocation.Type(_))

  sealed trait ExecutableDirectiveLocation
  object ExecutableDirectiveLocation {
    case object QUERY extends ExecutableDirectiveLocation
    case object MUTATION extends ExecutableDirectiveLocation
    case object SUBSCRIPTION extends ExecutableDirectiveLocation
    case object FIELD extends ExecutableDirectiveLocation
    case object FRAGMENT_DEFINITION extends ExecutableDirectiveLocation
    case object FRAGMENT_SPREAD extends ExecutableDirectiveLocation
    case object INLINE_FRAGMENT extends ExecutableDirectiveLocation
  }
  lazy val executableDirectiveLocation = {
    import ExecutableDirectiveLocation._
    P.string("QUERY").as(QUERY) |
      P.string("MUTATION").as(MUTATION) |
      P.string("SUBSCRIPTION").as(SUBSCRIPTION) |
      P.string("FIELD").as(FIELD) |
      P.string("FRAGMENT_DEFINITION").as(FRAGMENT_DEFINITION) |
      P.string("FRAGMENT_SPREAD").as(FRAGMENT_SPREAD) |
      P.string("INLINE_FRAGMENT").as(INLINE_FRAGMENT)
  }

  sealed trait TypeSystemDirectiveLocation
  object TypeSystemDirectiveLocation {
    case object SCHEMA extends TypeSystemDirectiveLocation
    case object SCALAR extends TypeSystemDirectiveLocation
    case object OBJECT extends TypeSystemDirectiveLocation
    case object FIELD_DEFINITION extends TypeSystemDirectiveLocation
    case object ARGUMENT_DEFINITION extends TypeSystemDirectiveLocation
    case object INTERFACE extends TypeSystemDirectiveLocation
    case object UNION extends TypeSystemDirectiveLocation
    case object ENUM extends TypeSystemDirectiveLocation
    case object ENUM_VALUE extends TypeSystemDirectiveLocation
    case object INPUT_OBJECT extends TypeSystemDirectiveLocation
    case object INPUT_FIELD_DEFINITION extends TypeSystemDirectiveLocation
  }
  lazy val typeSystemDirectiveLocation = {
    import TypeSystemDirectiveLocation._
    P.string("SCHEMA").as(SCHEMA) |
      P.string("SCALAR").as(SCALAR) |
      P.string("OBJECT").as(OBJECT) |
      P.string("FIELD_DEFINITION").as(FIELD_DEFINITION) |
      P.string("ARGUMENT_DEFINITION").as(ARGUMENT_DEFINITION) |
      P.string("INTERFACE").as(INTERFACE) |
      P.string("UNION").as(UNION) |
      P.string("ENUM").as(ENUM) |
      P.string("ENUM_VALUE").as(ENUM_VALUE) |
      P.string("INPUT_OBJECT").as(INPUT_OBJECT) |
      P.string("INPUT_FIELD_DEFINITION").as(INPUT_FIELD_DEFINITION)
  }

  lazy val intValue: P[BigInt] = w(integerPart)

  lazy val integerPart = Numbers.bigInt

  lazy val floatValue: P[JsonNumber] = w(Numbers.jsonNumber).map(JsonNumber.fromString).collect { case Some(x) => x }

  lazy val stringValue: P[String] = w {
    val d = P.char('"')
    val tripple = d.rep(3, 3)

    blockStringCharacter.map(_.toString()).rep.surroundedBy(P.backtrack(tripple)) |
      stringCharacter.rep.surroundedBy(d)
  }.map(_.mkString_(""))

  lazy val stringCharacter: P[String] =
    ((!(P.charIn('"', '\\') | lineTerminator)).with1 *> sourceCharacter.map(_.toString())) |
      (P.string("\\u").as("\\u") ~ escapedUnicode).map { case (x, y) =>
        x + y
      } |
      (P.charIn('\\') ~ escapedCharacter).map { case (x, y) => x.toString() + y }

  lazy val escapedUnicode: P[String] =
    P.charIn(('0' to '9') :++ ('a' to 'f') :++ ('A' to 'F')).rep(4, 4).map(_.mkString_(""))

  lazy val escapedCharacter: P[Char] =
    P.charIn('"', '\\', '/', 'b', 'f', 'n', 'r', 't')

  lazy val blockStringCharacter: P[Char] = {
    val qs = P.string("\"\"\"")
    (!(qs | (P.char('\\') *> qs))).with1 *> sourceCharacter
  }

  sealed trait Type
  object Type {
    final case class Named(name: String) extends Type
    final case class List(of: Type) extends Type
    final case class NonNull(of: Type) extends Type
  }
  lazy val `type`: P[Type] = {
    import Type._
    namedType |
      P.defer(listType).map(List(_)) |
      P.defer(nonNullType).map(NonNull(_))
  }

  lazy val listType: P[Type] =
    `type`.between(t('['), t(']'))
}
