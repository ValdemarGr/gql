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
package gql.parser

import cats.parse.{Parser => P}

// https://spec.graphql.org/June2018/#sec-Source-Text
object QueryParser {
  import QueryAst._
  import GraphqlParser._

  lazy val executableDefinition = seps0.with1 *> {
    import ExecutableDefinition._
    Pos.pos(fragmentDefinition).map(Fragment(_)) |
      Pos.pos(operationDefinition).map(Operation(_))
  }

  lazy val operationDefinition = {
    import OperationDefinition._
    P.backtrack(selectionSet).map(Simple(_)) |
      (operationType ~ name.? ~ variableDefinitions.? ~ selectionSet).map { case (((opt, name), vars), ss) =>
        Detailed(opt, name, vars, ss)
      }
  }

  lazy val variableDefinitions =
    variableDefinition.rep
      .between(t('('), t(')'))
      .map(VariableDefinitions(_))

  lazy val variableDefinition = Pos.pos {
    (variable ~ (t(':') *> `type`) ~ defaultValue(constValue).?).map { case ((n, t), d) => VariableDefinition(n, t, d) }
  }

  lazy val operationType = {
    import OperationType._

    s("query").as(Query) |
      s("mutation").as(Mutation) |
      s("subscription").as(Subscription)
  }

  lazy val selectionSet: P[SelectionSet] = P.defer {
    Pos.pos(selection).rep.between(t('{'), t('}')).map(SelectionSet(_))
  }

  lazy val selection: P[Selection] = {
    import Selection._
    field.map(FieldSelection(_)) |
      // expects on, backtrack on failure
      inlineFragment.map(InlineFragmentSelection(_)) |
      fragmentSpread.map(FragmentSpreadSelection(_))
  }

  lazy val field: P[Field] = P.defer {
    (P.backtrack(alias).?.with1 ~ name ~ arguments.? ~ Pos.pos0(selectionSet.?))
      .map { case (((a, n), args), s) => Field(a, n, args, s) }
  }

  lazy val alias = name <* t(':')

  lazy val arguments =
    argument.rep.between(t('('), t(')')).map(Arguments.apply)

  lazy val argument =
    (name ~ (t(':') *> value)).map { case (n, v) => Argument(n, v) }

  lazy val fragmentSpread =
    (s("...") *> fragmentName).map(FragmentSpread.apply)

  lazy val inlineFragment =
    ((s("...") *> typeCondition.?).soft ~ selectionSet).map { case (t, s) => InlineFragment(t, s) }

  lazy val fragmentDefinition =
    (s("fragment") *> fragmentName ~ typeCondition ~ selectionSet).map { case ((n, t), s) =>
      FragmentDefinition(n, t, s)
    }

  lazy val fragmentName: P[String] =
    (!s("on")).with1 *> name

  lazy val typeCondition: P[String] =
    s("on") *> name
}