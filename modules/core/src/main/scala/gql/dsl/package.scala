/*
 * Copyright 2024 Valdemar Grange
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

package object dsl {
  object all extends GqlDslFull
  object aliases extends Aliases
  object directive extends DirectiveDslFull
  object enumType extends EnumDslFull
  object field extends FieldDslFull
  object input extends InputDslFull
  object interface extends InterfaceDslFull
  object tpe extends TypeDslFull
  object union extends UnionDslFull
}
