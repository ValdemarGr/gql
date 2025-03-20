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

import cats.implicits._
import munit.CatsEffectSuite

class TypeSystemParserTest extends CatsEffectSuite {
  typeSystems.zipWithIndex.map { case (q, i) =>
    test(s"parsing type system $i should work as expected") {
      assert(clue(gql.parser.parseSchema(q).leftMap(_.prettyError.value)).isRight)
    }
  }

  lazy val tq = "\"\"\""
  lazy val typeSystems = List(
    s"""
$tq
Root type for all your query operations
$tq
type Query {
  $tq
  Translates a string from a given language into a different language.
  $tq
  translate(
    "The original language that `text` is provided in."
    fromLanguage: Language

    "The translated language to be returned."
    toLanguage: Language

    "The text to be translated."
    text: String
  ): String
}

$tq
The set of languages supported by `translate`.
$tq
enum Language {
  "English"
  EN

  "French"
  FR

  "Chinese"
  CH
}

    """,
    """
type Query {
  latestVirus: Virus
}

type Virus {
  name: String
  mutations: [Mutation]
}

type Mutation {
  name: String
}
    """,
    """
type Query {
  dog: Dog
  findDog(searchBy: FindDogInput): Dog
}

enum DogCommand {
  SIT
  DOWN
  HEEL
}

type Dog implements Pet {
  name: String!
  nickname: String
  barkVolume: Int
  doesKnowCommand(dogCommand: DogCommand!): Boolean!
  isHouseTrained(atOtherHomes: Boolean): Boolean!
  owner: Human
}

interface Sentient {
  name: String!
}

interface Pet {
  name: String!
}

type Alien implements Sentient {
  name: String!
  homePlanet: String
}

type Human implements Sentient {
  name: String!
  pets: [Pet!]
}

enum CatCommand {
  JUMP
}

type Cat implements Pet {
  name: String!
  nickname: String
  doesKnowCommand(catCommand: CatCommand!): Boolean!
  meowVolume: Int
}

union CatOrDog = Cat | Dog
union DogOrHuman = Dog | Human
union HumanOrAlien = Human | Alien

input FindDogInput {
  name: String
  owner: String
}

    """,
    """
type Arguments {
  multipleRequirements(x: Int!, y: Int!): Int!
  booleanArgField(booleanArg: Boolean): Boolean
  floatArgField(floatArg: Float): Float
  intArgField(intArg: Int): Int
  nonNullBooleanArgField(nonNullBooleanArg: Boolean!): Boolean!
  booleanListArgField(booleanListArg: [Boolean]!): [Boolean]
  optionalNonNullBooleanArgField(optionalBooleanArg: Boolean! = false): Boolean!
}
    """
  )
}
