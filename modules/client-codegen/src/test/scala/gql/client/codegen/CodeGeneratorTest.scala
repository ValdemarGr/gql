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
package gql.client.codegen

import munit.CatsEffectSuite

class CodeGeneratorTest extends CatsEffectSuite {
  // import Generator._
  test("test the code-gen") {
    /* val o = getSchemaFrom(testSchema).flatMap(generateWith(_, testQuery))

    println(o.map(_._2.render(80)))*/
  }

  def testQuery = """
  query AlienQuery($name: String!, $id: ID!) {
    dog {
      ... DogFragment
      ... on Dog {
        name
        barkVolume
      }
      owner {
        name
      }
    }
    findDog(searchBy: { name: "Barky" }) {
      ... DogFragment
    }
  }

  fragment DogFragment on Dog {
    name,
    barkVolume
  }
  """

  def testSchema = """
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
"""
}
