package gql.client.codegen

import munit.CatsEffectSuite

class CodeGeneratorTest extends CatsEffectSuite {
  import Generator._
  test("test the code-gen") {
    val o = getSchemaFrom(testSchema).flatMap(generateWith(_, testQuery))

    println(o.map(_.render(80)))
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
