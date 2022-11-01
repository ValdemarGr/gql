package gql

import cats.implicits._
import munit.CatsEffectSuite

class ParserTest extends CatsEffectSuite {
  queries.zipWithIndex.map { case (q, i) =>
    test(s"parsing query $i should work as expected") {
      assert(clue(gql.parser.parse(q).leftMap(_.prettyError.value)).isRight)
    }
  }

  lazy val tq = "\"\"\""
  lazy val queries = List(
    """
query FragmentTyping {
  profiles(handles: ["zuck", "cocacola"]) {
    handle
    ...userFragment
    ...pageFragment
  }
}

fragment userFragment on User {
  friends {
    count
  }
}

fragment pageFragment on Page {
  likers {
    count
  }
}
  """,
    """
query withNestedFragments {
  user(id: 4) {
    friends(first: 10) {
      ...friendFields
    }
    mutualFriends(first: 10) {
      ...friendFields
    }
  }
}

fragment friendFields on User {
  id
  name
  ...standardProfilePic
}

fragment standardProfilePic on User {
  profilePic(size: 50)
}
  """,
    """
query inlineFragmentTyping {
  profiles(handles: ["zuck", "cocacola"]) {
    handle
    ... on User {
      friends {
        count
      }
    }
    ... on Page {
      likers {
        count
      }
    }
  }
}
  """,
    """
query inlineFragmentNoType($expandedInfo: Boolean) {
  user(handle: "zuck") {
    id
    name
      firstName
      lastName
      birthday
  }
}
  """,
    s"""
mutation {
  sendEmail(message: $tq
    Hello,
      World!

    Yours,
      GraphQL.
  $tq)
}
""",
    s"""
query {
  user(id: 4) {
    id
    name
    smallPic: profilePic(size: 64)
    bigPic: profilePic(size: 1024)
  }
}
""",
    s"""
query withNestedFragments {
  getData {
    ... on Data {
      a
      b
      c {
        ... DataFragment
      }
    }
  }
}

    fragment DataFragment on Data {
      a
      b
      c {
        ... NestedData
      }
    }

    fragment NestedData on Data {
      a
      b
      c {
        ... NestedData2
      }
    }

    fragment NestedData2 on Data {
      a
      b
    }
  """,
    s"""
query queryName($$foo: ComplexType, $$site: Site = MOBILE)  {
  whoever123is: node(id: [123, 456]) {
    id
    ... on User  {
      field2 {
        id
        alias: field1(first: 10, after: $$foo) {
          id
          ...frag 
        }
      }
      
      field3
      field4
      requiredField5: field5
      requiredSelectionSet(first: 10)  {
        field
      }
      unsetListItemsRequiredList: listField
      requiredListItemsUnsetList: listField
      requiredListItemsRequiredList: listField
      unsetListItemsOptionalList: listField
      optionalListItemsUnsetList: listField
      optionalListItemsOptionalList: listField
      multidimensionalList: listField
    }
    ... {
      id
    }
    ... {
      id
    }
  }
}
mutation likeStory {
  like(story: 123) {
    story {
      id 
    }
  }
}
subscription StoryLikeSubscription(
  $$input: StoryLikeSubscribeInput
)
   {
  storyLikeSubscribe(input: $$input) {
    story {
      likers {
        count
      }
      likeSentence {
        text
      }
    }
  }
}
fragment frag on Friend  {
  foo(
    size: $$size,
    bar: $$b,
    obj: {
      key: "value",
      block: $tq,
      block string uses
      $tq
    }
  )
}
{
  unnamed(truthy: true, falsy: false, nullish: null)
  query
}
query {
  __typename
}
  """,
    """
        query HeroNameQuery {
          hero {
            name
          }
        }
  """,
    """
        query HeroNameAndFriendsQuery {
          hero {
            id
            name
            friends {
              name
            }
          }
        }
  """,
    """
        query NestedQuery {
          hero {
            name
            friends {
              name
              appearsIn
              friends {
                name
              }
            }
          }
        }
  """,
    """
        query FetchLukeAndC3POQuery {
          human(id: "1000") {
            name
          }
          droid(id: "2000") {
            name
          }
        }
  """,
    """
        query FetchSomeIDQuery($someId: String!) {
          human(id: $someId) {
            name
          }
        }
  """,
    """
        query humanQuery($id: String!) {
          human(id: $id) {
            name
          }
        }
  """,
    """
        query UseFragment {
          luke: human(id: "1000") {
            ...HumanFragment
          }
          leia: human(id: "1003") {
            ...HumanFragment
          }
        }
        fragment HumanFragment on Human {
          name
          homePlanet
        }
  """,
    """
        query CheckTypeOfR2 {
          hero {
            __typename
            name
          }
        }
  """,
    """
        query CheckTypeOfLuke {
          hero(episode: EMPIRE) {
            __typename
            name
          }
        }
  """,
    """
        query HeroNameQuery {
          mainHero: hero {
            name
            story: secretBackstory
          }
        }
  """,
    """
        query NestedQueryWithFragment {
          hero {
            ...NameAndAppearances
            friends {
              ...NameAndAppearances
              friends {
                ...NameAndAppearances
              }
            }
          }
        }
        fragment NameAndAppearances on Character {
          name
          appearsIn
        }
  """,
    """
      {
        id
        name
      }
  """,
    """
      query ($foo: TestType)  {
        id
        name
      }
  """,
    """
      query ($foo: TestType = { a: 123 }) {
        id
      }
  """,
    """
query ($foo: TestType = { a: 123 }) { id }
  """,
    """
{trip(wheelchair:false arriveBy:false includePlannedCancellations:true transitDistanceReluctance:2000){dateTime}}
  """,
    """
      {
        ...rootQueryFragment 
      }
      fragment rootQueryFragment on QueryRoot {
        message {
          body
        }
      }
  """,
    """
      {
        ...  {
          message {
            body
          }
        }
      }
  """,
    """
      mutation {
        ...rootFragment 
      }
      fragment rootFragment on MutationRoot {
        mutationField {
          body
        }
      }
  """,
    """
      mutation {
        ...  {
          mutationField {
            body
          }
        }
      }
  """,
    """
      subscription {
        ...rootFragment 
      }
      fragment rootFragment on SubscriptionRoot {
        subscriptionField {
          body
        }
      }
  """,
    """
      subscription {
        ...  {
          subscriptionField {
            body
          }
        }
      }
  """,
    """
      subscription {
        subscriptionField {
          ...nestedFragment
        }
      }
      fragment nestedFragment on Message {
        body
      }
  """,
    """
      mutation {
        ...rootFragment
      }
      fragment rootFragment on MutationRoot {
        mutationListField  {
          name
        }
      }
  """,
    """
      query Foo($a: String, $b: String, $c: String) {
        field(a: $a) {
          field(b: $b) {
            field(c: $c)
          }
        }
      }
  """,
    """
      query Foo($a: String, $b: String, $c: String) {
        ... on Type {
          field(a: $a) {
            field(b: $b) {
              ... on Type {
                field(c: $c)
              }
            }
          }
        }
      }
  """,
    """
      query Foo($a: String, $b: String, $c: String) {
        ...FragA
      }
      fragment FragA on Type {
        field(a: $a) {
          ...FragB
        }
      }
      fragment FragB on Type {
        field(b: $b) {
          ...FragC
        }
      }
      fragment FragC on Type {
        field(c: $c)
      }
  """,
    """
      query Foo($a: String, $b: String) {
        ...FragA
      }
      fragment FragA on Type {
        field(a: $a) {
          ...FragB
        }
      }
      fragment FragB on Type {
        field(b: $b) {
          ...FragC
        }
      }
      fragment FragC on Type {
        field(c: $c)
      }
  """,
    """
      query Foo($b: String) {
        ...FragAB
      }
      query Bar($a: String) {
        ...FragAB
      }
      fragment FragAB on Type {
        field1(a: $a, b: $b)
        ...FragC
        field3(a: $a, b: $b)
      }
      fragment FragC on Type {
        field2(c: $c)
      }
  """,
    """
      fragment booleanArgFrag on ComplicatedArgs {
        booleanArgField(booleanArg: $booleanArg)
      }
      query Query($booleanArg: Boolean)
      {
        complicatedArgs {
          ...booleanArgFrag
        }
      }
  """,
    """
      query Query($booleanArg: Boolean)
      {
        complicatedArgs {
          ...booleanArgFrag
        }
      }
      fragment booleanArgFrag on ComplicatedArgs {
        booleanArgField(booleanArg: $booleanArg)
      }
  """,
    """
      query {
        human {
          pets {
            ... on Cat {
              meowsVolume
            }
            ... on Dog {
              barkVolume
            }
          }
        }
      }
  """,
    s"""
    query {
      field(arg: ["l"1"2"])
    }
  """
  )

  // Old queries WITH directives
  // lazy val queries = List(
  //   """
// query FragmentTyping {
  // profiles(handles: ["zuck", "cocacola"]) {
  //   handle
  //   ...userFragment
  //   ...pageFragment
  // }
// }

// fragment userFragment on User {
  // friends {
  //   count
  // }
// }

// fragment pageFragment on Page {
  // likers {
  //   count
  // }
// }
  // """,
  //   """
// query withNestedFragments {
  // user(id: 4) {
  //   friends(first: 10) {
  //     ...friendFields
  //   }
  //   mutualFriends(first: 10) {
  //     ...friendFields
  //   }
  // }
// }

// fragment friendFields on User {
  // id
  // name
  // ...standardProfilePic
// }

// fragment standardProfilePic on User {
  // profilePic(size: 50)
// }
  // """,
  //   """
// query inlineFragmentTyping {
  // profiles(handles: ["zuck", "cocacola"]) {
  //   handle
  //   ... on User {
  //     friends {
  //       count
  //     }
  //   }
  //   ... on Page {
  //     likers {
  //       count
  //     }
  //   }
  // }
// }
  // """,
  //   """
// query inlineFragmentNoType($expandedInfo: Boolean) {
  // user(handle: "zuck") {
  //   id
  //   name
  //   ... @include(if: $expandedInfo) {
  //     firstName
  //     lastName
  //     birthday
  //   }
  // }
// }
  // """,
  //   s"""
// mutation {
  // sendEmail(message: $tq
  //   Hello,
  //     World!

  //   Yours,
  //     GraphQL.
  // $tq)
// }
// """,
  //   s"""
// query {
  // user(id: 4) {
  //   id
  //   name
  //   smallPic: profilePic(size: 64)
  //   bigPic: profilePic(size: 1024)
  // }
// }
// """,
  //   s"""
// query withNestedFragments {
  // getData {
  //   ... on Data {
  //     a
  //     b
  //     c {
  //       ... DataFragment
  //     }
  //   }
  // }
// }

  //   fragment DataFragment on Data {
  //     a
  //     b
  //     c {
  //       ... NestedData
  //     }
  //   }

  //   fragment NestedData on Data {
  //     a
  //     b
  //     c {
  //       ... NestedData2
  //     }
  //   }

  //   fragment NestedData2 on Data {
  //     a
  //     b
  //   }
  // """,
  //   s"""
// query queryName($$foo: ComplexType, $$site: Site = MOBILE) @onQuery {
  // whoever123is: node(id: [123, 456]) {
  //   id
  //   ... on User @onInlineFragment {
  //     field2 {
  //       id
  //       alias: field1(first: 10, after: $$foo) @include(if: $$foo) {
  //         id
  //         ...frag @onFragmentSpread
  //       }
  //     }

  //     field3
  //     field4
  //     requiredField5: field5
  //     requiredSelectionSet(first: 10) @directive {
  //       field
  //     }
  //     unsetListItemsRequiredList: listField
  //     requiredListItemsUnsetList: listField
  //     requiredListItemsRequiredList: listField
  //     unsetListItemsOptionalList: listField
  //     optionalListItemsUnsetList: listField
  //     optionalListItemsOptionalList: listField
  //     multidimensionalList: listField
  //   }
  //   ... @skip(unless: $$foo) {
  //     id
  //   }
  //   ... {
  //     id
  //   }
  // }
// }
// mutation likeStory @onMutation {
  // like(story: 123) @onField {
  //   story {
  //     id @onField
  //   }
  // }
// }
// subscription StoryLikeSubscription(
  // $$input: StoryLikeSubscribeInput
// )
  // @onSubscription {
  // storyLikeSubscribe(input: $$input) {
  //   story {
  //     likers {
  //       count
  //     }
  //     likeSentence {
  //       text
  //     }
  //   }
  // }
// }
// fragment frag on Friend @onFragmentDefinition {
  // foo(
  //   size: $$size,
  //   bar: $$b,
  //   obj: {
  //     key: "value",
  //     block: $tq,
  //     block string uses
  //     $tq
  //   }
  // )
// }
// {
  // unnamed(truthy: true, falsy: false, nullish: null)
  // query
// }
// query {
  // __typename
// }
  // """,
  //   """
  //       query HeroNameQuery {
  //         hero {
  //           name
  //         }
  //       }
  // """,
  //   """
  //       query HeroNameAndFriendsQuery {
  //         hero {
  //           id
  //           name
  //           friends {
  //             name
  //           }
  //         }
  //       }
  // """,
  //   """
  //       query NestedQuery {
  //         hero {
  //           name
  //           friends {
  //             name
  //             appearsIn
  //             friends {
  //               name
  //             }
  //           }
  //         }
  //       }
  // """,
  //   """
  //       query FetchLukeAndC3POQuery {
  //         human(id: "1000") {
  //           name
  //         }
  //         droid(id: "2000") {
  //           name
  //         }
  //       }
  // """,
  //   """
  //       query FetchSomeIDQuery($someId: String!) {
  //         human(id: $someId) {
  //           name
  //         }
  //       }
  // """,
  //   """
  //       query humanQuery($id: String!) {
  //         human(id: $id) {
  //           name
  //         }
  //       }
  // """,
  //   """
  //       query UseFragment {
  //         luke: human(id: "1000") {
  //           ...HumanFragment
  //         }
  //         leia: human(id: "1003") {
  //           ...HumanFragment
  //         }
  //       }
  //       fragment HumanFragment on Human {
  //         name
  //         homePlanet
  //       }
  // """,
  //   """
  //       query CheckTypeOfR2 {
  //         hero {
  //           __typename
  //           name
  //         }
  //       }
  // """,
  //   """
  //       query CheckTypeOfLuke {
  //         hero(episode: EMPIRE) {
  //           __typename
  //           name
  //         }
  //       }
  // """,
  //   """
  //       query HeroNameQuery {
  //         mainHero: hero {
  //           name
  //           story: secretBackstory
  //         }
  //       }
  // """,
  //   """
  //       query NestedQueryWithFragment {
  //         hero {
  //           ...NameAndAppearances
  //           friends {
  //             ...NameAndAppearances
  //             friends {
  //               ...NameAndAppearances
  //             }
  //           }
  //         }
  //       }
  //       fragment NameAndAppearances on Character {
  //         name
  //         appearsIn
  //       }
  // """,
  //   """
  //     {
  //       id
  //       name
  //     }
  // """,
  //   """
  //     query ($foo: TestType) @testDirective {
  //       id
  //       name
  //     }
  // """,
  //   """
  //     query ($foo: TestType = { a: 123 }) {
  //       id
  //     }
  // """,
  //   """
// query ($foo: TestType = { a: 123 }) { id }
  // """,
  //   """
// {trip(wheelchair:false arriveBy:false includePlannedCancellations:true transitDistanceReluctance:2000){dateTime}}
  // """,
  //   """
  //     {
  //       ...rootQueryFragment @defer
  //     }
  //     fragment rootQueryFragment on QueryRoot {
  //       message {
  //         body
  //       }
  //     }
  // """,
  //   """
  //     {
  //       ... @defer {
  //         message {
  //           body
  //         }
  //       }
  //     }
  // """,
  //   """
  //     mutation {
  //       ...rootFragment @defer
  //     }
  //     fragment rootFragment on MutationRoot {
  //       mutationField {
  //         body
  //       }
  //     }
  // """,
  //   """
  //     mutation {
  //       ... @defer {
  //         mutationField {
  //           body
  //         }
  //       }
  //     }
  // """,
  //   """
  //     subscription {
  //       ...rootFragment @defer
  //     }
  //     fragment rootFragment on SubscriptionRoot {
  //       subscriptionField {
  //         body
  //       }
  //     }
  // """,
  //   """
  //     subscription {
  //       ... @defer {
  //         subscriptionField {
  //           body
  //         }
  //       }
  //     }
  // """,
  //   """
  //     subscription {
  //       subscriptionField {
  //         ...nestedFragment
  //       }
  //     }
  //     fragment nestedFragment on Message {
  //       body
  //     }
  // """,
  //   """
  //     mutation {
  //       ...rootFragment
  //     }
  //     fragment rootFragment on MutationRoot {
  //       mutationListField @stream {
  //         name
  //       }
  //     }
  // """,
  //   """
  //     query Foo($a: String, $b: String, $c: String) {
  //       field(a: $a) {
  //         field(b: $b) {
  //           field(c: $c)
  //         }
  //       }
  //     }
  // """,
  //   """
  //     query Foo($a: String, $b: String, $c: String) {
  //       ... on Type {
  //         field(a: $a) {
  //           field(b: $b) {
  //             ... on Type {
  //               field(c: $c)
  //             }
  //           }
  //         }
  //       }
  //     }
  // """,
  //   """
  //     query Foo($a: String, $b: String, $c: String) {
  //       ...FragA
  //     }
  //     fragment FragA on Type {
  //       field(a: $a) {
  //         ...FragB
  //       }
  //     }
  //     fragment FragB on Type {
  //       field(b: $b) {
  //         ...FragC
  //       }
  //     }
  //     fragment FragC on Type {
  //       field(c: $c)
  //     }
  // """,
  //   """
  //     query Foo($a: String, $b: String) {
  //       ...FragA
  //     }
  //     fragment FragA on Type {
  //       field(a: $a) {
  //         ...FragB
  //       }
  //     }
  //     fragment FragB on Type {
  //       field(b: $b) {
  //         ...FragC
  //       }
  //     }
  //     fragment FragC on Type {
  //       field(c: $c)
  //     }
  // """,
  //   """
  //     query Foo($b: String) {
  //       ...FragAB
  //     }
  //     query Bar($a: String) {
  //       ...FragAB
  //     }
  //     fragment FragAB on Type {
  //       field1(a: $a, b: $b)
  //       ...FragC
  //       field3(a: $a, b: $b)
  //     }
  //     fragment FragC on Type {
  //       field2(c: $c)
  //     }
  // """,
  //   """
  //     fragment booleanArgFrag on ComplicatedArgs {
  //       booleanArgField(booleanArg: $booleanArg)
  //     }
  //     query Query($booleanArg: Boolean)
  //     {
  //       complicatedArgs {
  //         ...booleanArgFrag
  //       }
  //     }
  // """,
  //   """
  //     query Query($booleanArg: Boolean)
  //     {
  //       complicatedArgs {
  //         ...booleanArgFrag
  //       }
  //     }
  //     fragment booleanArgFrag on ComplicatedArgs {
  //       booleanArgField(booleanArg: $booleanArg)
  //     }
  // """,
  //   """
  //     query {
  //       human {
  //         pets {
  //           ... on Cat {
  //             meowsVolume
  //           }
  //           ... on Dog {
  //             barkVolume
  //           }
  //         }
  //       }
  //     }
  // """
  // )
}
