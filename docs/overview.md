---
title: Overview of gql
---
## Overview
* gql is a library for defining [GraphQL](https://graphql.org/) servers and clients in Scala.
* gql embraces a functional [code-first](https://www.apollographql.com/blog/backend/architecture/schema-first-vs-code-only-graphql/#code-only) approach to GraphQL.
* gql builds on purely functional abstractions from the [Typelevel](https://typelevel.org/) ecosystem such as [cats-parse](https://github.com/typelevel/cats-parse), [cats-effect](https://github.com/typelevel/cats-effect) and [fs2](https://github.com/typelevel/fs2).
* gql is pre-release software, if you find any bugs, please post them on the [issue tracker](https://github.com/ValdemarGr/gql/issues).

## Resources
gql assumes you have knowledge of [cats-effect](https://github.com/typelevel/cats-effect) and [cats](https://github.com/typelevel/cats).

No knowledge of the GraphQL is needed to get started with [gql's tutorial](tutorial/introduction)!

For further information consider:
* The server reference documentation, starting at [Output types](server/schema/output_types)
* The client reference documentation, starting at [Query DSL](client/dsl)
* The [modules](overview/modules.md) page for a list of all of gql' modules.
* The [full example](overview/full_example.md) page for a full example of an executable schema.
* The official [GraphQL specification](https://spec.graphql.org/draft/)