---
title: The schema
---
## SchemaShape
The `SchemaShape` consists of the roots that make up your gql schema; A query, mutation and subscription type.

## Schema
A `Schema` is a collection of some components that are required to execute a query.
The `Schema` contains a `SchemaShape`, a `Statistics` instance and state regarding `BatchResolver` implementations.
:::tip
Check out the [statistics section](../execution/statistics) for more information on the `Statistics` object.

Also, checkout the [resolver section](./resolvers) for more information on `BatchResolver`s.
:::

The most powerful `Schema` constructor `stateful`, converts a `State[SchemaState[F], SchemaShape[F, Q, M, S]]` to a `Schema[F, Q, M, S]`.

