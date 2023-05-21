---
title: Natchez (tracing)
---
The natchez package contains some functions to add traces to your `Compiler`.

The tracing functions include quite a lot of information such as the query plan and query in case of a invalid query.
The functions are quite simple, so by all means feel free explore tracing options that fit your use case better.

:::tip
If you have any custom tracing typeclass that can trace streams (something like `Resource[F, F ~> F]`), then seriously consider implementing your own tracing of `Application[F]` since the (seemingly) only possible implementation with `Trace`, is very unpleasant.
:::
