---
title: GraphQL-WS
---
`gql` contains a spec-compliant GraphQL over WebSocket protocol implementation.
The implementation requres a compiler implementation and returns a stream of messages to send to the client and a handler for messages that originate from the client.

The GraphQL-WS implementation shouldn't usually be used directly, but rather through a websocket-supported http server implementation, like [http4s](./http4s).
