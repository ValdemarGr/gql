package gql

import cats.effect._

package object interpreter {
  type BatchKey = Any
  type BatchValue = Any
  type BatchResult = Any

  type ResourceToken = Unique.Token

  type StreamToken = Unique.Token
}
