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
package gql.server.interpreter

final case class StreamingData[F[_], A, B](
    originIndex: Int,
    edges: StepCont[F, A, B],
    value: Either[Throwable, A]
)
object StreamingData {
  implicit def docedForStreamingData[F[_]]: Doced[StreamingData[F, ?, ?]] =
    DebugPrinter.Printer.streamingDataDoced[F]
}
