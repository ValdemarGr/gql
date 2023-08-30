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
