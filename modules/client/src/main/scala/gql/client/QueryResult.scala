package gql.client

import io.circe._

final case class QueryResult[A](
    data: A,
    errors: Option[List[QueryResult.Error]]
)

object QueryResult {
  final case class Error(
      mesage: String,
      path: List[String],
      original: JsonObject
  )

  object Error {
    implicit val decoder = Decoder.instance[Error]{ c =>
        for {
            message <- c.downField("message").as[String]
            path <- c.downField("path").as[List[String]]
            original <- c.as[JsonObject]
        } yield Error(message, path, original)
    }
  }

  implicit def decoder[A: Decoder] = Decoder.instance[QueryResult[A]]{ c =>
      for {
          data <- c.downField("data").as[A]
          errors <- c.downField("errors").as[Option[List[Error]]]
      } yield QueryResult(data, errors)
  }
}
