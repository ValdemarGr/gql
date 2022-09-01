package gql.out

import cats.data._

final case class Sub[F[_], I, M](
    name: String,
    fields: NonEmptyList[(String, I => fs2.Stream[F, M], Field[F, M, _, _])]
) {
  def obj: Obj[F, M] = Obj(name, fields.map { case (fn, _, f) => fn -> f })
}
