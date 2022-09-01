package gql.out

trait Toplevel[F[_], A] extends Output[F, A] {
  def name: String
}
