package gql.goi

// import cats.implicits._

trait Node {
  def id: String
}

final case class NodeId(id: String)

trait NodeInstance[F[_], A] {
  def id(a: A): NodeId
}

object Node {
  def fromTypename[F[_], A](typename: String, get: String => F[Option[A]], id: A => String) = {
    def get0(id: String) = id.split(':').toList match {
      case x :: xs if x == typename => Some(get(xs.mkString(":")))
      case _                        => None
    }
    def id0(a: A) = s"$typename:${id(a)}"
  }
}
