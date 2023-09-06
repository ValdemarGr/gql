package gql.relational

import cats.effect._
import skunk.implicits._
import gql.ast._
import gql.dsl._
import cats.implicits._
import skunk._
import cats._
import java.util.UUID
import gql.Arg
import gql.EmptyableArg
import skunk.codec.all._
import cats.data._
import gql.resolver.Resolver
import cats.effect.std.AtomicCell
import cats.effect.std.Supervisor
import cats.effect.std.Hotswap
import cats.effect.std.Mutex
import doobie._
import doobie.implicits._

object DoobieIntegraion extends QueryAlgebra with QueryDsl {
  type Frag = doobie.Fragment
  def stringToFrag(s: String): Frag = doobie.Fragment.const(s)
  implicit def appliedFragmentMonoid: Monoid[Frag] = doobie.Fragment.FragmentMonoid

  type Decoder[A] = doobie.Read[A]
  type Encoder[A] = doobie.Write[A]

  def optDecoder[A](d: Decoder[A]): Decoder[Option[A]] =
    new doobie.Read(
      d.gets.map { case (ev, _) => (ev, doobie.enumerated.Nullability.Nullable) },
      { (rs, n) =>
        val xs = d.gets.zipWithIndex.traverse { case ((ev, _), i) => ev.unsafeGetNullable(rs, n + i) }
        xs.as(d.unsafeGet(rs, n))
      }
    )

  implicit def applicativeForDecoder: Applicative[Decoder] = doobie.Read.ReadApply
}
