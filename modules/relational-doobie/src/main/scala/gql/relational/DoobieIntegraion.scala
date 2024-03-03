/*
 * Copyright 2024 Valdemar Grange
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
package gql.relational.doobie

import cats.effect._
import cats._
import cats.data._
import doobie._
import doobie.implicits._
import gql.relational.{QueryAlgebra, QueryDsl}

object DoobieIntegraion extends QueryAlgebra {
  type Frag = doobie.Fragment
  def stringToFrag(s: String): Frag = doobie.Fragment.const0(s)
  implicit def appliedFragmentMonoid: Monoid[Frag] = doobie.Fragment.FragmentMonoid

  type Decoder[A] = doobie.Read[A]

  def optDecoder[A](d: Decoder[A]): Decoder[Option[A]] = {
    import cats.implicits._
    val ys = d.gets.traverse { case (g, _) => doobie.Read.fromGetOption(g.map(x => x: Any)) }
    new doobie.Read(
      ys.gets,
      { (rs, n) =>
        if (ys.unsafeGet(rs, n).forall(_.isEmpty)) None
        else Some(d.unsafeGet(rs, n))
      }
    )
  }

  implicit def applicativeForDecoder: Applicative[Decoder] = doobie.Read.ReadApply

  type Connection[F[_]] = Transactor[F]
  implicit def doobieQueryable[F[_]: MonadCancelThrow]: Queryable[F] = new Queryable[F] {
    def apply[A](query: Frag, decoder: Decoder[A], connection: Connection[F]): F[List[A]] =
      query.query(decoder).to[List].transact(connection)
  }
}

object dsl extends QueryDsl(DoobieIntegraion) {
  import algebra._

  trait DoobieTable extends Table {
    def aliased(x: Fragment): Fragment =
      Fragment.const(alias) ++ fr"." ++ x

    def sel[A](x: String, d: Decoder[A]): (Fragment, Query.Select[A]) = {
      val col = aliased(Fragment.const(x))
      col -> Query.Select(Chain(col), d)
    }
  }
}
