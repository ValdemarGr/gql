/*
 * Copyright 2022 Valdemar Grange
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
package gql.resolver

import cats._
import cats.data._
import cats.implicits._
import fs2.Stream
import gql.interpreter.Cursor
import gql.PreparedQuery
import gql.parser.{QueryParser => P}
import gql.dsl._
import gql.Arg

sealed trait Resolver[F[_], -I, A] {
  def mapK[G[_]: Functor](fk: F ~> G): Resolver[G, I, A]

  def contramap[B](g: B => I): Resolver[F, B, A]

  def mapWithInput[I2 <: I, B](f: (I2, A) => B)(implicit F: Functor[F]): Resolver[F, I2, B]
}

object Resolver {
  import cats.arrow._
  implicit def arrowForResolver[F[_]: Functor] = new Arrow[Resolver[F, *, *]] {
    override def compose[A, B, C](f: Resolver[F, B, C], g: Resolver[F, A, B]): Resolver[F, A, C] =
      CompositionResolver(g, f)

    override def first[A, B, C](fa: Resolver[F, A, B]): Resolver[F, (A, C), (B, C)] =
      fa
        .contramap[(A, C)] { case (a, _) => a }
        .mapWithInput[(A, C), (B, C)] { case ((_, c), b) => (b, c) }

    override def lift[A, B](f: A => B): Resolver[F, A, B] = 
      PureResolver(f)
  }
}

final case class ArgResolver[F[_], I, A, O](
    arg: Arg[A],
    resolver: Resolver[F, (I, A), O]
) extends Resolver[F, I, O] {
  override def mapK[G[_]: Functor](fk: F ~> G): Resolver[G, I, O] =
    ArgResolver(arg, resolver.mapK[G](fk))

  override def contramap[B](g: B => I): Resolver[F, B, O] =
    ArgResolver(arg, resolver.contramap[(B, A)] { case (b, a) => (g(b), a) })

  override def mapWithInput[I2 <: I, B](f: (I2, O) => B)(implicit F: Functor[F]): Resolver[F, I2, B] =
    ArgResolver(arg, resolver.mapWithInput[(I2, A), B] { case ((i, _), o) => f(i, o) })
}

final case class FallibleResolver[F[_], I, A](resolve: I => F[Ior[String, A]]) extends Resolver[F, I, A] {
  def mapK[G[_]: Functor](fk: F ~> G): FallibleResolver[G, I, A] =
    FallibleResolver(resolve.andThen(fk.apply))

  def contramap[B](g: B => I): FallibleResolver[F, B, A] =
    FallibleResolver(g andThen resolve)

  override def mapWithInput[I2 <: I, B](f: (I2, A) => B)(implicit F: Functor[F]): FallibleResolver[F, I2, B] =
    FallibleResolver(i => resolve(i).map(_.map(a => f(i, a))))
}

final case class EffectResolver[F[_], I, A](resolve: I => F[A]) extends Resolver[F, I, A] {
  def mapK[G[_]: Functor](fk: F ~> G): EffectResolver[G, I, A] =
    EffectResolver(resolve.andThen(fk.apply))

  def contramap[B](g: B => I): EffectResolver[F, B, A] =
    EffectResolver(g andThen resolve)

  def mapWithInput[I2 <: I, B](f: (I2, A) => B)(implicit F: Functor[F]): EffectResolver[F, I2, B] =
    EffectResolver(i => resolve(i).map(a => f(i, a)))
}

final case class PureResolver[F[_], I, A](resolve: I => A) extends Resolver[F, I, A] {
  override def mapK[G[_]: Functor](fk: F ~> G): PureResolver[G, I, A] =
    PureResolver(resolve)

  def contramap[B](g: B => I): PureResolver[F, B, A] =
    PureResolver(g andThen resolve)

  def mapWithInput[I2 <: I, B](f: (I2, A) => B)(implicit F: Functor[F]): PureResolver[F, I2, B] =
    PureResolver(i => f(i, resolve(i)))
}

final case class StreamResolver[F[_], I, A](
    stream: I => Stream[F, IorNec[String, A]]
) extends Resolver[F, I, A] {
  override def mapK[G[_]: Functor](fk: F ~> G): Resolver[G, I, A] =
    StreamResolver(stream.andThen(_.translate(fk)))

  override def contramap[B](g: B => I): Resolver[F, B, A] =
    StreamResolver[F, B, A](i => stream(g(i)))

  override def mapWithInput[I2 <: I, B](f: (I2, A) => B)(implicit F: Functor[F]): StreamResolver[F, I2, B] =
    StreamResolver[F, I2, B](i => stream(i).map(_.map(a => f(i, a))))
}

final case class CompositionResolver[F[_], I, A, O](
    left: Resolver[F, I, A],
    right: Resolver[F, A, O]
) extends Resolver[F, I, O] {
  override def mapK[G[_]: Functor](fk: F ~> G): Resolver[G, I, O] =
    CompositionResolver(left.mapK(fk), right.mapK(fk))

  override def contramap[B](g: B => I): Resolver[F, B, O] =
    CompositionResolver(left.contramap(g), right)

  override def mapWithInput[I2 <: I, B](f: (I2, O) => B)(implicit F: Functor[F]): CompositionResolver[F, I2, (I2, A), B] = {
    val l: Resolver[F, I2, (I2, A)] = left.mapWithInput[I2, (I2, A)]((i, a) => (i, a))
    val r0: Resolver[F, (I2, A), O] = right.contramap[(I2, A)] { case (_, a) => a }
    val r1: Resolver[F, (I2, A), B] = r0.mapWithInput[(I2, A), B] { case ((i2, _), o) => f(i2, o) }
    CompositionResolver(l, r1)
  }
}

final case class CacheResolver[F[_], I, I2, O](
    first: I => F[Either[I2, O]],
    fallback: Resolver[F, I2, O]
) extends Resolver[F, I, O] {
  override def mapK[G[_]: Functor](fk: F ~> G): Resolver[G, I, O] =
    CacheResolver(first.andThen(fk.apply), fallback.mapK(fk))

  override def contramap[B](g: B => I): Resolver[F, B, O] =
    CacheResolver(i => first(g(i)), fallback)

  override def mapWithInput[I3 <: I, B](f: (I3, O) => B)(implicit F: Functor[F]): CacheResolver[F, I3, (I2, I3), B] =
    CacheResolver[F, I3, (I2, I3), B](
      i3 =>
        first(i3).map {
          case Left(i2) => Left((i2, i3))
          case Right(o) => Right(f(i3, o))
        },
      fallback.contramap[(I2, I3)] { case (i2, _) => i2 }.mapWithInput[(I2, I3), B] { case ((_, i3), o) => f(i3, o) }
    )
}

final case class MetaResolver[F[_], I, O](fa: Resolver[F, (I, Meta), O]) extends Resolver[F, I, O] {
  override def mapK[G[_]: Functor](fk: F ~> G): Resolver[G, I, O] = ???

  override def contramap[B](g: B => I): Resolver[F, B, O] = ???

  override def mapWithInput[I2 <: I, B](f: (I2, O) => B)(implicit F: Functor[F]): Resolver[F, I2, B] =
    MetaResolver(fa.mapWithInput[(I2, Meta), B] { case ((i, _), o) => f(i, o) })
}

abstract case class BatchResolver[F[_], I, O](
    id: BatchResolver.ResolverKey,
    run: I => (Set[Any], Map[Any, Any] => O)
) extends Resolver[F, I, O] {
  override def mapK[G[_]: Functor](fk: F ~> G): BatchResolver[G, I, O] =
    new BatchResolver[G, I, O](id, run) {}

  override def contramap[B](g: B => I): BatchResolver[F, B, O] =
    new BatchResolver[F, B, O](id, b => run(g(b))) {}

  override def mapWithInput[I2 <: I, B](f: (I2, O) => B)(implicit F: Functor[F]): BatchResolver[F, I2, B] =
    new BatchResolver[F, I2, B](
      id,
      { i2 =>
        val (keys, finalize) = run(i2)
        (keys, finalize.andThen(f(i2, _)))
      }
    ) {}
}

object BatchResolver {
  final case class ResolverKey(id: Int) extends AnyVal

  object ResolverKey {
    implicit val order: Order[ResolverKey] = Order.by(_.id)
  }

  def apply[F[_], K, T](
      f: Set[K] => F[Map[K, T]]
  ): State[gql.SchemaState[F], BatchResolver[F, Set[K], Map[K, T]]] =
    State { s =>
      val id = s.nextId
      val rk = ResolverKey(id)
      val r = new BatchResolver[F, Set[K], Map[K, T]](
        rk,
        k => (k.asInstanceOf[Set[Any]], (m: Map[Any, Any]) => m.asInstanceOf[Map[K, T]])
      ) {}
      val entry = f.asInstanceOf[Set[Any] => F[Map[Any, Any]]]
      (s.copy(nextId = id + 1, batchers = s.batchers + (rk -> entry)), r)
    }
}
