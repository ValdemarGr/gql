package gql.resolver

import cats.data._
import gql._

final class Resolver[F[_], -I, +O] (private [resolver] val underlying: Step[F, I, O]) {
    def andThen[O2](that: Resolver[F, O, O2]): Resolver[F, I, O2] =
        new Resolver(Step.compose(underlying, that.underlying))
        
    def continue[O2, O1 >: O](f: Resolver[F, O1, O1] => Resolver[F, O1, O2]): Resolver[F, I, O2] =
        andThen(f(Resolver.id[F, O1]))    

    def compose[I1 <: I, I2](that: Resolver[F, I2, I1]): Resolver[F, I2, O] =
        that andThen this

    def map[O2](f: O => O2): Resolver[F, I, O2] =
        this andThen Resolver.lift(f)

    def contramap[I2](f: I2 => I): Resolver[F, I2, O] =
        Resolver.lift(f) andThen this

    def evalMap[O2](f: O => F[O2]): Resolver[F, I, O2] =
        this andThen Resolver.eval(f)

    def evalContramap[I1 <: I, I2](f: I2 => F[I1]): Resolver[F, I2, O] =
        Resolver.eval(f) andThen this

    def fallibleMap[O2](f: O => Ior[String, O2]): Resolver[F, I, O2] =
        this andThen Resolver.raise(f)

    def fallibleContraMap[I2](f: I2 => Ior[String, I]): Resolver[F, I2, O] =
        Resolver.raise(f) andThen this

    def first[C]: Resolver[F, (I, C), (O, C)] =
        new Resolver(Step.first(underlying))

    def tupleIn[I1 <: I]: Resolver[F, I1, (O, I1)] =
        first[I1].contramap[I1](i => (i, i))

    def arg[A](arg: Arg[A]): Resolver[F, I, (A, O)] =
        this andThen Resolver.argument[F, O, A](arg).tupleIn

    def contraArg[A, I2](arg: Arg[A])(implicit ev: (A, I2) <:< I): Resolver[F, I2, O] =
        Resolver.id[F, I2].arg(arg) andThen this.contramap(ev.apply)

    def meta: Resolver[F, I, (Meta, O)] =
        this andThen Resolver.meta[F, O].tupleIn

    def stream[O2](f: O => fs2.Stream[F, O2]): Resolver[F, I, O2] =
        this andThen Resolver.stream[F, O, O2](f)

    def skippable[O1 >: O]: Resolver[F, Either[I, O1], O1] =
        new Resolver(Step.skip(this.underlying))

    def skipThis[I1 <: I, I2, O1 >: O](verify: Resolver[F, I2, Either[I1, O1]]): Resolver[F, I2, O1] =
        verify andThen this.skippable

    def skipThisWith[I1 <: I, I2, O1 >: O](f: Resolver[F, I2, I2] => Resolver[F, I2, Either[I1, O1]]): Resolver[F, I2, O1] =
        skipThis[I1, I2, O1](f(Resolver.id[F, I2]))
    
    def skipThat[I1 <: I, I2, O2](compute: Resolver[F, I2, O2])(implicit ev: O <:< Either[I2, O2]): Resolver[F, I1, O2] =
        compute skipThis this.map(ev.apply)

    def skipThatWith[I1 <: I, I2, O2](f: Resolver[F, I2, I2] => Resolver[F, I2, O2])(implicit ev: O <:< Either[I2, O2]): Resolver[F, I1, O2] =
        skipThat[I1, I2, O2](f(Resolver.id[F, I2]))
}

object Resolver extends ResolverInstances {
    def lift[F[_], I, O](f: I => O): Resolver[F, I, O] =
      new Resolver(Step.lift(f))

    def id[F[_], I]: Resolver[F, I, I] =
        lift(identity)

    def eval[F[_], I, O](f: I => F[O]): Resolver[F, I, O] =
        new Resolver(Step.effect(f))
    
    def raise[F[_], I, O](f: I => Ior[String, O]): Resolver[F, I, O] =
        new Resolver(Step.raise(f))

    def argument[F[_], I <: Any, A](arg: Arg[A]): Resolver[F, I, A] =
        new Resolver(Step.argument(arg))

    def meta[F[_], I <: Any]: Resolver[F, I, Meta] =
        new Resolver(Step.getMeta)

    def stream[F[_], I, O](f: I => fs2.Stream[F, O]): Resolver[F, I, O] =
        new Resolver(Step.stream(f))

    def batch[F[_], K, V](f: Set[K] => F[Map[K, V]]): State[gql.SchemaState[F], Resolver[F, Set[K], Map[K, V]]] =
        Step.batch[F, K, V](f).map(new Resolver(_))
        
}

trait ResolverInstances {
    import cats.arrow._
    implicit def arrowForResolver[F[_]] = new Arrow[Resolver[F, *, *]] {
        override def compose[A, B, C](f: Resolver[F, B, C], g: Resolver[F, A, B]): Resolver[F, A, C] = 
            new Resolver(Step.compose(g.underlying, f.underlying))

        override def first[A, B, C](fa: Resolver[F, A, B]): Resolver[F, (A, C), (B, C)] = 
            new Resolver(Step.first[F, A, B, C](fa.underlying))

        override def lift[A, B](f: A => B): Resolver[F, A, B] = Resolver.lift(f)
    }
/*
    implicit def applicativeForResolver[F[_], I] = new Applicative[Resolver[F, I, *]] {

    }*/
}
