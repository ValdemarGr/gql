package gql

import cats._
import cats.implicits._
import gql.ast._
import gql.resolver._
import cats.data._
import scala.reflect.ClassTag

object dsl {
  def tpe[F[_], A](
      name: String,
      hd: (String, Field[F, ? >: A, ?, ?]),
      tl: (String, Field[F, ? >: A, ?, ?])*
  ) = Type[F, A](name, NonEmptyList(hd, tl.toList), Nil)

  def input[A](
      name: String,
      fields: NonEmptyArg[A]
  ): Input[A] = Input(name, fields)

  def arg[A](name: String)(implicit tpe: => In[A]): NonEmptyArg[A] =
    NonEmptyArg.one[A](ArgValue(name, Eval.later(tpe), None, None))

  def arg[A](name: String, description: String)(implicit tpe: => In[A]): NonEmptyArg[A] =
    NonEmptyArg.one[A](ArgValue(name, Eval.later(tpe), None, Some(description)))

  def arg[A](name: String, default: Value)(implicit tpe: => In[A]): NonEmptyArg[A] =
    NonEmptyArg.one[A](ArgValue(name, Eval.later(tpe), Some(default), None))

  def arg[A](name: String, default: Value, description: String)(implicit tpe: => In[A]): NonEmptyArg[A] =
    NonEmptyArg.one[A](ArgValue(name, Eval.later(tpe), Some(default), Some(description)))

  def cache[F[_]: Functor, I, O](resolver: Resolver[F, I, O])(get: I => F[Option[O]]): CacheResolver[F, I, I, O] =
    CacheResolver(i => get(i).map(_.toRight(i)), resolver)

  def cacheFull[F[_], I, I2, O](resolver: Resolver[F, I2, O])(get: I => F[Either[I2, O]]): CacheResolver[F, I, I2, O] =
    CacheResolver(get, resolver)

  object value {
    def scalar[F[_], A](value: A)(implicit tpe: => Scalar[F, A]) =
      tpe.encoder(value)

    def fromEnum[F[_], A](value: A)(implicit tpe: => Enum[F, A]) =
      tpe.revm.get(value).map(enumValue)

    def enumValue(value: String) = Value.EnumValue(value)

    def arr(xs: Value*) = Value.ArrayValue(xs.toVector)

    def obj(xs: (String, Value)*) = Value.ObjectValue(xs.toMap)

    def nullValue = Value.NullValue
  }

  final class PartiallyAppliedField[I](val dummy: Boolean = false) extends AnyVal {
    def apply[F[_], T, A](arg: Arg[A])(resolver: Resolver[F, (I, A), T])(implicit
        tpe: => Out[F, T]
    ): Field[F, I, T, A] =
      full.field[F, I, T, A](arg)(resolver)(tpe)

    def apply[F[_], T](resolver: Resolver[F, I, T])(implicit tpe: => Out[F, T]): Field[F, I, T, Unit] =
      full.field[F, I, T](resolver)(tpe)
  }

  def field[I] = new PartiallyAppliedField[I]

  final class PartiallyAppliedStream[I](val dummy: Boolean = false) extends AnyVal {
    def apply[F[_], T](f: I => fs2.Stream[F, T]): StreamResolver[F, I, T] =
      full.stream[F, I, T](f)
  }

  def stream[I] = new PartiallyAppliedStream[I]

  final class PartiallyAppliedStreamFallible[I](val dummy: Boolean = false) extends AnyVal {
    def apply[F[_], T](f: I => fs2.Stream[F, IorNec[String, T]]): StreamResolver[F, I, T] =
      full.streamFallible[F, I, T](f)
  }

  def streamFallible[I] = new PartiallyAppliedStreamFallible[I]

  final class PartiallyAppliedFallible[I](val dummy: Boolean = false) extends AnyVal {
    def apply[F[_], T, A](
        arg: Arg[A]
    )(resolver: (I, A) => F[Ior[String, T]])(implicit tpe: => Out[F, T]): Field[F, I, T, A] =
      full.fallible[F, I, T, A](arg)(resolver)(tpe)

    def apply[F[_], T](resolver: I => F[Ior[String, T]])(implicit tpe: => Out[F, T]): Field[F, I, T, Unit] =
      full.fallible[F, I, T](resolver)(tpe)
  }

  def fallible[I] = new PartiallyAppliedFallible[I]

  final class PartiallyAppliedEff[I](val dummy: Boolean = false) extends AnyVal {
    def apply[F[_], T, A](arg: Arg[A])(resolver: (I, A) => F[T])(implicit tpe: => Out[F, T]): Field[F, I, T, A] =
      full.eff[F, I, T, A](arg)(resolver)(tpe)

    def apply[F[_], T](resolver: I => F[T])(implicit tpe: => Out[F, T]): Field[F, I, T, Unit] =
      full.eff[F, I, T](resolver)(tpe)
  }

  def eff[I] = new PartiallyAppliedEff[I]

  final class PartiallyAppliedPure[F[_], I](val dummy: Boolean = false) extends AnyVal {
    def apply[T, A](arg: Arg[A])(resolver: (I, A) => Id[T])(implicit tpe: => Out[F, T]): Field[F, I, T, A] =
      full.pure[F, I, T, A](arg)(resolver)(tpe)

    def apply[T](resolver: I => Id[T])(implicit tpe: => Out[F, T]): Field[F, I, T, Unit] =
      full.pure[F, I, T](resolver)(tpe)
  }

  def pure[F[_], I] = new PartiallyAppliedPure[F, I]

  object full {
    def field[F[_], I, T, A](arg: Arg[A])(resolver: Resolver[F, (I, A), T])(implicit
        tpe: => Out[F, T]
    ): Field[F, I, T, A] =
      Field[F, I, T, A](arg, resolver, Eval.later(tpe))

    def field[F[_], I, T](resolver: Resolver[F, I, T])(implicit tpe: => Out[F, T]): Field[F, I, T, Unit] =
      Field[F, I, T, Unit](Applicative[Arg].unit, resolver.contramap[(I, Unit)] { case (i, _) => i }, Eval.later(tpe))

    def eff[F[_], I, T, A](arg: Arg[A])(resolver: (I, A) => F[T])(implicit tpe: => Out[F, T]): Field[F, I, T, A] =
      field(arg)(EffectResolver[F, (I, A), T] { case (i, a) => resolver(i, a) })(tpe)

    def eff[F[_], I, T](resolver: I => F[T])(implicit tpe: => Out[F, T]): Field[F, I, T, Unit] =
      eff[F, I, T, Unit](Applicative[Arg].unit)((i, _) => resolver(i))(tpe)

    // Not sure if this is a compiler bug or something since all type parameters except I are invariant?
    def pure[F[_], I, T, A](arg: Arg[A])(resolver: (I, A) => Id[T])(implicit tpe: => Out[F, T]): Field[F, I, T, A] =
      field(arg)(PureResolver[F, (I, A), T] { case (i, a) => resolver(i, a) })(tpe)

    def pure[F[_], I, T](resolver: I => Id[T])(implicit tpe: => Out[F, T]): Field[F, I, T, Unit] =
      field(PureResolver[F, I, T](resolver))(tpe)

    def fallible[F[_], I, T, A](
        arg: Arg[A]
    )(resolver: (I, A) => F[Ior[String, T]])(implicit tpe: => Out[F, T]): Field[F, I, T, A] =
      field(arg)(FallibleResolver[F, (I, A), T] { case (i, a) => resolver(i, a) })(tpe)

    def fallible[F[_], I, T](resolver: I => F[Ior[String, T]])(implicit tpe: => Out[F, T]): Field[F, I, T, Unit] =
      fallible[F, I, T, Unit](Applicative[Arg].unit)((i, _) => resolver(i))(tpe)

    def stream[F[_], I, T](f: I => fs2.Stream[F, T]): StreamResolver[F, I, T] =
      streamFallible[F, I, T](i => f(i).map(_.rightIor))

    def streamFallible[F[_], I, T](f: I => fs2.Stream[F, IorNec[String, T]]): StreamResolver[F, I, T] =
      StreamResolver(f)
  }

  def enumVal[A](value: A): EnumValue[A] =
    EnumValue(value)

  def enumType[F[_], A](name: String, hd: (String, EnumValue[? <: A]), tl: (String, EnumValue[? <: A])*) =
    Enum[F, A](name, NonEmptyList(hd, tl.toList))

  def interface[F[_], A](
      name: String,
      hd: (String, Field[F, ? >: A, ?, ?]),
      tl: (String, Field[F, ? >: A, ?, ?])*
  ) = Interface[F, A](name, NonEmptyList(hd, tl.toList), Nil)

  def union[F[_], A](name: String) = PartiallyAppliedUnion0[F, A](name)

  def optType[F[_], A, B](resolver: Resolver[F, A, B])(implicit tpe: => Out[F, B]): Out[F, Option[A]] =
    OutOpt(tpe, resolver)

  def optType[A](implicit tpe: => In[A]): In[Option[A]] = InOpt(tpe)

  def arrType[F[_], A, C, B](toSeq: C => Seq[A])(resolver: Resolver[F, A, B])(implicit
      tpe: => Out[F, B]
  ): OutArr[F, A, C, B] =
    OutArr(tpe, toSeq, resolver)

  def arrType[F[_], A, G[x] <: Seq[x], B](resolver: Resolver[F, A, B])(implicit
      tpe: => Out[F, B]
  ): OutArr[F, A, G[A], B] =
    OutArr(tpe, identity, resolver)

  def arrType[A](implicit tpe: => In[A]): In[Seq[A]] = InArr[A, Seq[A]](tpe, _.asRight)

  implicit class ResolverSyntax[F[_], I, O](val resolver: Resolver[F, I, O]) extends AnyVal {
    def andThen[O2](next: Resolver[F, O, O2]): Resolver[F, I, O2] =
      CompositionResolver(resolver, next)

    def streamMap[O2](f: O => fs2.Stream[F, IorNec[String, O2]]): Resolver[F, I, O2] =
      resolver.andThen(StreamResolver(f))

    def fallibleMap[O2](f: O => F[Ior[String, O2]]): Resolver[F, I, O2] =
      resolver.andThen(FallibleResolver[F, O, O2](f))

    def evalMap[O2](f: O => F[O2]): Resolver[F, I, O2] =
      resolver.andThen(EffectResolver[F, O, O2](f))

    def map[O2](f: O => O2): Resolver[F, I, O2] =
      resolver.andThen(PureResolver[F, O, O2](f))
  }

  implicit class BatchResolverSyntax[F[_], K, V](val batchResolver: BatchResolver[F, Set[K], Map[K, V]]) extends AnyVal {
    def one: BatchResolver[F, K, Option[V]] = batchResolver.contramap[K](Set(_)).mapBoth { case (k, m) => m.get(k) }
  }

  implicit class TypeSyntax[F[_], A](val tpe: Type[F, A]) extends AnyVal {
    def implements[B](pf: PartialFunction[B, A])(implicit interface: => Interface[F, B]): Type[F, A] =
      tpe.copy(implementations = Implementation(Eval.later(interface))(pf.lift) :: tpe.implementations)

    def subtypeOf[B](implicit ev: A <:< B, tag: ClassTag[A], interface: => Interface[F, B]): Type[F, A] =
      implements[B] { case a: A => a }(interface)

    def addFields(xs: (String, Field[F, ? >: A, ?, ?])*) =
      tpe.copy(fields = tpe.fields concat xs.toList)
  }

  implicit class InterfaceSyntax[F[_], A](val tpe: Interface[F, A]) extends AnyVal {
    def implements[B](pf: PartialFunction[B, A])(implicit interface: => Interface[F, B]): Interface[F, A] =
      tpe.copy(implementations = Implementation(Eval.later(interface))(pf.lift) :: tpe.implementations)

    def subtypeOf[B](implicit ev: A <:< B, tag: ClassTag[A], interface: => Interface[F, B]): Interface[F, A] =
      implements[B] { case a: A => a }(interface)

    def addFields(xs: (String, Field[F, ? >: A, ?, ?])*) =
      tpe.copy(fields = tpe.fields concat xs.toList)
  }

  implicit class UnionSyntax[F[_], A](val tpe: Union[F, A]) extends AnyVal {
    def variant[B](pf: PartialFunction[A, B])(implicit innerTpe: => Type[F, B]): Union[F, A] =
      tpe.copy(types = Variant[F, A, B](Eval.later(innerTpe))(pf.lift) :: tpe.types)

    def subtype[B: ClassTag](implicit ev: B <:< A, innerTpe: => Type[F, B]): Union[F, A] =
      variant[B] { case a: B => a }(innerTpe)
  }

  final case class PartiallyAppliedUnion1[F[_], A](name: String, hd: Variant[F, A, ?]) {
    def variant[B](pf: PartialFunction[A, B])(implicit innerTpe: => Type[F, B]): Union[F, A] =
      Union[F, A](name, NonEmptyList.of(hd, Variant[F, A, B](Eval.later(innerTpe))(pf.lift)), None)

    def subtype[B: ClassTag](implicit ev: B <:< A, innerTpe: => Type[F, B]): Union[F, A] =
      variant[B] { case a: B => a }(innerTpe)
  }

  final case class PartiallyAppliedUnion0[F[_], A](name: String) extends AnyVal {
    def variant[B](pf: PartialFunction[A, B])(implicit innerTpe: => Type[F, B]): PartiallyAppliedUnion1[F, A] =
      PartiallyAppliedUnion1[F, A](name, Variant[F, A, B](Eval.later(innerTpe))(pf.lift))

    def subtype[B: ClassTag](implicit ev: B <:< A, innerTpe: => Type[F, B]): PartiallyAppliedUnion1[F, A] =
      variant[B] { case a: B => a }(innerTpe)
  }
}
