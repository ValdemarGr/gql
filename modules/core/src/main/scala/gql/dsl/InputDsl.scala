package gql.dsl

import gql.resolver._
import gql.ast._
import gql.parser.{Value => V, Const, QueryAst => QA}
import gql._
import cats.data._
import cats._
import scala.reflect.ClassTag

trait InputDslFull {
  def arg[A](name: String)(implicit tpe: => In[A]): Arg[A] =
    Arg.make[A](ArgValue(name, Eval.later(tpe), None, None))

  def arg[A](name: String, description: String)(implicit tpe: => In[A]): Arg[A] =
    Arg.make[A](ArgValue(name, Eval.later(tpe), None, Some(description)))

  def arg[A](name: String, default: V[Const, Unit])(implicit tpe: => In[A]): Arg[A] =
    Arg.make[A](ArgValue(name, Eval.later(tpe), Some(default), None))

  def arg[A](name: String, default: V[Const, Unit], description: String)(implicit tpe: => In[A]): Arg[A] =
    Arg.make[A](ArgValue(name, Eval.later(tpe), Some(default), Some(description)))

  def argFull[A]: InputDsl.PartiallyAppliedArgFull[A] = new InputDsl.PartiallyAppliedArgFull[A]

  def arged[F[_], I, A](a: Arg[A]) = Resolver.argument[F, I, A](a)

  def value = InputDsl.ValueDsl

  def input[A](
      name: String,
      fields: Arg[A]
  ): Input[A] = Input(name, fields)
}

object InputDsl extends InputDslFull {
  object ValueDsl {
    def scalar[A](value: A)(implicit tpe: => Scalar[A]) =
      tpe.encoder(value)

    def fromEnum[A](value: A)(implicit tpe: => Enum[A]) =
      tpe.revm.get(value).map(enumValue)

    def enumValue(value: String) = V.EnumValue(value)

    def arr(xs: V[Const, Unit]*) = V.ListValue(xs.toList)

    def obj(xs: (String, V[Const, Unit])*) = V.ObjectValue(xs.toList)

    def nullValue = V.NullValue()
  }

  final class PartiallyAppliedArgFull[A](private val dummy: Boolean = false) extends AnyVal {
    def apply[B](name: String, default: Option[V[Const, Unit]], description: Option[String])(
        f: ArgParam[A] => Either[String, B]
    )(implicit tpe: => In[A]): Arg[B] =
      Arg.makeFrom[A, B](ArgValue(name, Eval.later(tpe), default, description))(f)
  }
}
