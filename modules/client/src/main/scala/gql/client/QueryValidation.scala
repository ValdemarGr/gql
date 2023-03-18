package gql.client

import gql.parser.TypeSystemAst._
import cats._
import cats.implicits._
import cats.data._
import cats.mtl.Stateful
import gql.ast._
import gql.parser.{Value => V, Const}
import gql.ModifierStack
import gql.InverseModifierStack
import gql.InverseModifier
import gql.resolver.Resolver
import gql.Arg
import gql.ArgValue

object QueryValidation {
  def raise[F[_], A](msg: String)(implicit F: MonadError[F, NonEmptyChain[String]]): F[A] =
    F.raiseError(NonEmptyChain.one(msg))

  type CacheValue = Either[OutToplevel[fs2.Pure, Unit], InToplevel[Unit]]
  type Cache = Map[String, CacheValue]

  // Stubs the whole ast
  def liftAst[F[_]](ast: Map[String, TypeDefinition])(implicit
      F: MonadError[F, NonEmptyChain[String]],
      S: Stateful[F, Cache]
  ) = {
    def convetTd(td: TypeDefinition): F[CacheValue] = {
      td match {
        case e: TypeDefinition.EnumTypeDefinition =>
          Enum[Unit](
            e.name,
            e.values.map(evd => evd.name -> EnumValue[Unit](())),
            None
          )
        case s: TypeDefinition.ScalarTypeDefinition =>
          Scalar[Unit](
            s.name,
            _ => V.NullValue(),
            _ => Left("Not implemented")
          )
        case i: TypeDefinition.InterfaceTypeDefinition =>
          Interface[fs2.Pure, Unit](
            i.name,
            i.fieldDefinitions.map { f =>
              f.argumentsDefinition.map { a =>
                foldInputStack(ModifierStack.fromType(a.tpe).invert).map{ in =>
                    ???
                    //Arg.make(ArgValue.)
                }
                ???
              //Arg.make(ArgValue(a.name, Eval.later()))
              }
              val t = ModifierStack.fromType(f.tpe).invert

              ???
            },
            Nil
          )
      }
      ???
    }

    def convert(tn: String): F[CacheValue] = S.inspect(_.get(tn)).flatMap {
        case Some(cached) => F.pure(cached)
        case None =>
          ast.get(tn) match {
            case None    => raise[F, CacheValue](s"Typename ${tn} not found")
            case Some(x) => convetTd(x)
          }
      }

    def foldOutputStack(ms: InverseModifierStack[String]): F[Out[fs2.Pure, ?]] =
      ms.modifiers match {
        case InverseModifier.List :: xs =>
          foldOutputStack(InverseModifierStack(xs, ms.inner)).map { case e: Out[fs2.Pure, a] =>
            OutArr[fs2.Pure, a, Unit, a](
              e,
              _ => Seq.empty[a],
              Resolver.id[fs2.Pure, a]
            )
          }
        case InverseModifier.Optional :: xs =>
          foldOutputStack(InverseModifierStack(xs, ms.inner)).map { case e: Out[fs2.Pure, a] =>
            OutOpt[fs2.Pure, a, a](
              e,
              Resolver.id[fs2.Pure, a]
            )
          }
        case Nil =>
          convert(ms.inner).flatMap {
            case Left(x)  => F.pure(x)
            case Right(x) => raise(s"Expected output type, got input type ${x.name}")(F)
          }
      }

    def foldInputStack(ms: InverseModifierStack[String]): F[In[?]] =
      ms.modifiers match {
        case InverseModifier.List :: xs =>
          foldInputStack(InverseModifierStack(xs, ms.inner)).map { case e: In[a] =>
            InArr[a, Unit](
              e,
              _ => Left("Not implemented")
            )
          }
        case InverseModifier.Optional :: xs =>
          foldInputStack(InverseModifierStack(xs, ms.inner)).map { case e: In[a] =>
            InOpt[a](e)
          }
        case Nil =>
          convert(ms.inner).flatMap {
            case Left(x)  => raise(s"Expected input type, got output type ${x.name}")(F)
            case Right(x) => F.pure(x)
          }
      }

    ast.get("Query") match {
      case None => raise("Query type not found")(F)
      case Some(x) =>
        x
        ???
    }
  }
}
