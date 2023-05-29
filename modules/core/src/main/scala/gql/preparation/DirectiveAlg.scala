package gql.preparation

import gql.parser._
import gql.Position
import cats._
import cats.implicits._
import gql.DirectiveArg

trait DirectiveAlg[F[_], G[_], C] {
  def parseArg[P[x] <: Position[G, x], A](p: P[A], args: Option[QueryAst.Arguments[C, AnyValue]], context: List[C]): F[A]

  def foldDirectives[P[x] <: Position[G, x]]: DirectiveAlg.PartiallyAppliedFold[F, G, C, P]
}

object DirectiveAlg {
  trait PartiallyAppliedFold[F[_], G[_], C, P[x] <: Position[G, x]] {
    def apply[H[_]: Traverse, A](
        directives: Option[QueryAst.Directives[C, AnyValue]], context: List[C]
    )(base: A)(f: PartialFunction[(A, P[?], QueryAst.Directive[C, AnyValue]), F[H[A]]])(implicit H: Monad[H]): F[H[A]]
  }

  def forPositions[F[_], G[_], C](
      positions: Map[String, List[Position[G, ?]]]
  )(implicit
      EA: ErrorAlg[F, C],
      AP: ArgParsing[F, C],
      F: Monad[F]
  ): DirectiveAlg[F, G, C] = {
    import EA._
    new DirectiveAlg[F, G, C] {
      override def parseArg[P[x] <: Position[G, x], A](p: P[A], args: Option[QueryAst.Arguments[C, AnyValue]], context: List[C]): F[A] = {
        p.directive.arg match {
          case DirectiveArg.Empty => 
            args match {
              case Some(_) => raise(s"Directive '${p.directive.name}' does not expect arguments", context)
              case None => F.unit
            }
          case DirectiveArg.WithArg(a) => 
            val argFields = args.toList.flatMap(_.nel.toList).map(a => a.name -> a.value.map(List(_))).toMap
            AP.decodeArg(a, argFields, ambigiousEnum = false, context)
        }
      }

      override def foldDirectives[P[x] <: Position[G, x]]: PartiallyAppliedFold[F, G, C, P] =
        new PartiallyAppliedFold[F, G, C, P] {
          override def apply[H[_]: Traverse, A](directives: Option[QueryAst.Directives[C, AnyValue]], context: List[C])(base: A)(
              f: PartialFunction[(A, P[?], QueryAst.Directive[C, AnyValue]), F[H[A]]]
          )(implicit H: Monad[H]): F[H[A]] = {
            def foldNext(rest: List[QueryAst.Directive[C, AnyValue]], accum: A): F[H[A]] =
              rest match {
                case Nil => F.pure(H.pure(accum))
                case x :: xs =>
                  val name = x.name
                  positions.get(name) match {
                    case None => raise(s"Couldn't find directive '$name'", context)
                    case Some(d) =>
                      val faOpt = d.map(p => (accum, p, x)).collectFirst { case f(fa) => fa }
                      raiseOpt(faOpt, s"Directive '$name' cannot appear here", context).flatten
                      .flatMap(_.flatTraverse(a => foldNext(xs, a)))
                  }
              }

            foldNext(directives.map(_.nel.toList).getOrElse(Nil), base)
          }
        }
    }
  }
}
