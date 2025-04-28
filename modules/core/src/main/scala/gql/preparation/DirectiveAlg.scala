/*
 * Copyright 2023 Valdemar Grange
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
package gql.preparation

import gql.parser._
import cats._
import cats.implicits._
import gql._

class DirectiveAlg[F[_], C](
    positions: Map[String, List[Position[F, ?]]],
    ap: ArgParsing[C]
) {
  type G[A] = Alg[C, A]
  val G = Alg.Ops[C]

  def parseArg[P[x] <: Position[F, x], A](p: P[A], args: Option[QueryAst.Arguments[C, AnyValue]], context: List[C]): G[A] = {
    p.directive.arg match {
      case EmptyableArg.Empty =>
        args match {
          case Some(_) => G.raise(s"Directive '${p.directive.name}' does not expect arguments", context)
          case None    => G.unit
        }
      case EmptyableArg.Lift(a) =>
        val argFields = args.toList.flatMap(_.nel.toList).map(a => a.name -> a.value.map(List(_)))
        ap.decodeArg(a, argFields, ambigiousEnum = false, context)
    }
  }

  def foldDirectives[P[x] <: Position[F, x]]: DirectiveAlg.PartiallyAppliedFold[F, C, P] =
    new DirectiveAlg.PartiallyAppliedFold[F, C, P] {
      override def apply[H[_]: Traverse, A](directives: Option[QueryAst.Directives[C, AnyValue]], context: List[C])(base: A)(
          f: PartialFunction[(A, Position[F, Any], QueryAst.Directive[C, AnyValue]), Alg[C, H[A]]]
      )(implicit H: Monad[H]): Alg[C, H[A]] = {
        def foldNext(rest: List[QueryAst.Directive[C, AnyValue]], accum: A): Alg[C, H[A]] =
          rest match {
            case Nil => G.pure(H.pure(accum))
            case x :: xs =>
              val name = x.name
              positions.get(name) match {
                case None => G.raise(s"Couldn't find directive '$name'", context)
                case Some(d) =>
                  val faOpt = d.map(p => (accum, p, x)).collectFirst { case f(fa) => fa }
                  G.raiseOpt(faOpt, s"Directive '$name' cannot appear here", context)
                    .flatten
                    .flatMap(_.flatTraverse(a => foldNext(xs, a)))
              }
          }

        foldNext(directives.map(_.nel.toList).getOrElse(Nil), base)
      }
    }

}

object DirectiveAlg {
  trait PartiallyAppliedFold[F[_], C, P[x] <: Position[F, x]] {
    def apply[H[_]: Traverse, A](
        directives: Option[QueryAst.Directives[C, AnyValue]],
        context: List[C]
    )(base: A)(f: PartialFunction[(A, Position[F, Any], QueryAst.Directive[C, AnyValue]), Alg[C, H[A]]])(implicit H: Monad[H]): Alg[C, H[A]]
  }
}
