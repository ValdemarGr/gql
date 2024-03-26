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
import scala.reflect.ClassTag
import gql.ast._

class DirectiveAlg[F[_], C](
    positions: Map[String, List[Position[F, ?]]],
    ap: ArgParsing[C]
) {
  type G[A] = Alg[C, A]
  val G = Alg.Ops[C]

  def parseArg[P[x] <: Position[F, x], A](p: P[A], args: List[(String, Value[AnyValue, List[C]])], context: List[C]): G[A] = {
    p.directive.arg match {
      case EmptyableArg.Lift(a)               => ap.decodeArg(a, args.toMap, ambigiousEnum = false, context)
      case EmptyableArg.Empty if args.isEmpty => G.unit
      case EmptyableArg.Empty                 => G.raise(s"Directive '${p.directive.name}' does not expect arguments", context)
    }
  }

  def getDirective[P[x] <: Position[F, x]](name: String, context: List[C])(pf: PartialFunction[Position[F, ?], P[?]]): Alg[C, P[?]] =
    positions.get(name) match {
      case None => G.raise(s"Couldn't find directive '$name'", context)
      case Some(d) =>
        val p = d.collectFirst(pf)
        G.raiseOpt(p, s"Directive '$name' cannot appear here", context)
    }

  case class ParsedDirective[A, P[x] <: Position[F, x]](p: P[A], a: A)
  def parseProvided[P[x] <: Position[F, x]](
      directives: Option[QueryAst.Directives[C, AnyValue]],
      context: List[C]
  )(pf: PartialFunction[Position[F, ?], P[?]]): Alg[C, List[ParsedDirective[?, P]]] =
    directives.map(_.nel.toList).getOrElse(Nil).parTraverse { d =>
      getDirective[P](d.name, context)(pf).flatMap { p =>
        // rigid type variable inference help
        def go[A](p: P[A]): G[ParsedDirective[A, P]] =
          parseArg(
            p,
            d.arguments.map(_.nel.toList).getOrElse(Nil).map(a => a.name -> a.value.map(List(_))),
            context
          ).map(ParsedDirective(p, _))

        go(p)
      }
    }

  def parseSchema[P[x] <: SchemaPosition[F, x]](
    sd: SchemaDirective[F, P],
    ctx: List[C]
  ): G[ParsedDirective[?, P]] = {
    parseArg(sd.position, sd.args.map{ case (k, v) => k -> v.map(_ => List.empty[C]) }, ctx).map{ a =>
      // unfortunate
      ParsedDirective(sd.position.asInstanceOf[P[Any]], a) 
    }
  }

  def parseProvidedSubtype[P[x] <: Position[F, x]](
      directives: Option[QueryAst.Directives[C, AnyValue]],
      context: List[C]
  )(implicit CT: ClassTag[P[Any]]): Alg[C, List[ParsedDirective[?, P]]] = {
    val pf: PartialFunction[Position[F, ?], P[?]] = PartialFunction
      .fromFunction(identity[Position[F, ?]])
      .andThen(x => CT.unapply(x))
      .andThen { case Some(x) => x: P[?] }
    parseProvided[P](directives, context)(pf)
  }

  def parseSchemaDirective[P[x] <: SchemaPosition[F, x]](sd: ast.SchemaDirective[F, P], context: List[C]): G[ParsedDirective[?, P]] = {
    // rigid type variable inference help
    def go[A](p: P[A]): G[ParsedDirective[A, P]] =
      parseArg(p, sd.args.map { case (k, v) => k -> v.map(_ => List.empty[C]) }, context)
        .map(ParsedDirective(p, _))

    go(sd.position: P[?]).widen[ParsedDirective[?, P]]
  }

  def field[I, O](fi: MergedFieldInfo[F, C], field: Field[F, I, O]): G[List[(Field[F, I, _], MergedFieldInfo[F, C])]] =
    (
      parseProvidedSubtype[Position.Field[F, *]](fi.directives, List(fi.caret))
    ).flatMap{ xs =>
      val ys = /*field.directives.map(sd => ) ++*/ xs
      ys.foldLeftM[G, List[(Field[F, I, ?], MergedFieldInfo[F, C])]](List((field, fi))) { case (xs, prov) =>
        xs.parFlatTraverse { case (f: Field[F, I, ?], fi) =>
          G.raiseEither(prov.p.handler(prov.a, f, fi), List(fi.caret))
        }
      }
    }

  def foldDirectives[P[x] <: Position[F, x]]: DirectiveAlg.PartiallyAppliedFold[F, C, P] =
    new DirectiveAlg.PartiallyAppliedFold[F, C, P] {
      override def apply[H[_]: Traverse, A](directives: Option[QueryAst.Directives[C, AnyValue]], context: List[C])(base: A)(
          f: PartialFunction[(A, P[Any], QueryAst.Directive[C, AnyValue]), Alg[C, H[A]]]
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
    )(base: A)(f: PartialFunction[(A, P[Any], QueryAst.Directive[C, AnyValue]), Alg[C, H[A]]])(implicit H: Monad[H]): Alg[C, H[A]]
  }
}
