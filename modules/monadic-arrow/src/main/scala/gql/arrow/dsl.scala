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
package gql.arrow

import gql.resolver._
import gql._
import cats.data._
import org.tpolecat.sourcepos.SourcePos
import gql.dsl.FieldBuilder

trait ResolverArrowDsl[F[_]] extends Language[Resolver[F, *, *]] {
  def argument[A](arg: Arg[A])(implicit sp: SourcePos): Decl[Var[A]] =
    liftUnitArrow(Resolver.argument[F, Unit, A](arg))

  def meta[A](implicit sp: SourcePos): Decl[Var[FieldMeta[F]]] =
    liftUnitArrow(Resolver.meta[F, Unit])

  implicit class FieldBuilderOps[A](private val fb: FieldBuilder[F, A]) {
    def proc[B](f: Var[A] => Decl[Var[B]])(implicit sp: SourcePos, tpe: => ast.Out[F, B]): ast.Field[F, A, B] =
      fb.from(procFull(f))(tpe)
  }

  implicit class VarResolverOps[A](private val v: Var[A]) {
    def evalMap[B](f: A => F[B])(implicit sp: SourcePos): Decl[Var[B]] =
      v(_.evalMap(f))
    def emap[B](f: A => Ior[String, B])(implicit sp: SourcePos): Decl[Var[B]] =
      v(_.emap(f))
    def streamMap[B](f: A => fs2.Stream[F, B])(implicit sp: SourcePos): Decl[Var[B]] =
      v(_.streamMap(f))
  }
}

object dsl {
  def apply[F[_]]: ResolverArrowDsl[F] = new ResolverArrowDsl[F] {}
}
