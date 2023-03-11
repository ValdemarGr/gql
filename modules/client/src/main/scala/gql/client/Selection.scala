package gql.client

import cats.data._
import cats._
import cats.implicits._
import gql.std.FreeApply

trait Selection2[A]

object Selection2 {
  trait Query[A]

  final case class SelectedField[A](
      fieldName: String,
      alias: Option[String],
      subQuery: Eval[Query[A]]
  ) extends Query[A]

  trait Selection3[A]

  object Selection3 {
    case class Ap[A, B](ff: Selection3[A => B], fa: Selection3[A]) extends Selection3[B]
    case class Lift[A](fa: Query[A]) extends Selection3[A]
    case class Guard[A, B](fa: Selection3[A], f: A => EitherNec[String, B]) extends Selection3[B]
  }
  case class Selection[A](
      fields: NonEmptyChain[Query[?]],
      decode: Map[String, ?] => EitherNec[String, A]
  )

  object Selection {
    def one[A](q: Query[A]): Selection[A] =
      Selection(NonEmptyChain.one(q), m => Right(m.values.head.asInstanceOf[A]))

    implicit val applyForSelection: Apply[Selection] = new Apply[Selection] {
      override def map[A, B](fa: Selection[A])(f: A => B): Selection[B] =
        Selection(fa.fields, fa.decode.andThen(_.map(f)))

      override def ap[A, B](ff: Selection[A => B])(fa: Selection[A]): Selection[B] =
        Selection(ff.fields ++ fa.fields, m => (ff.decode(m), fa.decode(m)).parMapN(_(_)))
    }
  }

  final case class Emap[A, B](
      q: Query[A],
      f: A => EitherNec[String, B]
  ) extends Query[B]

  final case class Fragment[A](
      name: String,
      ons: NonEmptyChain[String],
      selection: Selection[A]
  ) extends Query[Option[A]] /* {
    def required: Emap[Option[A], A] =
      Emap(
        this,
        _.toRightNec(
          s"Fragment '$name' was not required, but none of the types ${ons.map(str => s"'${str}'").mkString_(", ")} were present."
        )
      )
  }*/

  final case class InlineFragment[A](
      ons: NonEmptyChain[String],
      selection: Selection[A]
  ) extends Query[Option[A]] /* {
    def required: Emap[Option[A], A] =
      Emap(
        this,
        _.toRightNec(
          s"Inline fragment was not required, but none of the types ${ons.map(str => s"'${str}'").mkString_(", ")} were present."
        )
      )
  }*/

  final case class Terminal[A](decoder: io.circe.Decoder[A]) extends Query[A]
  object Terminal {
    implicit def terminalDecoder[A](implicit decoder: io.circe.Decoder[A]): Terminal[A] = Terminal(decoder)
  }

  object dsl {
    def sel[A](fieldName: String, alias: Option[String] = None)(implicit q: => Query[A]): Selection[A] =
      Selection(
        NonEmptyChain.one(SelectedField(fieldName, alias, Eval.later(q))),
        m => Right(m(alias.getOrElse(fieldName)).asInstanceOf[A])
      )

    def inlineFrag[A](onHd: String, onTl: String*)(selection: Selection[A]): InlineFragment[A] =
      InlineFragment(NonEmptyChain.of(onHd, onTl: _*), selection)

    def fragment[A](name: String, onHd: String, onTl: String*)(selection: Selection[A]): Fragment[A] =
      Fragment(name, NonEmptyChain.of(onHd, onTl: _*), selection)

    // def firstSome[A](hd: Fragment[Option[A]], fragments: Fragment[Option[A]]*): Selection[Option[A]] = {
    //   val allFrags = NonEmptyChain.of(hd, fragments: _*)
    //   Selection[Option[A]](
    //     allFrags,
    //     { case (m: Map[String, Option[A]] @unchecked) => Right(allFrags.flatMap(_.ons).collectFirstSome(m.get(_).flatten)) }
    //   )
    //   // val fragMap = allFrags.flatMap(f => f.on -> f).toNem
    //   ???
    // }
  }
  /*
   * trait C
   *
   * case class Data(d: String)
   *
   * implicit val data = sel[String]("d").map(Data.apply)
   *
   * case class A(caseId: UUID, names: List[String], d: Data) extends C
   *
   * val a: Selection[A] = (
   *   sel[UUID]("caseId"),
   *   sel[List[String]]("names"),
   *   sel[Data]("data")
   * ).mapN(A.apply)
   *
   * case class B(entityId: UUID, numbers: List[Int]) extends C
   *
   * val b: Selection[B](
   *   sel[UUID]("entityId"),
   *   sel[List[Int]]("numbers")
   * ).mapN(B.apply)
   *
   * val inlineFragB: InlineFrag[B] = inlineFragment("B", b)
   *
   * val fragA: Fragment[A] = fragment("MyCoolFrag", "A", a)
   *
   * Selection[(String, C)] = (
   *   sel[String]("__typename"),
   *   oneOf(fragA, fragA): Selection[Option[C]]
   * ).tupled
   */
}

object Attempt2 {
  /*
   * A selection is either:
   *  - a field { name }
   *  - a fragment
   *  - an inline fragment
   */
  sealed trait Selection[A]

  object Selection {
    final case class Field[A](
        fieldname: String,
        alias: Option[String],
        subQuery: FieldCont[A]
    ) extends Selection[A]

    final case class Fragment[A](
        name: String,
        on: String,
        matchAlso: Chain[String],
        subSelection: Query[A]
    ) extends Selection[Option[A]]

    final case class InlineFragment[A](
        on: String,
        matchAlso: Chain[String],
        subSelection: Query[A]
    ) extends Selection[Option[A]]
  }

  /*
   * A field cont is a continuation of a field, either:
   *  - a terminal scalar/enum decoder
   *  - a group of fields
   */
  sealed trait FieldCont[A]
  object FieldCont {
    implicit def terminalDecoder[A](implicit decoder: io.circe.Decoder[A]): FieldCont[A] = Terminal(decoder)
  }

  final case class Terminal[A](decoder: io.circe.Decoder[A]) extends FieldCont[A]

  final case class Query[A](impl: Query.Impl[?, A]) extends FieldCont[A] {
    def vmap[B](f: A => ValidatedNec[String, B]): Query[B] = Query(impl.vmap(f))

    def emap[B](f: A => Either[String, B]): Query[B] = Query(impl.emap(f))
  }

  object Query {
    final case class Impl[A, B](
        underlying: FreeApply[Selection, A],
        emap: A => ValidatedNec[String, B]
    ) {
      def vmap[C](f: B => ValidatedNec[String, C]): Impl[A, C] = copy(emap = emap.andThen(_ andThen f))

      def emap[C](f: B => Either[String, C]): Impl[A, C] =
        copy(emap = emap.andThen(_.toEither.flatMap(f(_).leftMap(NonEmptyChain.one(_))).toValidated))
    }

    def lift[A](sel: Selection[A]): Query[A] =
      Query(Impl[A, A](FreeApply.lift(sel), _.validNec))

    implicit val applyForQuery: Apply[Query] = {
      new Apply[Query] {
        override def map[A, B](fa: Query[A])(f: A => B): Query[B] = Query {
          fa.impl match {
            case g: Impl[a, A] => Impl[a, B](g.underlying, g.emap.andThen(_.map(f)))
          }
        }

        override def ap[A, B](ff: Query[A => B])(fa: Query[A]): Query[B] = Query {
          (ff.impl, fa.impl) match {
            case (g1: Impl[a1, A => B], g2: Impl[a2, A]) =>
              Impl[ValidatedNec[String, B], B](
                (g1.underlying.map(g1.emap), g2.underlying.map(g2.emap)).mapN((_, _).mapN(_(_))),
                x => x
              )
          }
        }
      }
    }
  }

  object dsl {
    def sel[A](fieldName: String, alias: Option[String] = None)(implicit cont: FieldCont[A]): Query[A] =
      Query.lift(Selection.Field(fieldName, alias, cont))

    def inlineFrag[A](on: String, matchAlso: String*)(implicit q: Query[A]): Query[Option[A]] =
      Query.lift(Selection.InlineFragment(on, Chain.fromSeq(matchAlso), q))

    def fragment[A](name: String, on: String, matchAlso: String*)(implicit q: Query[A]): Query[Option[A]] =
      Query.lift(Selection.Fragment(name, on, Chain.fromSeq(matchAlso), q))

    def oneOf[A](hd: Query[Option[A]], tl: Query[Option[A]]*) =
        NonEmptyChain.of(hd, tl: _*)
            .nonEmptySequence
            .emap{ xs =>
                val hd = xs.collect { case Some(x) => x }
                if (hd.length > 1) Left("More than one sub-query matched")
                else Right(hd.headOption)
            }

    implicit class SyntaxForOptionalQuery[A](q: Query[Option[A]]) {
        def required: Query[A] = 
            q.emap(_.toRight("Required field was null"))
    }

    case class Comb(str: String)
    implicit val o = (
        sel[String]("hey"),
        sel[String]("med"),
        sel[String]("dig"),
    ).mapN(_ + _ + _).map(Comb.apply)

    val o2: Query[String] = (
        sel[Int]("age"),
        sel[Comb]("comb")
    ).mapN(_.toString() + _.str)

    val o3 = (
        sel[Int]("age"),
        sel("comb") {
            (
                sel[String]("hey"),
                sel[String]("med"),
                sel[String]("dig"),
            ).mapN(_ + _ + _).map(Comb.apply)
        }
    ).mapN(_.toString() + _.str)

    val o4: Query[(String, Option[Comb], Option[Comb], Comb)] = (
        sel[String]("__typename"),
        fragment[Comb]("MyCoolFrag", "A", "AlsoB"),
        oneOf(
            fragment[Comb]("MyCoolFrag", "A", "AlsoB"),
            fragment[Comb]("MyCoolFrag", "A", "AlsoB")
        ),
        fragment[Comb]("MyCoolFrag", "A", "AlsoB").required
    ).tupled
  }
}
