package gql.client

import cats.data._
import cats._
import cats.implicits._

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

