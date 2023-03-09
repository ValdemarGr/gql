import cats.free.Free


trait Selection2[A]

object Selection2 {
    trait Query[A]

    case class SelectedField[A](
        fieldName: String,
        alias: Option[String],
        subQuery: Query[A]
    )

    sealed trait FreeApply[F[_], A]

    object FreeApply {
        case class Ap[F[_], A, B](ff: FreeApply[F, A => B], fa: FreeApply[F, A]) extends FreeApply[F, B]
        case class Lift[F[_], A](fa: F[A]) extends FreeApply[F, A]
    }

    import cats.data._
    case class Selection[A](
        fields: NonEmptyChain[SelectedField[?]],
        decode: Map[String, ?] => Either[String, A]
    ) extends Query[A]
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
    trait InlineFragment
}