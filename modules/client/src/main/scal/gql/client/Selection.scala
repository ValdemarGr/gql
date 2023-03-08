package gql.client

trait Selection[A]

object Selection {
    trait Field[A] {
        def name: String
        def subSelection: Selection[A]
    }

    trait FieldGroup[A] extends Selection[A] {
        def fields: List[Field[?]]
    }

    trait Scalar[A] extends Selection[A] {

    }

    /*
     * trait C
     * 
     * case class A(caseId: UUID, names: List[String]) extends C
     * 
     * val a: Type[A] = tpe(
     *   "A",
     *   (
     *     sel[UUID]("caseId"),
     *     sel[List[String]]("names")
     *   ).mapN(A.apply)
     * )
     * 
     * case class B(entityId: UUID, numbers: List[Int]) extends C
     * 
     * val b: Type[B] = tpe(
     *   "B",
     *   (
     *     sel[UUID]("entityId"),
     *     sel[List[Int]]("numbers")
     *   ).mapN(B.apply)
     * )
     * 
     * val u: Union[C] = a | b
     */
    trait InlineFragment
}