package gql.preparation

object SubstitueVariables {
  def substSel[F[_], C, A](
      fa: Selection[F, A, Stage.Compilation[C]]
  ): Alg[C, Selection[F, A, Stage.Execution]] = ???
}
