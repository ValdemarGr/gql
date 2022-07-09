package gql

/*
 * The executable GraphQL query is a tree of potentially effectful functions to run.
 * The optimizers job is to pick the most efficient way to evaluate this tree.
 * Queries are batched by running all nodes on the same level of the tree (potentially) together.
 * As an example consider rewriting the following query:
 *            A____
 *           / \   \
 *          B  D   D
 *        / \  
 *       D  D 
 * To:
 *            A 
 *           / \
 *          B Defer
 *        / \  / \
 *       D  D D   D
 * Now we can run nodes D together.
 */
object Optimizer {
  // def optimize[F[_]]()
}
