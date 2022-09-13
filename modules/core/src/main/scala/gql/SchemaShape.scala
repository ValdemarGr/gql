package gql

import cats._
import cats.implicits._
import cats.mtl._
import cats.data._
import gql.out._

final case class SchemaShape[F[_], Q](
    query: Obj[F, Q]
    // mutation: Output.Obj[F, M],
    // subscription: Output.Obj[F, M],
) {
  lazy val discover = SchemaShape.discover[F, Q](this)
}

object SchemaShape {
  final case class DiscoveryState[F[_]](
      inputs: Map[String, ToplevelInput[Any]],
      outputs: Map[String, gql.out.Toplevel[F, Any]],
      // Key is the type, values are the interfaces it implements
      interfaceImplementations: Map[String, Set[String]]
  )

  def discover[F[_], Q](shape: SchemaShape[F, Q]): DiscoveryState[F] = {
    def inputNotSeen[G[_], A](
        tl: ToplevelInput[Any]
    )(ga: G[A])(implicit G: Monad[G], S: Stateful[G, DiscoveryState[F]], M: Monoid[A]): G[A] =
      S.get.flatMap { s =>
        if (s.inputs.contains(tl.name)) G.pure(M.empty)
        else S.modify(_.copy(inputs = s.inputs + (tl.name -> tl))) *> ga
      }

    def outputNotSeen[G[_], A](
        tl: gql.out.Toplevel[F, Any]
    )(ga: G[A])(implicit G: Monad[G], S: Stateful[G, DiscoveryState[F]], M: Monoid[A]): G[A] =
      S.get.flatMap { s =>
        if (s.outputs.contains(tl.name)) G.pure(M.empty)
        else S.modify(_.copy(outputs = s.outputs + (tl.name -> tl))) *> ga
      }

    def goOutput[G[_]](out: Output[F, Any])(implicit G: Monad[G], S: Stateful[G, DiscoveryState[F]]): G[Unit] =
      out match {
        case gql.out.Arr(of) => goOutput[G](of)
        case gql.out.Opt(of) => goOutput[G](of)
        case t: gql.out.Toplevel[F, Any] =>
          outputNotSeen(t) {
            def handleFields(o: ObjLike[F, Any]): G[Unit] =
              o.fieldsList.traverse_ { case (_, x) =>
                goOutput[G](x.output.value) >>
                  x.args.entries.traverse_(x => goInput[G](x.input.asInstanceOf[Input[Any]]))
              }

            t match {
              case o @ gql.out.Obj(_, _) => handleFields(o)
              case o @ gql.out.Interface(_, instances, _) =>
                S.modify { s =>
                  val newMap =
                    instances.keySet.toList.foldLeft(s.interfaceImplementations) { case (m, instance) =>
                      val y = m.get(instance) match {
                        case None    => Set(o.name)
                        case Some(x) => x + o.name
                      }
                      m + (instance -> y)
                    }
                  s.copy(interfaceImplementations = newMap)
                } >>
                  handleFields(o) >>
                  instances.values.toList.traverse_(inst => handleFields(inst.ol))
              case gql.out.Union(_, instances) =>
                instances.toList.traverse_(inst => goOutput[G](inst.ol))
              case _ => G.unit
            }
          }
      }

    def goInput[G[_]](in: Input[Any])(implicit G: Monad[G], S: Stateful[G, DiscoveryState[F]]): G[Unit] =
      in match {
        case Input.Arr(of) => goInput[G](of)
        case Input.Opt(of) => goInput[G](of)
        case t: ToplevelInput[Any] =>
          inputNotSeen(t) {
            t match {
              case Input.Obj(_, fields) =>
                fields.entries.traverse_(x => goInput[G](x.input.asInstanceOf[Input[Any]]))
              case _ => G.unit
            }
          }
      }

    goOutput[State[DiscoveryState[F], *]](shape.query).runS(DiscoveryState(Map.empty, Map.empty, Map.empty)).value
  }

  def validate[F[_], Q](schema: SchemaShape[F, Q]) = {
  }
}
