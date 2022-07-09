package gql

import gql.Output._
import cats.implicits._
import cats._
import cats.mtl._
import cats.data._

object Render {
  final case class RenderState[F[_]](
      discoveredTypes: Set[String],
      toRender: List[ToplevelOutput[F, Any]]
  )

  def maybeAdd[F[_], G[_]](tpe: ToplevelOutput[G, Any])(implicit
      F: Monad[F],
      D: Defer[F],
      S: Stateful[F, RenderState[G]]
  ) =
    S.get.flatMap { state =>
      if (state.discoveredTypes.contains(tpe.name)) F.unit
      else {
        S.set(
          RenderState(
            state.discoveredTypes + tpe.name,
            tpe :: state.toRender
          )
        )
      }
    }

  def renderToplevelTypeInline[F[_], G[_]](tl: ToplevelOutput[G, Any])(implicit
      F: Monad[F],
      D: Defer[F],
      S: Stateful[F, RenderState[G]]
  ): F[String] = maybeAdd[F, G](tl).as(tl.name)

  def renderField[F[_], G[_]](o: Output[G, Any], optional: Boolean = false)(implicit
      F: Monad[F],
      D: Defer[F],
      S: Stateful[F, RenderState[G]]
  ): F[String] = {
    o match {
      case Opt(of) => renderField[F, G](of, optional = true)
      case Arr(of) =>
        renderField[F, G](of, optional = false)
          .map(x => s"[$x]")
          .map(x => if (optional) x else s"$x!")
      case tl: ToplevelOutput[G, Any] =>
        renderToplevelTypeInline[F, G](tl)
          .map(x => if (optional) x else s"$x!")
    }
  }

  def renderFields[F[_], G[_]](xs: NonEmptyList[(String, Fields.Field[G, _, _])])(implicit
      F: Monad[F],
      D: Defer[F],
      S: Stateful[F, RenderState[G]]
  ) = D.defer {
    xs.traverse { case (name, field) =>
      renderField[F, G](field.output.value).map(fv => s"$name: $fv")
    }
  }

  def renderToplevelType[F[_], G[_]](tpe: ToplevelOutput[G, Any], interfaceInstances: Map[String, List[String]])(implicit
      F: Monad[F],
      D: Defer[F],
      S: Stateful[F, RenderState[G]]
  ) = tpe match {
    case Scalar(name, encoder) =>
      F.pure(s"""
        |scalar $name""".stripMargin)
    case Enum(name, encoder) =>
      F.pure(s"""
        |enum $name {
        ${encoder.toList.map(x => s"|  $x").mkString("\n")}
        |}""".stripMargin)
    case Union(name, types) =>
      F.pure(s"""
        |union $name = ${types.keys.mkString_(" | ")}""".stripMargin) <*
        types.toList.traverse_(inst => maybeAdd[F, G](inst.ol))
    case Interface(name, instances, fields) =>
      val impls = interfaceInstances.get(name).map(_.mkString_("implements ", " & ", " ")).mkString
      renderFields[F, G](fields).map { fields =>
        s"""
            |interface $name $impls{
            ${fields.map(x => s"|  $x").mkString_(",\n")}
            |}""".stripMargin
      } <* instances.toList.traverse_ { case (_, inst) => maybeAdd[F, G](inst.ol) }
    case Obj(name, fields) =>
      val impls = interfaceInstances.get(name).map(_.mkString_("implements ", " & ", " ")).mkString
      renderFields[F, G](fields).map { fields =>
        s"""
            |type $name $impls{
            ${fields.map(x => s"|  $x").mkString_(",\n")}
            |}""".stripMargin
      }
  }

  def renderAll[F[_], G[_]](interfaceInstances: Map[String, List[String]])(implicit
      F: Monad[F],
      D: Defer[F],
      S: Stateful[F, RenderState[G]]
  ): F[String] =
    D.defer {
      S.inspect(_.toRender).flatMap {
        case x :: xs =>
          S.modify(_.copy(toRender = xs)) >> renderToplevelType[F, G](x, interfaceInstances).flatMap { prefix =>
            renderAll[F, G](interfaceInstances).map(suffix => s"$prefix\n$suffix")
          }
        case Nil => F.pure("")
      }
    }

  final case class InterfaceDiscovery(
      alreadyChecked: Set[String]
  )

  def discoverInterfaceInstances[F[_], G[_]](x: ObjectLike[G, _])(implicit
      F: Monad[F],
      D: Defer[F],
      S: Stateful[F, InterfaceDiscovery]
  ): F[Chain[(String, String)]] = D.defer {
    S.inspect(_.alreadyChecked).flatMap { cc =>
      if (cc.contains(x.name)) F.pure(Chain.nil)
      else {
        S.set(InterfaceDiscovery(cc + x.name)) >> {
          x match {
            case Interface(name, instances, fields) =>
              val here = Chain.fromSeq(instances.keySet.map(inst => inst -> name).toSeq)
              Chain
                .fromSeq(fields.toList)
                .map { case (_, f) => f.output.value }
                .collect { case ol: ObjectLike[G, _] => ol }
                .flatTraverse(discoverInterfaceInstances[F, G](_))
                .map(here ++ _)
            case Obj(_, fields) =>
              Chain
                .fromSeq(fields.toList)
                .map { case (_, f) => f.output.value }
                .collect { case ol: ObjectLike[G, _] => ol }
                .flatTraverse(discoverInterfaceInstances[F, G](_))
            case Union(name, types) =>
              Chain
                .fromSeq(types.toList)
                .flatTraverse(x => discoverInterfaceInstances[F, G](x.ol))
          }
        }
      }
    }
  }

  def renderSchema[F[_]](schema: Schema[F, _]): String = {
    type G[A] = StateT[Eval, RenderState[F], A]
    type H[A] = StateT[Eval, InterfaceDiscovery, A]

    val d = discoverInterfaceInstances[H, F](schema.query).runA(InterfaceDiscovery(Set.empty)).value
    val m = d.toList.groupMap { case (k, _) => k } { case (_, v) => v }
    renderAll[G, F](m).runA(RenderState(Set.empty, List(schema.query))).value
  }
}
