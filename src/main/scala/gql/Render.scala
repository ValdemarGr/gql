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

  def renderToplevelTypeInline[F[_], G[_]](tl: ToplevelOutput[G, Any])(implicit
      F: Monad[F],
      D: Defer[F],
      S: Stateful[F, RenderState[G]]
  ): F[String] =
    S.get.flatMap { state =>
      val fa =
        if (state.discoveredTypes.contains(tl.name)) F.unit
        else {
          S.set(
            RenderState(
              state.discoveredTypes + tl.name,
              tl :: state.toRender
            )
          )
        }

      fa.as(tl.name)
    }

  def renderField[F[_], G[_]](o: Output[G, Any])(implicit
      F: Monad[F],
      D: Defer[F],
      S: Stateful[F, RenderState[G]]
  ): F[String] =
    o match {
      case Opt(of) => renderField[F, G](of)
      case Arr(of) =>
        renderField[F, G](of)
          .map(x => s"[$x]")
          .map { x =>
            of match {
              case Opt(_) => x
              case _      => s"$x!"
            }
          }
      case tl: ToplevelOutput[G, Any] =>
        renderToplevelTypeInline[F, G](tl).map(x => s"$x!")
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

  def renderToplevelType[F[_], G[_]](tpe: ToplevelOutput[G, Any])(implicit
      F: Monad[F],
      D: Defer[F],
      S: Stateful[F, RenderState[G]]
  ) = tpe match {
    case Union(name, types) =>
      F.pure(s"union $name = ${types.keys.mkString_(" | ")}")
    case Scalar(name, encoder) =>
      F.pure(s"scalar $name")
    case Enum(name, encoder) =>
      F.pure(s"""
        |enum $name {
        ${encoder.toList.map(x => s"|  $x").mkString("\n")}
        |}
        |""".stripMargin)
    case Interface(name, instances, fields) =>
      renderFields[F, G](fields).map { fields =>
        s"""
            |interface $name {
            ${fields.map(x => s"|  $x").mkString_(",\n")}
            |}
            |""".stripMargin
      }
    case Obj(name, fields) =>
      renderFields[F, G](fields).map { fields =>
        s"""
            |type $name {
            ${fields.map(x => s"|  $x").mkString_(",\n")}
            |}
            |""".stripMargin
      }
  }

  def renderAll[F[_], G[_]](implicit
      F: Monad[F],
      D: Defer[F],
      S: Stateful[F, RenderState[G]]
  ): F[String] =
    D.defer {
      S.inspect(_.toRender).flatMap {
        case x :: xs =>
          S.modify(_.copy(toRender = xs)) >> renderToplevelType[F, G](x).flatMap { prefix =>
            renderAll[F, G].map(suffix => s"$prefix\nsuffix")
          }
        case Nil => F.pure("")
      }
    }

  def renderSchema[F[_]](schema: Schema[F, _]): String = {
    type G[A] = StateT[Eval, RenderState[F], A]

    renderAll[G, F].runA(RenderState(Set.empty, List(schema.query))).value
  }
}
