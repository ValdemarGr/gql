package gql

import gql.ast._

sealed trait Modifier
object Modifier {
  case object List extends Modifier
  case object NonNull extends Modifier
}

sealed trait InverseModifier
object InverseModifier {
  case object List extends InverseModifier
  case object Nullable extends InverseModifier
}

final case class ModifierStack[+T](modifiers: List[Modifier], inner: T) {
  def push(m: Modifier): ModifierStack[T] =
    ModifierStack(m :: modifiers, inner)

  def show(showInner: T => String): String = modifiers match {
    case Nil                      => showInner(inner)
    case Modifier.List :: rest    => s"[${ModifierStack(rest, inner).show(showInner)}]"
    case Modifier.NonNull :: rest => s"${ModifierStack(rest, inner).show(showInner)}!"
  }
}

final case class InverseModifierStack[+T](modifiers: List[InverseModifier], inner: T) {
  def push(m: InverseModifier): InverseModifierStack[T] =
    InverseModifierStack(m :: modifiers, inner)

  def invert: ModifierStack[T] = {
    def go(ms: List[InverseModifier], inOption: Boolean): ModifierStack[T] = {
      val out = ms match {
        case Nil                              => ModifierStack(Nil, inner)
        case InverseModifier.List :: rest     => go(rest, inOption = true).push(Modifier.List)
        case InverseModifier.Nullable :: rest => go(rest, inOption = true)
      }
      if (inOption) out else out.push(Modifier.NonNull)
    }
    go(modifiers, inOption = false)
  }
}

object ModifierStack {
  def fromOut[F[_]](t: Out[F, ?]): ModifierStack[OutToplevel[F, ?]] =
    InverseModifierStack.fromOut(t).invert

  def fromIn(t: In[?]): ModifierStack[InToplevel[?]] =
    InverseModifierStack.fromIn(t).invert
}

object InverseModifierStack {
  def fromOut[F[_]](t: Out[F, ?]): InverseModifierStack[OutToplevel[F, ?]] =
    t match {
      case t: OutToplevel[F, ?] => InverseModifierStack(Nil, t)
      case OutArr(of, _, _)     => fromOut(of).push(InverseModifier.List)
      case o: OutOpt[F, ?, ?]   => fromOut(o.of).push(InverseModifier.Nullable)
    }

  def fromIn(t: In[?]): InverseModifierStack[InToplevel[?]] =
    t match {
      case t: InToplevel[?] => InverseModifierStack(Nil, t)
      case InArr(of, _)     => fromIn(of).push(InverseModifier.List)
      case o: InOpt[?]      => fromIn(o.of).push(InverseModifier.Nullable)
    }
}
