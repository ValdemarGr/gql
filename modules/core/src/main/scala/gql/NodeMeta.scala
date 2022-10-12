package gql

import org.tpolecat.sourcepos.SourcePos

final case class NodeMeta(
    description: Option[String],
    sourcePos: SourcePos
)

object NodeMeta {
  implicit def summonNodeMeta(implicit sp: SourcePos): NodeMeta = NodeMeta(None, sp)
}
