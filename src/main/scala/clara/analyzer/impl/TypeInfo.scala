package clara.analyzer.impl

import clara.asg.Types.Typ

case class TypeInfo[I](m: Map[Typ, I]) {
  def addOrModify(binding: (Typ, I)): TypeInfo[I] = this.copy(m = m + binding)
  def add(binding: (Typ, I)): Option[TypeInfo[I]] = m.get(binding._1) match {
    case Some(_) => None
    case None => Some(this.addOrModify(binding))
  }
  def get(typ: Typ): Option[I] = m.get(typ)
}

object TypeInfo {
  def empty[I] = TypeInfo(Map.empty[Typ, I])
}
