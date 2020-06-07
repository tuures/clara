package clara.analyzer.impl

import clara.asg.Types.Type

case class TypeInfo[I](m: Map[Type, I]) {
  def addOrModify(binding: (Type, I)): TypeInfo[I] = this.copy(m = m + binding)
  def add(binding: (Type, I)): Option[TypeInfo[I]] = m.get(binding._1) match {
    case Some(_) => None
    case None => Some(this.addOrModify(binding))
  }
  def get(typ: Type): Option[I] = m.get(typ)
}

object TypeInfo {
  def empty[I] = TypeInfo(Map.empty[Type, I])
}
