package clara.analyzer.impl

import clara.asg.Uniq

case class UniqInfo[I](m: Map[Uniq, I]) {
  def addOrModify(binding: (Uniq, I)): UniqInfo[I] = this.copy(m = m + binding)
  def add(binding: (Uniq, I)): Option[UniqInfo[I]] = m.get(binding._1) match {
    case Some(_) => None
    case None => Some(this.addOrModify(binding))
  }
  def get(typ: Uniq): Option[I] = m.get(typ)
}

object UniqInfo {
  def empty[I] = UniqInfo(Map.empty[Uniq, I])
}
