package clara.analyzer.impl

import clara.asg.UniqId

case class UniqInfo[K <: UniqId, I](m: Map[K, I]) {
  def addOrModify(binding: (K, I)): UniqInfo[K, I] = this.copy(m = m + binding)
  def add(binding: (K, I)): Option[UniqInfo[K, I]] = m.get(binding._1) match {
    case Some(_) => None
    case None => Some(this.addOrModify(binding))
  }
  def get(id: K): Option[I] = m.get(id)
}

object UniqInfo {
  def empty[K <: UniqId, I] = UniqInfo(Map.empty[K, I])
}
