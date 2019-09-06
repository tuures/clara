package clara.analyzer.impl

import scala.collection.immutable.ListMap

case class Namespace[I](m: ListMap[String, I]) {
  def addOrUpdate(binding: (String, I)): Namespace[I] = this.copy(m = m + binding)
  def add(binding: (String, I)): Option[Namespace[I]] = m.get(binding._1) match {
    case Some(_) => None
    case None => Some(this.addOrUpdate(binding))
  }
  def addOrShadow(binding: (String, I), allowShadow: Namespace[I]): Option[Namespace[I]] =
    if (allowShadow.get(binding._1).isDefined) {
      Some(addOrUpdate(binding))
    } else {
      add(binding)
    }
  def get(name: String): Option[I] = m.get(name)
  def getName(item: I): Option[String] = m.find { case (name, i) => i == item } map(_._1)
  def length = m.size
  def names = m.keys
  def items = m.values
  def entries = m.toList
  def filter(p: (String, I) => Boolean) = Namespace.Impl.fromEntriesUnsafe(entries.filter(p.tupled))
  def mergeShadowingEverything(other: Namespace[I]) = Namespace(m ++ other.m)
}

object Namespace {
  def empty[I] = Namespace(ListMap.empty[String, I])

  object Impl {
    // unsafe because duplicate keys are silently ignored
    def fromEntriesUnsafe[I](es: Seq[(String, I)]) = Namespace(ListMap(es:_*))
  }
}
