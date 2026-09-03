package clara.asg

import scala.collection.immutable.VectorMap

case class Namespace[A](m: VectorMap[String, A]) {
  def addOrModify(binding: (String, A)): Namespace[A] =
    this.copy(m = m.removed(binding._1) + binding)
  def add(binding: (String, A)): Option[Namespace[A]] = m.get(binding._1) match {
    case Some(_) => None
    case None => Some(this.addOrModify(binding))
  }
  def get(name: String): Option[A] = m.get(name)
  /** NOTE: slow sequential search */
  // def getNames(item: I): Iterable[String] = m.filter(_._2 === item).map(_._1)
  // def size = m.size
  // def names = m.keys
  // def items = m.values
  def entries: Seq[(String, A)] = m.toSeq
  // def filter(p: (String, I) => Boolean) = Namespace(m.filter(p.tupled))
  def mapValues[B](f: A => B): Namespace[B] = Namespace(m.map { case (name, item) => (name, f(item)) })
  def mergeShadowingEverything(other: Namespace[A]) = Namespace(m ++ other.m)
}

object Namespace {
  def apply[A](entries: (String, A)*): Namespace[A] = Namespace(VectorMap(entries:_*))
  // def unapplySeq[A](ns: Namespace[A]): Option[Seq[(String, A)]] = Some(ns.entries)
  def empty[A]: Namespace[A] = Namespace(VectorMap.empty[String, A])
}
