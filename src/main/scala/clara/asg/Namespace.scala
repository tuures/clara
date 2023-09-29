package clara.asg

import scala.collection.immutable.ListMap

case class Namespace[A](m: ListMap[String, A]) {
  def addOrModify(binding: (String, A)): Namespace[A] = this.copy(m = m + binding)
  def add(binding: (String, A)): Option[Namespace[A]] = m.get(binding._1) match {
    case Some(_) => None
    case None => Some(this.addOrModify(binding))
  }
  def addOrShadow(binding: (String, A), allowShadow: Namespace[A]): Option[Namespace[A]] =
    if (allowShadow.get(binding._1).isDefined) {
      Some(addOrModify(binding))
    } else {
      add(binding)
    }
  def get(name: String): Option[A] = m.get(name)
  /** NOTE: slow sequential search */
  // def getNames(item: I): Iterable[String] = m.filter(_._2 === item).map(_._1)
  // def size = m.size
  def names = m.keys
  // def items = m.values
  def entries: Seq[(String, A)] = m.toSeq
  // def filter(p: (String, I) => Boolean) = Namespace(m.filter(p.tupled))
  def mapValues[B](f: A => B): Namespace[B] = Namespace(m.map { case (name, item) => (name, f(item)) })
  // def mergeShadowingEverything(other: Namespace[I]) = Namespace(m ++ other.m)
}

object Namespace {
  def apply[A](entries: (String, A)*): Namespace[A] = Namespace(ListMap(entries:_*))
  def unapplySeq[A](ns: Namespace[A]): Option[Seq[(String, A)]] = Some(ns.entries)
  def empty[A]: Namespace[A] = Namespace()
}
