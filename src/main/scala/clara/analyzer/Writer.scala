package clara.analyzer

case class Writer[+A, +M](value: A, log: Vector[M]) {
  def flatMap[B, M2 >: M](f: A => Writer[B, M2]) = {
    val b = f(value)

    b.copy(log = log ++ b.log)
  }
  def map[B](f: A => B) = Writer(f(value), log)
  def tell[M2 >: M](ms: Seq[M2]): Writer[A, M2] = Writer(value, log ++ ms)
  // def tell[M2 >: M](m: M2): Writer[A, M2] = tell(Vector(m))
  def zip[B, M2 >: M](b: Writer[B, M2]) =
    Writer((value, b.value), log ++ b.log)
}

object Writer {
  def seq[A, M](as: Seq[Writer[A, M]]): Writer[Seq[A], M] =
    as.foldLeft(Writer(Vector.empty[A], Vector.empty[M])) { case (acc, a) =>
      Writer(acc.value :+ a.value, acc.log ++ a.log)
    }
}
