package clara.analyzer

import clara.util.Message

case class An[+A](w: An.Impl[A]) {
  def flatMap[B](f: A => An[B]): An[B] = w.value match {
    case Left(_) => this.asInstanceOf[An[B]]
    case Right(result) => An(Writer(result, w.log).flatMap(a => f(a).w))
  }

  def map[B](f: A => B): An[B] = w.value match {
    case Left(_) => this.asInstanceOf[An[B]]
    case Right(result) => An(Writer(result, w.log).map(a => Right(f(a))))
  }

  def tell(ms: Seq[Message]) = An(w.tell(ms))
  def tell(m: Message): An[A] = tell(Vector(m))

  def zip[B](b: An[B]): An[(A, B)] = {
    val Writer(value, log) = w.zip(b.w)
    val zippedValue = value match {
      case (Left(aErrors), Left(bErrors)) => Left(aErrors ++ bErrors)
      case (Left(aErrors), Right(_)) => Left(aErrors)
      case (Right(_), Left(bErrors)) => Left(bErrors)
      case (Right(aResult), Right(bResult)) => Right((aResult, bResult))
    }

    An(Writer(zippedValue, log))
  }
}

object An {
  type Errors = Vector[Message]
  type Impl[+A] = Writer[Either[Errors, A], Message]

  def result[A](a: A): An[A] = An(Writer(Right(a), Vector()))

  def error(e: Message): An[Nothing] = An(Writer(Left(Vector(e)), Vector()))

  def seq[A](ans: Seq[An[A]]) = {
    val Writer(values, log) = Writer.seq(ans.map(_.w))
    val (allErrors, allResults) = values.foldLeft((Vector.empty: Errors, Vector.empty[A])) { case ((errorsAcc, resultsAcc), value) =>
      value match {
        case Right(result) => (errorsAcc, resultsAcc :+ result)
        case Left(errors) => (errorsAcc ++ errors, resultsAcc)
      }
    }

    val seqValue = if (allErrors.isEmpty) {
      Right(allResults)
    } else {
      Left(allErrors)
    }

    An(Writer(seqValue, log))
  }
}
