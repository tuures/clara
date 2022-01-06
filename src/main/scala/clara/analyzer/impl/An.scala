package clara.analyzer.impl

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

  object Success {
    def apply[A](a: A, log: Vector[Message]): An[A] = An(Writer(Right(a), log))
    def unapply[A](an: An[A]): Option[(A, Vector[Message])] = an.w.value match {
      case Right(a) => Some((a, an.w.log))
      case Left(_) => None
    }
  }
  /** builds a Success with the result and no log */
  def result[A](a: A): An[A] = Success(a, Vector())

  object Failure {
    def apply[A](errors: Vector[Message], log: Vector[Message]): An[Nothing] = An(Writer(Left(errors), log))
    def unapply[A](an: An[A]): Option[(Vector[Message], Vector[Message])] = an.w.value match {
      case Right(_) => None
      case Left(errors) => Some((errors, an.w.log))
    }
  }
  /** builds a Failure with the error and no log */
  def error(e: Message): An[Nothing] = Failure(Vector(e), Vector())

  def seq[A](ans: Seq[An[A]]): An[Seq[A]] = {
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

  def fromSomeOrElse[A](o: Option[A], fallback: => An[A]): An[A] = o.map(An.result).getOrElse(fallback)

  def fromSomeOrError[A](o: Option[A], e: => Message): An[A] = fromSomeOrElse(o, An.error(e))

  /**
   * Starting with `initialResult` result, for each element in `as`,
   * apply `f` to the result and the element to obtain the next result.
   * If any `An` returned from `f` contains errors, return all accumulated errors,
   * otherwise return the last result.
   *
   * In other words, analyse a list of elements when the analysis of the next
   * element depends on the analysis results of the previous element.
   */
  def step[A, B](as: Seq[A])(initialResult: B)(f: (B, A) => An[B]): An[B] =
    as.foldLeft(StepState.begin(initialResult)) { case (state, a) =>
      state.step(a, f)
    }.end

  case class StepState[B](
    currentErrors: Vector[Message],
    currentLog: Vector[Message],
    currentResult: B
  ) {
    def step[A](a: A, f: (B, A) => An[B]): StepState[B] = f(currentResult, a) match {
      case An.Success(result, log) => StepState(currentErrors, currentLog ++ log, result)
      case An.Failure(errors, log) => StepState(currentErrors ++ errors, currentLog ++ log, currentResult)
    }
    def end = currentErrors.isEmpty match {
      case true => An(Writer(Right(currentResult), currentLog))
      case false => An(Writer(Left(currentErrors), currentLog))
    }
  }

  object StepState {
    def begin[B](currentResult: B) = StepState(Vector[Message](), Vector[Message](), currentResult)
  }

}




