package clara.analyzer.impl

import clara.util.Message
import clara.asg.Uniq
import clara.ast.Pos


// TODO: tracking usage of: 1) record fields (Uniq, String), 2) methods (Uniq, String)
// TODO: use UniqInfo
case class UsageTrace(valueDefs: Map[Uniq, Set[Pos]] = Map.empty, typeCons: Map[Uniq, Set[Pos]] = Map.empty) {
  def append(other: UsageTrace): UsageTrace = UsageTrace(
    mergePositions(valueDefs, other.valueDefs),
    mergePositions(typeCons, other.typeCons)
  )

  private def mergePositions(
    left: Map[Uniq, Set[Pos]],
    right: Map[Uniq, Set[Pos]]
  ): Map[Uniq, Set[Pos]] =
    right.foldLeft(left) { case (acc, (uniq, positions)) =>
      acc.updated(uniq, acc.getOrElse(uniq, Set.empty) ++ positions)
    }
}

case class AnLog(warnings: Vector[Message] = Vector.empty, usageTrace: UsageTrace = UsageTrace()) {
  def append(other: AnLog): AnLog =
    AnLog(warnings ++ other.warnings, usageTrace.append(other.usageTrace))
}

case class An[+A](v: Either[An.Errors, A], l: AnLog) {
  def value: Either[An.Errors, A] = v
  def log: AnLog = l

  def zipLog: An[(A, AnLog)] = map((_, l))

  def flatMap[B](f: A => An[B]): An[B] = v match {
    case Left(_) => this.asInstanceOf[An[B]]
    case Right(result) =>
      val next = f(result)
      An(next.v, l.append(next.l))
  }

  def map[B](f: A => B): An[B] = v match {
    case Left(_) => this.asInstanceOf[An[B]]
    case Right(result) => An(Right(f(result)), l)
  }

  def tellWarnings(ms: Seq[Message]): An[A] = An(v, l.append(AnLog(warnings = ms.toVector)))
  def tellWarning(m: Message): An[A] = tellWarnings(Vector(m))
  def tellUsage(usageTrace: UsageTrace): An[A] = An(v, l.append(AnLog(usageTrace = usageTrace)))
  def tellValueDefUsage(uniq: Uniq, pos: Pos): An[A] = tellUsage(UsageTrace(valueDefs = Map(uniq -> Set(pos))))
  def tellTypeConUsage(uniq: Uniq, pos: Pos): An[A] = tellUsage(UsageTrace(typeCons = Map(uniq -> Set(pos))))

  /**
   * Combines two analyses together.
   * Returns a combined Failure if either one is a Failure.
   * Returns a Success with the results tupled if both are Successes.
   */
  def zip[B](b: An[B]): An[(A, B)] = {
    val zippedValue = (v, b.v) match {
      case (Left(aErrors), Left(bErrors)) => Left(aErrors ++ bErrors)
      case (Left(aErrors), Right(_)) => Left(aErrors)
      case (Right(_), Left(bErrors)) => Left(bErrors)
      case (Right(aResult), Right(bResult)) => Right((aResult, bResult))
    }

    An(zippedValue, l.append(b.l))
  }
}

object An {
  type Errors = Vector[Message]

  /** Successful analysis which produced a complete result */
  object Success {
    def apply[A](a: A, log: AnLog): An[A] = An(Right(a), log)
    def unapply[A](an: An[A]): Option[(A, AnLog)] = an.v match {
      case Right(a) => Some((a, an.l))
      case Left(_) => None
    }
  }
  /** Builds a Success with the result and no log. */
  def result[A](a: A): An[A] = Success(a, AnLog())

  /** Failed analysis which could not be completed due to errors */
  object Failure {
    def apply[A](errors: Vector[Message], log: AnLog): An[Nothing] = An(Left(errors), log)
    def unapply[A](an: An[A]): Option[(Vector[Message], AnLog)] = an.v match {
      case Right(_) => None
      case Left(errors) => Some((errors, an.l))
    }
  }
  /** Builds a Failure with the error and no log. */
  def error(e: Message): An[Nothing] = Failure(Vector(e), AnLog())

  def errorIf(pred: Boolean)(e: Message): An[Unit] = if (pred) error(e) else result(())

  def errorFromSome[A](o: Option[A])(f: A => Message): An[Unit] = o.map(a => error(f(a))).getOrElse(result(()))

  /**
   * Combines sequence of analyses together. Returns a combined Failure if any of the analyses had failed.
   * All logs are always combined.
   */
  def seq[A](ans: Seq[An[A]]): An[Seq[A]] = {
    val (values, log) = ans.foldLeft((Vector.empty[Either[Errors, A]], AnLog())) { case ((vs, l), an) =>
      (vs :+ an.v, l.append(an.l))
    }
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

    An(seqValue, log)
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
   * element may depend on the state of the analysis after the previous elements.
   *
   * If analysis of one of the elements fails, the analysis still continues
   * to the next element providing the last successful result. The combined
   * analysis in the end will be a Failure, but the benefit of this behaviour is that
   * all possible further errors and logs from the later elements can be captured.
   */
  def step[A, B](as: Seq[A])(initialResult: B)(f: (B, A) => An[B]): An[B] =
    as.foldLeft(StepState.begin(initialResult)) { case (state, a) =>
      state.step(a, f)
    }.end

  case class StepState[B](
    currentErrors: Vector[Message],
    currentResult: B,
    currentLog: AnLog,
  ) {
    def step[A](a: A, f: (B, A) => An[B]): StepState[B] = {
      val An(nextValue, nextLog) = f(currentResult, a)
      val combinedLog = currentLog.append(nextLog)
      nextValue match {
        case Right(result) => StepState(currentErrors, result, combinedLog)
        case Left(errors) => StepState(currentErrors ++ errors, currentResult, combinedLog)
      }
    }
    def end: An[B] = currentErrors.isEmpty match {
      case true => An(Right(currentResult), currentLog)
      case false => An(Left(currentErrors), currentLog)
    }
  }

  object StepState {
    def begin[B](currentResult: B): StepState[B] = StepState(Vector.empty, currentResult, AnLog())
  }

}
