package clara.analyzer.impl

import clara.util.Message
import clara.asg.{DefId, TypeConId, Types}
import clara.ast.Pos


case class AnResult[+A](value: Either[An.Errors, A], log: AnLog)

case class An[+A](runFrom: AnLog => (Option[A], AnLog)) {
  def run: AnResult[A] = runFrom(AnLog()) match {
    case (_, log) if log.errors.nonEmpty => AnResult(Left(log.errors), log.copy(errors = Vector.empty))
    case (Some(result), log) => AnResult(Right(result), log)
    case (None, _) => throw new java.lang.AssertionError("an analysis without a result hasn't logged its errors")
  }

  def flatMap[B](f: A => An[B]): An[B] = An { log =>
    runFrom(log) match {
      case (Some(result), nextLog) => f(result).runFrom(nextLog)
      case (None, nextLog) => (None, nextLog)
    }
  }

  def map[B](f: A => B): An[B] = An { log =>
    runFrom(log) match { case (result, nextLog) => (result.map(f), nextLog) }
  }

  def tellWarnings(ms: Seq[Message]): An[A] = tell(AnLog(warnings = ms.toVector))
  def tellWarning(m: Message): An[A] = tellWarnings(Vector(m))
  def tellUsages(uses: Seq[(DefId, Pos)]): An[A] =
    tell(AnLog(usageTrace = UsageTrace(uses.groupMapReduce { case (defId, _) => defId } { case (_, pos) => Set(pos) }(_ ++ _))))
  def tellUsage(defId: DefId, pos: Pos): An[A] = tellUsages(Vector(defId -> pos))
  def tellStructure(con: TypeConId, structure: Types.Type): An[A] = tell(AnLog(typeStructures = TypeStructures(Map(con -> structure))))

  private def tell(entry: AnLog): An[A] = An { log =>
    runFrom(log) match { case (result, nextLog) => (result, nextLog.append(entry)) }
  }

  /**
   * Combines two analyses together, b runs after this one also when this one has no result (so that its errors are
   * logged too). Has the results tupled if both have a result.
   */
  def zip[B](b: An[B]): An[(A, B)] = An { log =>
    val (aResult, aLog) = runFrom(log)
    val (bResult, bLog) = b.runFrom(aLog)

    (aResult.zip(bResult), bLog)
  }
}

object An {
  type Errors = Vector[Message]

  /** Successful analysis which produced a valid result, adding `log` to the log */
  object Valid {
    def apply[A](a: A, log: AnLog): An[A] = An(previous => (Some(a), previous.append(log)))
  }
  /** Builds a Valid with the result and no log. */
  def valid[A](a: A): An[A] = Valid(a, AnLog())

  /** Analysis which could not be completed due to errors, adding `log` and the errors to the log */
  object Invalid {
    def apply[A](errors: Vector[Message], log: AnLog): An[Nothing] =
      An(previous => (None, previous.append(log).append(AnLog(errors = errors))))
  }
  /** Builds an Invalid with the error and no log. */
  def error(e: Message): An[Nothing] = Invalid(Vector(e), AnLog())

  /** The log of everything analyzed before, as the result */
  def currentLog: An[AnLog] = An(log => (Some(log), log))

  def errorIf(pred: Boolean)(e: Message): An[Unit] = if (pred) error(e) else valid(())

  def errorFromSome[A](o: Option[A])(f: A => Message): An[Unit] = o.map(a => error(f(a))).getOrElse(valid(()))

  /**
   * Combines sequence of independent analyses together, each runs after the previous one (also when one has no
   * result, so that the errors of all are logged). Has the results if all have a result.
   */
  def seq[A](ans: Seq[An[A]]): An[Seq[A]] = An { log =>
    // one fold inside a single An (not nested zips), so a long sequence doesn't grow the stack
    ans.foldLeft((Option(Vector.empty[A]), log)) { case ((results, currentLog), an) =>
      val (result, nextLog) = an.runFrom(currentLog)
      (results.zip(result).map { case (results, result) => results :+ result }, nextLog)
    }
  }

  def fromSomeOrElse[A](o: Option[A], fallback: => An[A]): An[A] = o.map(An.valid).getOrElse(fallback)

  def fromSomeOrError[A](o: Option[A], e: => Message): An[A] = fromSomeOrElse(o, An.error(e))

  /**
   * Starting with `initialResult` result, for each element in `as`,
   * apply `f` to the result and the element to obtain the next result.
   * If any `An` returned from `f` has no result, there's no result in the end, otherwise it's the last result.
   *
   * In other words, analyse a list of elements when the analysis of the next
   * element may depend on the state of the analysis after the previous elements.
   *
   * If analysis of one of the elements has errors, the analysis still continues
   * to the next element providing the last successful result. The combined
   * analysis in the end will be Invalid, but the benefit of this behaviour is that
   * all possible further errors and logs from the later elements can be captured.
   */
  def step[A, B](as: Seq[A])(initialResult: B)(f: (B, A) => An[B]): An[B] =
    escalateErrors(stepDeferring(as)(initialResult)(f))

  /**
   * Like step, but its result is kept also when an element had errors: the result is the last successful one. For
   * stepping through a sequence nested in an element of another step (e.g. a namespace's body), so that an error
   * inside it doesn't lose what the rest of it did.
   */
  def stepDeferring[A, B](as: Seq[A])(initialResult: B)(f: (B, A) => An[B]): An[B] = An { log =>
    val (lastResult, nextLog) = as.foldLeft((initialResult, log)) { case ((currentResult, currentLog), a) =>
      val (result, nextLog) = f(currentResult, a).runFrom(currentLog)
      (result.getOrElse(currentResult), nextLog)
    }

    (Some(lastResult), nextLog)
  }

  /**
   * The analysis goes on from `fallback` if `an` has no result, its errors are logged so the analysis is still invalid
   * in the end and has no result at the next boundary (escalateErrors, step).
   */
  def deferErrors[A](an: An[A])(fallback: => A): An[A] = An { log =>
    val (result, nextLog) = an.runFrom(log)
    (result.orElse(Some(fallback)), nextLog)
  }

  /**
   * Error boundary: `an` has no result if it logged errors, anywhere inside it (nested analyses included). Errors
   * logged before it don't count, and what's chained after it doesn't run when the result is dropped.
   */
  def escalateErrors[A](an: An[A]): An[A] = An { log =>
    val (result, nextLog) = an.runFrom(log)
    // errors are only ever appended, so the ones `an` logged are beyond the ones logged before it
    val loggedErrors = nextLog.errors.length > log.errors.length

    (result.filterNot(_ => loggedErrors), nextLog)
  }

}
