package clara.analyzer.impl

import clara.asg.Types
import clara.ast.{SourceMessage, Pos}

import ai.x.safe._

// TODO better name
object Checks {
  def expectAssignable(t1: Types.Typ, t2: Types.Typ, pos: Pos): An[Unit] = Types.isAssignable(t1, t2) match {
    case true =>  An.result(())
    case false => An.error(SourceMessage(pos, safe"Type `${Types.toSource(t1)}` is not assignable to type `${Types.toSource(t2)}`"))
  }
}
