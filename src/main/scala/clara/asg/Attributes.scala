package clara.asg

object Attributes {
  sealed trait EmitKind
  case object BinaryOperator extends EmitKind
  case object InstanceProperty extends EmitKind
  case class MethodAttributes(emitKind: Option[EmitKind] = None, emitName: Option[String] = None)
}

