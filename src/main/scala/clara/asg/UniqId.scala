package clara.asg

sealed abstract class UniqId {
  override def toString: String = this.getClass.getSimpleName() + this.hashCode()
}

// identity of a type constructor
final class TypeConId extends UniqId
object TypeConId {
  def apply() = new TypeConId()
}

// one per declaration, shared by the type and value entries it creates (e.g. a singleton), for usage tracking
final class DefId extends UniqId
object DefId {
  def apply() = new DefId()
}
