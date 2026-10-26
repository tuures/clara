package clara.analyzer.impl

import clara.util.Message
import clara.util.Safe._
import clara.asg.{DefId, TypeConId, TypeCons, Types}
import clara.ast.Pos


// TODO: tracking usage of: 1) record fields (Uniq, String), 2) methods (Uniq, String)
// TODO: use UniqInfo
// uses of declarations, a use of either side of a declaration with both a type and a value (e.g. a singleton) counts
case class UsageTrace(defs: Map[DefId, Set[Pos]] = Map.empty) {
  def append(other: UsageTrace): UsageTrace = UsageTrace(
    other.defs.foldLeft(defs) { case (acc, (defId, positions)) =>
      acc.updated(defId, acc.getOrElse(defId, Set.empty) ++ positions)
    }
  )
}

case class TypeStructures(structures: Map[TypeConId, Types.Type]) {
  def structure(con: TypeCons.WrapperTypeCon): Types.Type = structures.getOrElse(con.uniq,
    throw new java.lang.AssertionError(safe"the structure of type `${con.name}` hasn't been registered")
  )

  // a con's structure is registered once, in the types pass of its scope
  def append(other: TypeStructures): TypeStructures = {
    other.structures.keys.find(structures.contains).foreach { con =>
      throw new java.lang.AssertionError(safe"the structure of type con ${con.toString} has been registered twice")
    }

    TypeStructures(structures ++ other.structures)
  }
}

object TypeStructures {
  val empty: TypeStructures = TypeStructures(Map.empty)
}

case class AnLog(
  errors: Vector[Message] = Vector.empty,
  warnings: Vector[Message] = Vector.empty,
  usageTrace: UsageTrace = UsageTrace(),
  typeStructures: TypeStructures = TypeStructures.empty,
) {
  def append(l: AnLog): AnLog = AnLog(
    errors ++ l.errors,
    warnings ++ l.warnings,
    usageTrace.append(l.usageTrace),
    typeStructures.append(l.typeStructures),
  )
}
