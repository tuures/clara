package clara.testutil

import clara.ast.NoPos
import clara.asg.{Namespace, TypeCons, Types}
import clara.analyzer.impl.Env

// TODO: see later if this will be actually shared or if it will just be used in PatternAnalyzerSpec
object AnalyzerTestPrelude {
  val intCon: TypeCons.OpaqueTypeCon = TypeCons.OpaqueTypeCon("Int", Nil, NoPos)
  val floatCon: TypeCons.OpaqueTypeCon = TypeCons.OpaqueTypeCon("Float", Nil, NoPos)
  val stringCon: TypeCons.OpaqueTypeCon = TypeCons.OpaqueTypeCon("String", Nil, NoPos)

  val typeCons: Namespace[TypeCons.TypeCon] = Namespace(
    ("Int", intCon),
    ("Float", floatCon),
    ("String", stringCon)
  )

  val env: Env = Env.empty.copy(typeCons = typeCons)

  val intType: Types.Type = Types.Opaque(intCon, Nil)
  val floatType: Types.Type = Types.Opaque(floatCon, Nil)
  val stringType: Types.Type = Types.Opaque(stringCon, Nil)
}
