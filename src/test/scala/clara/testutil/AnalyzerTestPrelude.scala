package clara.testutil

import clara.ast.NoPos
import clara.asg.{DefId, Namespace, TypeCons, Types}
import clara.analyzer.impl.{Env, BlockScope, EnvTypeEntry, EnvTypeCon}

// TODO: see later if this will be actually shared or if it will just be used in PatternAnalyzerSpec
object AnalyzerTestPrelude {
  val intCon: TypeCons.OpaqueTypeCon = TypeCons.OpaqueTypeCon("Int", Nil, Nil, NoPos)
  val floatCon: TypeCons.OpaqueTypeCon = TypeCons.OpaqueTypeCon("Float", Nil, Nil, NoPos)
  val stringCon: TypeCons.OpaqueTypeCon = TypeCons.OpaqueTypeCon("String", Nil, Nil, NoPos)

  val intDefId: DefId = DefId()

  val types: Namespace[EnvTypeEntry] = Namespace(
    ("Int", EnvTypeCon(intCon, intDefId)),
    ("Float", EnvTypeCon(floatCon, DefId())),
    ("String", EnvTypeCon(stringCon, DefId()))
  )

  val env: Env = Env.empty.copy(local = BlockScope(Namespace.empty, types))

  val intType: Types.Type = Types.Opaque(intCon, Nil)
  val floatType: Types.Type = Types.Opaque(floatCon, Nil)
  val stringType: Types.Type = Types.Opaque(stringCon, Nil)

  val aParamCon: TypeCons.ParamCon = TypeCons.ParamCon("A", NoPos)
  val aParam: Types.Param = Types.Param(aParamCon)
}
