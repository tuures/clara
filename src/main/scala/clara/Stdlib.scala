package clara

object Stdlib {
  val baseEnv = {
    import collection.immutable.HashMap
    import Analyzer._

    val intType = new TypeCon(TypeDesc(parent=None))
    Env.empty.copy(types = HashMap(
      "()" -> new TypeCon(TypeDesc(parent=None)),
      "Int" -> intType,
      "String" -> new TypeCon(TypeDesc(parent=None).
        addMember("length", PlainValueMember(intType.inst().getOrElse(???))).right.get
      ),
      // "Tuple" -> new TypeCon(TypeDesc().
      //   addMember
      // ),
      "Function" -> new TypeCon(TypeDesc(parent=None).
        addTypeParam("P", TypeParam(Contravariant)).right.get.
        addTypeParam("R", TypeParam(Covariant)).right.get.
        addMember("apply", Method(TypeParamRef("P"), TypeParamRef("R"))).right.get
      )
    ))
  }
}
