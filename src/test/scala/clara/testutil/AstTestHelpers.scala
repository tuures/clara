package clara.testutil

import clara.ast.Ast

object AstTestHelpers {
  import Ast._

    def NamedType(name: String) = new NamedType(NameWithPos(name), Nil)
    def NamedType(name: String, typeArgs: Seq[TypeExpr]) = new NamedType(NameWithPos(name), typeArgs)

    def TypeDef(typeDefKind: TypeDefKind, name: String, t: TypeExpr) =
      new TypeDef(typeDefKind, NameWithPos(name), Nil, t)
    def TypeDef(typeDefKind: TypeDefKind, name: String, typeParams: Seq[TypeParam], t: TypeExpr) =
      new TypeDef(typeDefKind, NameWithPos(name), typeParams, t)
}
