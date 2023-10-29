package clara.testutil

import clara.ast.Ast

object AstTestHelpers {
  import Ast._

    def NamedType(name: String) = new NamedType(NameWithPos(name), Nil)
    def NamedType(name: String, typeArgs: Seq[TypeExpr]) = new NamedType(NameWithPos(name), typeArgs)

    def TypeDef(typeDefKind: TypeDefKind, name: String) =
      new TypeDef(typeDefKind, NameWithPos(name), Nil, None)
    def TypeDef(typeDefKind: TypeDefKind, name: String, typeParams: Seq[TypeParam]) =
      new TypeDef(typeDefKind, NameWithPos(name), typeParams, None)
    def TypeDef(typeDefKind: TypeDefKind, name: String, t: TypeExpr) =
      new TypeDef(typeDefKind, NameWithPos(name), Nil, Some(t))
    def TypeDef(typeDefKind: TypeDefKind, name: String, typeParams: Seq[TypeParam], t: TypeExpr) =
      new TypeDef(typeDefKind, NameWithPos(name), typeParams, Some(t))

    def Lambda(parameter: Pattern, body: ValueExpr) =
      new Lambda(Nil, parameter, body)
    def Lambda(typeParams: Seq[TypeParam], parameter: Pattern, body: ValueExpr) =
      new Lambda(typeParams, parameter, body)

    def FuncType(parameter: TypeExpr, result: TypeExpr) =
      new FuncType(Nil, parameter, result)
    def FuncType(typeParams: Seq[TypeParam], parameter: TypeExpr, result: TypeExpr) =
      new FuncType(typeParams, parameter, result)
}
