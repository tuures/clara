package clara.testutil

import clara.ast.Ast

object AstTestHelpers {
  import Ast._

    def NamedType(name: String) = new NamedType(NameWithPos(name), Nil)
    def NamedType(name: String, typeArgs: Seq[TypeExpr]) = new NamedType(NameWithPos(name), typeArgs)

    def DeclTargetType(name: String) = new DeclTargetType(NameWithPos(name), Nil)
    def DeclTargetType(name: String, typeParams: Seq[TypeParam]) = new DeclTargetType(NameWithPos(name), typeParams)

    def TypeDef(typeDefKind: TypeDefKind, name: String) =
      new TypeDef(typeDefKind, DeclTargetType(name, Nil), None)
    def TypeDef(typeDefKind: TypeDefKind, name: String, typeParams: Seq[TypeParam]) =
      new TypeDef(typeDefKind, DeclTargetType(name, typeParams), None)
    def TypeDef(typeDefKind: TypeDefKind, name: String, t: TypeExpr) =
      new TypeDef(typeDefKind, DeclTargetType(name, Nil), Some(t))
    def TypeDef(typeDefKind: TypeDefKind, name: String, typeParams: Seq[TypeParam], t: TypeExpr) =
      new TypeDef(typeDefKind, DeclTargetType(name, typeParams), Some(t))

    def MethodDecl(name: String, t: NamedType) = new MethodDecl(Nil, NameWithPos(name), t)
    def MethodDecl(attributes: Seq[Attribute], name: String, t: NamedType) =
      new MethodDecl(attributes, NameWithPos(name), t)

    def MethodDef(name: String, t: Option[NamedType], e: ValueExpr) = new MethodDef(Nil, NameWithPos(name), t, e)
    def MethodDef(attributes: Seq[Attribute], name: String, t: Option[NamedType], e: ValueExpr) =
      new MethodDef(attributes, NameWithPos(name), t, e)

    def Lambda(parameter: Pattern, body: ValueExpr) =
      new Lambda(Nil, parameter, body)
    def Lambda(typeParams: Seq[TypeParam], parameter: Pattern, body: ValueExpr) =
      new Lambda(typeParams, parameter, body)

    def FuncType(parameter: TypeExpr, result: TypeExpr) =
      new FuncType(Nil, parameter, result)
    def FuncType(typeParams: Seq[TypeParam], parameter: TypeExpr, result: TypeExpr) =
      new FuncType(typeParams, parameter, result)
}
