package clara.asg

import clara.ast.Ast
import clara.ast.{Pos, NoPos}

object Asg { // split: Types, Terms

  class Uniq()
  object Uniq {
    def apply() = new Uniq()
  }

  // type lattice
  sealed trait Typ
  sealed trait StructuralTyp extends Typ
  case object Top extends Typ
  case object Bottom extends Typ
  case object Uni extends StructuralTyp // Unit
  case class Func(parameter: Typ, result: Typ) extends StructuralTyp
  case class Unique(structure: StructuralTyp, uniq: Uniq = new Uniq()) extends Typ

  // program structure
  sealed trait BlockContent
  sealed trait Pattern
  sealed trait ValueExpr extends BlockContent {
    def typ: Typ
  }
  sealed trait InBlockDef extends BlockContent

  case class UnitLiteral() extends ValueExpr {
    def typ = Uni
  }
  case class Block(bcs: Seq[BlockContent], typ: Typ) extends ValueExpr

  case class NamedValue(name: String, typ: Typ) extends ValueExpr
  case class NamePattern(name: String) extends Pattern

  case class ValueNamesDef(target: Pattern, e: ValueExpr) extends InBlockDef
  case class TypeDef(name: String) extends InBlockDef

  sealed trait MethodSection extends InBlockDef
  case class MethodDeclSection(targetType: Typ, methodsDecls: Namespace[MethodDecl]) extends MethodSection
  case class MethodDecl(typ: Typ)
  case class MethodDefSection(typeName: String, targetType: Typ, methodDefs: Namespace[MethodDef]) extends MethodSection
  case class MethodDef(body: ValueExpr)

}
