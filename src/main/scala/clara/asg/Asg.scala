package clara.asg

import clara.ast.Ast
import clara.ast.{Pos, NoPos}

object Asg {

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
  sealed trait ValueExpr extends BlockContent {
    def typ: Typ
  }
  sealed trait InBlockDef extends BlockContent

  case class UnitLiteral() extends ValueExpr {
    def typ = Uni
  }
  case class Block(bcs: Seq[BlockContent], typ: Typ) extends ValueExpr

  case class NamedValue(name: String, typ: Typ) extends ValueExpr

  case class ValueNamesDef() extends InBlockDef
  case class TypeDef(name: String) extends InBlockDef
  sealed trait MethodSection extends InBlockDef
  case class MethodDefSection(typ: Typ) extends InBlockDef
  case class MethodDeclSection(typ: Typ) extends InBlockDef
  case class Method(name: String)

}
