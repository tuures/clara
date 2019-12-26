package clara.asg

import clara.ast.{Pos, NoPos}

object Asg {

  class Uniq()
  object Uniq {
    def apply() = new Uniq()
  }

  sealed trait TypeCon {
    def inst(): TypeInst
  }
  sealed trait TypeInst

  case class UniqTypeCon(uniq: Uniq) extends TypeCon {
    def inst() = UniqTypeInst(this)
  }
  object UniqTypeCon {
    def apply(): UniqTypeCon = UniqTypeCon(new Uniq())
  }

  case class UniqTypeInst(con: UniqTypeCon) extends TypeInst

  sealed trait BlockContent
  sealed trait ValueExpr extends BlockContent {
    def typeInst: TypeInst
  }
  sealed trait InBlockDef extends BlockContent

  case class UnitLiteral(typeInst: TypeInst, pos: Pos = NoPos) extends ValueExpr

  case class Block(bcs: Seq[BlockContent], typeInst: TypeInst, pos: Pos = NoPos) extends ValueExpr
  case class TypeDef(name: String, pos: Pos = NoPos) extends InBlockDef

}
