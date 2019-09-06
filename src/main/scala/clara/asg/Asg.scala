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

  case class DummyTypeCon(uniq: Uniq) extends TypeCon {
    def inst() = DummyTypeInst(this)
  }
  object DummyTypeCon {
    def apply(): DummyTypeCon = DummyTypeCon(new Uniq())
  }

  case class DummyTypeInst(con: DummyTypeCon) extends TypeInst

  sealed trait BlockContent
  sealed trait ValueExpr extends BlockContent {
    def typeInst: TypeInst
  }
  sealed trait InBlockDef extends BlockContent

  case class Block(bcs: Seq[BlockContent], typeInst: TypeInst, pos: Pos = NoPos) extends ValueExpr
  case class TypeDef(name: String, pos: Pos = NoPos) extends InBlockDef

}
