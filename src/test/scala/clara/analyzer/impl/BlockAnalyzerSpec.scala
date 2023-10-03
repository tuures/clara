package clara.analyzer

import clara.analyzer.impl.{BlockAnalyzer, Env}
import clara.ast.Ast
import clara.asg.{Terms, Types}
import clara.testutil.AstTestHelpers

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.{Inside, EitherValues}
import clara.asg.TypeCons.TypeDefCon

class BlockAnalyzerSpec extends AnyFunSuite with Inside with EitherValues {
  import Ast.{TypeDef => _, NamedType => _, _}
  import AstTestHelpers._

  test("::alias Unit: ()") {
    val typeDef = TypeDef(TypeDefKind.Alias, "Unit", UnitType())
    val block = Ast.Block(Seq(typeDef))

    val endState = BlockAnalyzer(Env.empty).walkBlockContents(block.bcs)

    val typ = endState.value.value.currentEnv.typeCons.get("Unit").get

    inside(typ) {
      case TypeDefCon(typeDefKind, name, typeParams, wrappedType, _, _) =>
        assert((typeDefKind, name, typeParams, wrappedType) === (TypeDefKind.Alias, "Unit", Nil, Types.Uni))
    }

    val term = endState.flatMap(_.finishTerm(block.pos)).value.value

    val expectedTerm = Terms.Block(Vector(Terms.TypeDef("Unit"/*FIXME add missing properties*/)), Types.Uni)
    assert(term === expectedTerm)
  }

  test("invalid ::opaque Pair<A>: (A, A)") {

    val typeDef = TypeDef(TypeDefKind.Opaque, "Pair", UnitType())
    val block = Ast.Block(Seq(typeDef))

    val an = BlockAnalyzer(Env.empty).walkBlock(block)

    assert(an.value.left.value.length === 1)
  }
}
