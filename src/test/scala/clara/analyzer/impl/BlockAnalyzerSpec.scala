package clara.analyzer

import clara.analyzer.impl.{BlockAnalyzer, Env}
import clara.ast.Ast
import clara.asg.{Terms, Types}
import clara.testutil.AstTestHelpers

import org.scalatest.funsuite.AnyFunSuite

class BlockAnalyzerSpec extends AnyFunSuite {
  import Ast.{TypeDef => _, NamedType => _, _}
  import AstTestHelpers._

  test("::alias Unit: ()") {
    val typeDef = TypeDef(TypeDefKind.Alias, "Unit", UnitType())
    val block = Ast.Block(Seq(typeDef))

    val an = BlockAnalyzer(Env.empty).walkBlock(block.bcs, block.pos)

    val expectedTerm = Terms.Block(Vector(Terms.TypeDef("Unit"/*FIXME add missing properties*/)), Types.Uni)
    assert(an.resultOrErrors === Right(expectedTerm))
  }

  // test("::alias Pair<A>: (A, A)") {

  //   val typeDef = TypeDef(TypeDefKind.Alias, "Pair", UnitType())
  //   val block = Ast.Block(Seq(typeDef))

  //   val an = BlockAnalyzer(Env.empty).walkBlock(block.bcs, block.pos)

  //   val expectedTerm = Terms.Block(Seq(Terms.UnitLiteral()), Types.Uni)
  //   assert(an.resultOrErrors === Right(expectedTerm))
  // }
}
