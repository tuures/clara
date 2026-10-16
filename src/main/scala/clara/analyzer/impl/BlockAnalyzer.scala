package clara.analyzer.impl

import clara.asg.{Terms, Types}
import clara.ast.{Ast, Pos, SourceMessage}

import clara.util.Safe._

object BlockAnalyzerImpl {

  case class BlockState(
    currentEnv: Env,
    currentContents: Vector[Terms.BlockContent],
    currentReturnType: Option[Types.Type],
  ) {
    def unusedDefinitionWarnings(usageTrace: UsageTrace): Seq[SourceMessage] = {
      // TODO: Ignore exported definitions even when unused.
      val unusedTypes = currentEnv.local.typeCons.entries.collect {
        case (name, EnvTypeCon(con, defId)) if !usageTrace.defs.contains(defId) =>
          defId -> SourceMessage(con.definedAt, safe"Unused type `$name`")
      }
      val unusedValues = currentEnv.local.values.entries.collect {
        case (name, EnvValue(_, definedAt, defId)) if !usageTrace.defs.contains(defId) =>
          defId -> SourceMessage(definedAt, safe"Unused value `$name`")
      }

      // one warning per declaration, a declaration with both a type and a value (a singleton) is reported as a type
      (unusedTypes ++ unusedValues).
        distinctBy { case (defId, _) => defId }.
        map { case (_, message) => message }.
        sortBy(_.pos)
    }

    def buildTerm(blockPos: Pos, isProgramBlock: Boolean): An[Terms.Block] = currentReturnType match {
      case Some(typ) if isProgramBlock && (typ !== Types.Uni) => An.result(Terms.Block(currentContents, typ)).
        tellWarning(SourceMessage(blockPos, "Non-unit value discarded in program"))
      case Some(typ) => An.result(Terms.Block(currentContents, typ))
      case None if currentContents.isEmpty => An.result(Terms.Block(currentContents, Types.Uni))
      case None => An.result(Terms.Block(currentContents, Types.Uni)).
        tellWarning(SourceMessage(blockPos, "Block should end with an expression. Implicitly returning unit."))
    }
  }

  case class BlockContentStep(nextEnv: Env, contentTerm: Option[Terms.BlockContent], nextReturnType: Option[Types.Type])

  def walkBlockContent(currentEnv: Env, bc: Ast.BlockContent, isLast: Boolean): An[BlockContentStep] = bc match {
    case valueExprAst: Ast.ValueExpr =>
      ValueExprAnalyzer.valueExprTerm(currentEnv, valueExprAst).flatMap { valueExprTerm =>
        val isUnit = valueExprTerm.typ === Types.Uni

        lazy val discardWarning = SourceMessage(bc.pos, "Non-unit value discarded in block")
        val maybeDiscardWarning = if (!isLast && !isUnit) Seq(discardWarning) else Nil

        An.result(BlockContentStep(currentEnv, Some(valueExprTerm), Some(valueExprTerm.typ))).tellWarnings(maybeDiscardWarning)
      }
    case Ast.ValueDecl(name, t, pos) =>
      TypeExprAnalyzer.typeExprType(currentEnv, t).flatMap { typ =>
        // FIXME use namePos to be consistent with valueDef and typeDef
        currentEnv.addOrShadowValue(name, typ, pos)
      }.map { nextEnv =>
        BlockContentStep(nextEnv, Some(Terms.ValueDecl(name)), None)
      }
    case Ast.ValueDef(target, e, _) =>
      ValueExprAnalyzer.valueExprTerm(currentEnv, e).flatMap { valueExprTerm =>
        PatternAnalyzer.walkAssignment(currentEnv, target, Some(valueExprTerm.typ)).
          map { case (nextEnv, targetTerm) =>
            BlockContentStep(nextEnv, Some(Terms.ValueDef(targetTerm, valueExprTerm)), None)
          }
      }
    // type defs have already been analyzed/hoisted by walkTypeDefs, but here affects the block's return value
    // (block should end with an expression)
    case _: Ast.TypeDef => An.result(BlockContentStep(currentEnv, None, None))
    case methodSection: Ast.MethodSection =>
      MethodSectionAnalyzer.methodSectionTerm(currentEnv, methodSection).
        map { case (nextEnv, term) => BlockContentStep(nextEnv, Some(term), None) }
  }

  // type names a type expression refers to, without the type params it introduces itself. It must not miss any:
  // a missed dependency could get analyzed later, so the name would resolve to a parent scope's type of the same name
  // (e.g. type param bounds need to be added here when they come)
  def referencedTypeNames(typeExpr: Ast.TypeExpr): Seq[String] = typeExpr match {
    case _: Ast.TopType | _: Ast.BottomType | _: Ast.UnitType => Nil
    case Ast.TupleType(ts, _) => ts.flatMap(referencedTypeNames)
    case Ast.NamedType(name, typeArgs, _) => name.name +: typeArgs.flatMap(referencedTypeNames)
    case Ast.RecordType(fields, _) => fields.flatMap(field => referencedTypeNames(field.t))
    case Ast.UnionType(ts, _) => ts.flatMap(referencedTypeNames)
    case Ast.IntersectionType(ts, _) => ts.flatMap(referencedTypeNames)
    case Ast.FuncType(typeParams, parameter, result, _) =>
      val introduced = typeParams.map(_.name).toSet
      (referencedTypeNames(parameter) ++ referencedTypeNames(result)).filterNot(introduced)
  }

  // indices of the type defs in an order where each comes after the type defs of the block it refers to
  def typeDefsInDependencyOrder(typeDefs: Vector[Ast.TypeDef]): An[Seq[Int]] = {
    def name(index: Int) = typeDefs(index).target.name.name

    // a later type def with the same name is reported when it's added to the env, only the first one is referred to
    val indexByName = typeDefs.indices.foldLeft(Map.empty[String, Int]) { (acc, index) =>
      if (acc.contains(name(index))) acc else acc + (name(index) -> index)
    }
    val (firstIndices, duplicateIndices) = typeDefs.indices.partition(index => indexByName(name(index)) === index)

    def dependencies(index: Int): Seq[Int] = {
      val Ast.TypeDef(_, Ast.DeclTargetType(_, typeParams, _), maybeTypeExpr, _) = typeDefs(index)
      val introduced = typeParams.map(_.name).toSet

      maybeTypeExpr.toSeq.flatMap(referencedTypeNames).filterNot(introduced).distinct.flatMap(indexByName.get)
    }

    // post-order traversal, `path` holds the type defs being visited for reporting a cycle
    def visit(ordered: Vector[Int], path: Vector[Int], index: Int): Either[SourceMessage, Vector[Int]] =
      if (ordered.contains(index)) Right(ordered)
      else if (path.contains(index)) {
        val cycle = (path.dropWhile(_ !== index) :+ index).map(i => safe"`${name(i)}`").safeString(" -> ")
        Left(SourceMessage(typeDefs(index).target.name.pos, safe"Recursive types aren't supported yet: $cycle"))
      } else {
        dependencies(index).foldLeft[Either[SourceMessage, Vector[Int]]](Right(ordered)) { (either, dependency) =>
          either.flatMap(visit(_, path :+ index, dependency))
        }.map(_ :+ index)
      }

    firstIndices.foldLeft[Either[SourceMessage, Vector[Int]]](Right(Vector.empty)) { (either, index) =>
      either.flatMap(visit(_, Vector.empty, index))
    }.fold(An.error, ordered => An.result(ordered ++ duplicateIndices))
  }

  def walkTypeDefs(parentEnv: Env, typeDefs: Vector[Ast.TypeDef]): An[BlockState] =
    typeDefsInDependencyOrder(typeDefs).flatMap { order =>
      // the terms stay in dependency order, before any value (the emitter defines singletons as consts)
      An.step(order)(BlockState(parentEnv.startNestedScope, Vector.empty, None)) { case (state, index) =>
        TypeDefAnalyzer.typeDefTerm(state.currentEnv, typeDefs(index)).map { case (nextEnv, term) =>
          BlockState(nextEnv, state.currentContents :+ term, None)
        }
      }
    }

  def walkBlockContents(parentEnv: Env, bcs: Seq[Ast.BlockContent]): An[BlockState] = {
    val typeDefs = bcs.collect { case typeDef: Ast.TypeDef => typeDef }.toVector

    // analyze all type defs in dependency order before analyzing the rest of the block contents
    walkTypeDefs(parentEnv, typeDefs).flatMap { stateWithTypeDefs =>
      An.step(bcs.zipWithIndex)(stateWithTypeDefs) { case (currentState, (bc, index)) =>
        val BlockState(currentEnv, currentContents, _) = currentState

        walkBlockContent(currentEnv, bc, index === bcs.length - 1).
          map { case BlockContentStep(nextEnv, contentTerm, nextReturnType) =>
            BlockState(nextEnv, currentContents ++ contentTerm, nextReturnType)
          }
      }
    }
  }

  def finishBlock(blockState: An[BlockState], blockPos: Pos, isProgramBlock: Boolean): An[Terms.Block] =
    blockState.zipLog.flatMap { case (blockState, log) =>
      An.result(blockState).tellWarnings(blockState.unusedDefinitionWarnings(log.usageTrace))
    }.flatMap(_.buildTerm(blockPos, isProgramBlock))

  def blockTerm(parentEnv: Env, block: Ast.Block, isProgramBlock: Boolean): An[Terms.Block] =
    finishBlock(walkBlockContents(parentEnv, block.bcs), block.pos, isProgramBlock)

}

object BlockAnalyzer {
  def regularBlockTerm(parentEnv: Env, block: Ast.Block): An[Terms.Block] =
    BlockAnalyzerImpl.blockTerm(parentEnv, block, isProgramBlock = false)

  def programBlockTerm(parentEnv: Env, block: Ast.Block): An[Terms.Block] =
    BlockAnalyzerImpl.blockTerm(parentEnv, block, isProgramBlock = true)
}
