package clara.analyzer.impl

import clara.asg.{Terms, Types}
import clara.ast.{Ast, SourceMessage}

import clara.util.Safe._

import scala.annotation.tailrec

// The third pass of a block, see BlockAnalyzer
object ValuesPass {
  case class State(
    env: Env,
    contents: Vector[Terms.BlockContent],
    returnType: Option[Types.Type],
  )

  case class Step(nextEnv: Env, contentTerms: Seq[Terms.BlockContent], nextReturnType: Option[Types.Type])

  def walk(env: Env, typeDefTerms: Vector[Terms.TypeDef], block: IndexedBlock): An[State] =
    An.step(withFunctionRuns(block.contents).zipWithIndex)(State(env, typeDefTerms, returnType = None)) {
      case (State(currentEnv, currentContents, _), ((blockContent, functionRun), index)) =>
        content(currentEnv, block, blockContent, functionRun, index === block.contents.length - 1).
          map { case Step(nextEnv, contentTerms, nextReturnType) =>
            State(nextEnv, currentContents ++ contentTerms, nextReturnType)
          }
    }

  def withFunctionRuns(contents: Vector[IndexedBlock.Content]): Vector[(IndexedBlock.Content, Seq[String])] =
    contents.zip(functionRuns(contents.map(_.ast)))

  // consecutive function defs
  // each function def in the run gets the names of the whole run
  def functionRuns(bcs: Seq[Ast.BlockContent]): Vector[Seq[String]] = {
    @tailrec
    def loop(rest: List[Ast.BlockContent], namesSoFar: Vector[Seq[String]]): Vector[Seq[String]] =
      rest.span(functionDefTarget(_).isDefined) match {
        case (Nil, Nil) => namesSoFar
        case (Nil, _ :: tail) => loop(tail, namesSoFar :+ Nil)
        case (run, afterRun) =>
          val runNames = run.flatMap(functionDefTarget).flatMap(PatternAnalyzer.boundNames).map(_.name)
          // each function def in the run gets the names of the whole run
          loop(afterRun, namesSoFar ++ run.map(_ => runNames))
      }

    loop(bcs.toList, Vector.empty)
  }

  def functionDefTarget(bc: Ast.BlockContent): Option[Ast.Pattern] = bc match {
    case Ast.ValueDef(target, e, _) => e match {
      case _: Ast.Lambda | _: Ast.Piecewise => Some(target)
      case _: Ast.UnitLiteral | _: Ast.IntegerLiteral | _: Ast.FloatLiteral | _: Ast.StringLiteral | _: Ast.Tuple |
        _: Ast.Block | _: Ast.NamedValue | _: Ast.ValueAs | _: Ast.Record | _: Ast.MemberSelection | _: Ast.Call |
        _: Ast.Pipe => None
    }
    case _: Ast.ValueExpr | _: Ast.ValueDecl | _: Ast.TypeDef | _: Ast.NamespaceDef | _: Ast.MethodSection => None
  }

  // functionRun: the names of the run of function defs the content is in (see functionRuns)
  def content(
    currentEnv: Env,
    block: IndexedBlock,
    blockContent: IndexedBlock.Content,
    functionRun: Seq[String],
    isLast: Boolean,
  ): An[Step] = blockContent match {
    // affects the block's return value (block should end with an expression)
    case _: IndexedBlock.TypeDef => An.valid(Step(currentEnv, Nil, None))
    case IndexedBlock.Namespace(_, _, body) => namespaceBody(currentEnv, body, body.block.contents)
    case IndexedBlock.Extend(namespaceDef, contents) => block.companionByName(namespaceDef.name.name) match {
      case Some(companion) => namespaceBody(currentEnv, companion, contents)
      case None => An.valid(Step(currentEnv, Nil, None))
    }
    case IndexedBlock.MethodSection(methodSection) =>
      MethodSectionAnalyzer.methodSectionTerm(currentEnv, methodSection).
        map { case (nextEnv, term) => Step(nextEnv, Seq(term), None) }
    case IndexedBlock.ValueDecl(Ast.ValueDecl(Ast.NameWithPos(name, _), _, _), _) =>
      An.valid(Step(currentEnv, Seq(Terms.ValueDecl(name)), None))
    case vd: IndexedBlock.SingleTypedValueDef => singleTypedValueDef(currentEnv, vd, functionRun)
    case vd: IndexedBlock.ValueDef => valueDef(currentEnv, vd, functionRun)
    case IndexedBlock.ValueExpr(valueExprAst) =>
      ValueExprAnalyzer.valueExprTerm(currentEnv, valueExprAst).flatMap { valueExprTerm =>
        val isUnit = valueExprTerm.typ === Types.Uni

        lazy val discardWarning = SourceMessage(valueExprAst.pos, "Non-unit value discarded in block")
        val maybeDiscardWarning = if (!isLast && !isUnit) Seq(discardWarning) else Nil

        An.valid(Step(currentEnv, Seq(valueExprTerm), Some(valueExprTerm.typ))).tellWarnings(maybeDiscardWarning)
      }
  }

  def singleTypedValueDef(currentEnv: Env, valueDef: IndexedBlock.SingleTypedValueDef, functionRun: Seq[String]): An[Step] = {
    val IndexedBlock.SingleTypedValueDef(Ast.ValueDef(_, e, _), IndexedBlock.DeclaredName(Ast.NameWithPos(name, pos), defId), t) = valueDef
    val signature = currentEnv.signature(defId).getOrElse(
      throw new java.lang.AssertionError(safe"value `$name` hasn't been given a signature")
    )

    valueTerm(currentEnv, e, Some(signature), functionRun).flatMap { valueExprTerm =>
      val fitsSignature = valueExprTerm.fold(An.valid(()))(term => TypeInterpreter.expectAssignable(term.typ, signature, t.pos))

      An.deferErrors(fitsSignature)(()).map { _ =>
        val nextEnv = currentEnv.defineValue(name, defId, signature, pos)
        val target = Terms.PatternAs(Terms.CapturePattern(name, signature), signature)

        Step(nextEnv, valueExprTerm.map(Terms.ValueDef(target, currentEnv.nsPath, _)).toSeq, None)
      }
    }
  }

  def valueDef(currentEnv: Env, valueDef: IndexedBlock.ValueDef, functionRun: Seq[String]): An[Step] = {
    val IndexedBlock.ValueDef(Ast.ValueDef(target, e, _), names) = valueDef

    valueTerm(currentEnv, e, None, functionRun).flatMap { valueExprTerm =>
      val valueType = valueExprTerm.fold[Types.Type](Types.ErrorType)(_.typ)
      val targetTerm = PatternAnalyzer.walkValueDef(currentEnv, target, Some(valueType), valueDef.defIdsByName)

      An.deferErrors(targetTerm.map { case (nextEnv, term) => (nextEnv, Option(term)) })((currentEnv, None)).flatMap {
        case (nextEnv, targetTerm) =>
          val terms = targetTerm.zip(valueExprTerm).map { case (t, v) => Terms.ValueDef(t, currentEnv.nsPath, v) }

          An.valid(Step(defineUndefinedAsErrorType(nextEnv, names), terms.toSeq, None))
      }
    }
  }

  def valueTerm(
    currentEnv: Env,
    e: Ast.ValueExpr,
    expectedType: Option[Types.Type],
    functionRun: Seq[String],
  ): An[Option[Terms.ValueExpr]] = {
    val functionRunIds = functionRun.flatMap(currentEnv.local.values.get).map(_.defId).toSet
    val valueEnv = currentEnv.allowForwardReferences(functionRunIds)

    An.deferErrors(ValueExprAnalyzer.valueExprTermExpecting(valueEnv, e, expectedType).map(Option(_)))(None)
  }

  def defineUndefinedAsErrorType(env: Env, names: Seq[IndexedBlock.DeclaredName]): Env =
    names.foldLeft(env) { case (env, IndexedBlock.DeclaredName(Ast.NameWithPos(name, pos), defId)) =>
      env.defineValue(name, defId, Types.ErrorType, pos)
    }

  def namespaceBody(currentEnv: Env, body: IndexedBlock.NamespaceBody, contents: Vector[IndexedBlock.Content]): An[Step] = {
    val bodyEnv = currentEnv.enterNamespace(body.name, body.nsId)

    An.stepDeferring(withFunctionRuns(contents))((bodyEnv, Vector.empty[Terms.BlockContent])) {
      case ((bodyEnv, terms), (member, functionRun)) =>
        content(bodyEnv, body.block, member, functionRun, isLast = false).
          map(step => (step.nextEnv, terms ++ step.contentTerms))
    }.map { case (bodyEnv, terms) => Step(currentEnv.leaveNamespace(bodyEnv), terms, None) }
  }
}
