package clara.analyzer.impl

import clara.asg.{NsId, Terms, Types}
import clara.ast.{Ast, SourceMessage}

import clara.util.Safe._

import scala.annotation.tailrec

// The third pass of a scope, see BlockAnalyzer
object ValuesPass {
  case class State(
    env: Env,
    contents: Vector[Terms.BlockContent],
    returnType: Option[Types.Type],
  )

  case class Step(nextEnv: Env, contentTerms: Seq[Terms.BlockContent], nextReturnType: Option[Types.Type])

  def walk(env: Env, typeDefTerms: Vector[Terms.TypeDef], bcs: Seq[Ast.BlockContent]): An[State] =
    An.step(bcs.zip(functionRuns(bcs)).zipWithIndex)(State(env, typeDefTerms, returnType = None)) {
      case (State(currentEnv, currentContents, _), ((bc, functionRun), index)) =>
        content(currentEnv, bc, functionRun, index === bcs.length - 1).
          map { case Step(nextEnv, contentTerms, nextReturnType) =>
            State(nextEnv, currentContents ++ contentTerms, nextReturnType)
          }
    }

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

  // functionRun: the names of the run of function defs bc is in (see functionRuns)
  def content(
    currentEnv: Env,
    bc: Ast.BlockContent,
    functionRun: Seq[String],
    isLast: Boolean,
  ): An[Step] = bc match {
    case valueExprAst: Ast.ValueExpr =>
      ValueExprAnalyzer.valueExprTerm(currentEnv, valueExprAst).flatMap { valueExprTerm =>
        val isUnit = valueExprTerm.typ === Types.Uni

        lazy val discardWarning = SourceMessage(bc.pos, "Non-unit value discarded in block")
        val maybeDiscardWarning = if (!isLast && !isUnit) Seq(discardWarning) else Nil

        An.valid(Step(currentEnv, Seq(valueExprTerm), Some(valueExprTerm.typ))).tellWarnings(maybeDiscardWarning)
      }
    // defined by the types pass
    case Ast.ValueDecl(Ast.NameWithPos(name, _), _, _) => An.valid(Step(currentEnv, Seq(Terms.ValueDecl(name)), None))
    case Ast.ValueDef(target, e, _) =>
      val signature = ScopeDecl.topLevelSignature(target).map { case (name, _) =>
        registeredSignature(currentEnv, name.name)
      }

      val functionRunIds = functionRun.flatMap(currentEnv.local.values.get).map(_.defId).toSet
      val valueEnv = currentEnv.allowForwardReferences(functionRunIds)

      ValueExprAnalyzer.valueExprTermExpecting(valueEnv, e, signature).flatMap { valueExprTerm =>
        PatternAnalyzer.walkValueDef(currentEnv, target, Some(valueExprTerm.typ)).
          map { case (nextEnv, targetTerm) =>
            Step(nextEnv, Seq(Terms.ValueDef(targetTerm, currentEnv.nsPath, valueExprTerm)), None)
          }
      }
    // type defs have already been analyzed by the names and types passes, but here affects the block's return value
    // (block should end with an expression)
    case _: Ast.TypeDef => An.valid(Step(currentEnv, Nil, None))
    // the namespace's values become the block's own value defs (they know their namespace)
    // an error in one doesn't lose the others
    case namespaceDef: Ast.NamespaceDef =>
      // the values pass of a scope runs only when its names and types passes had no errors
      val nsId = registeredNamespace(currentEnv, namespaceDef).getOrElse(
        throw new java.lang.AssertionError(safe"namespace `${namespaceDef.name.name}` hasn't been registered")
      )
      val bodyEnv = currentEnv.enterNamespace(namespaceDef.name.name, nsId)

      val members = namespaceDef.body.zip(functionRuns(namespaceDef.body))

      An.stepDeferring(members)((bodyEnv, Vector.empty[Terms.BlockContent])) { case ((bodyEnv, terms), (member, functionRun)) =>
        content(bodyEnv, member, functionRun, isLast = false).map(step => (step.nextEnv, terms ++ step.contentTerms))
      }.map { case (bodyEnv, terms) => Step(currentEnv.leaveNamespace(bodyEnv), terms, None) }
    case methodSection: Ast.MethodSection =>
      MethodSectionAnalyzer.methodSectionTerm(currentEnv, methodSection).
        map { case (nextEnv, term) => Step(nextEnv, Seq(term), None) }
  }

  // the namespace a namespace def registered in the names pass, None if it failed to be registered; an `::extend` finds
  // only a companion, a `::namespace` only a standalone namespace
  def registeredNamespace(env: Env, namespaceDef: Ast.NamespaceDef): Option[NsId] =
    if (namespaceDef.isExtend) env.localCompanionNamespace(namespaceDef.name.name)
    else env.localStandaloneNamespace(namespaceDef.name.name)

  // the signature the value def of this name got in the types pass (see ScopeDecl.topLevelSignature)
  def registeredSignature(env: Env, name: String): Types.Type = env.local.values.get(name) match {
    case Some(EnvDeclaredValue(_, Some(signature), _, _)) => signature
    case _ => throw new java.lang.AssertionError(safe"value `$name` hasn't been given a signature")
  }
}
