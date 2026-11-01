package clara.analyzer.impl

import clara.asg.TypeCons
import clara.ast.Ast

case class TypeParamAnalyzer(parentEnv: Env) {
  def walkTypeParams(typeParams: Seq[Ast.TypeParam]): An[(Env, Seq[TypeCons.ParamCon])] = {
    val paramCons = typeParams.map { case Ast.TypeParam(name, pos) => TypeCons.ParamCon(name, pos) }

    withTypeParams(paramCons).map((_, paramCons))
  }

  def withTypeParams(paramCons: Seq[TypeCons.ParamCon]): An[Env] =
    // FIXME starting the nested scope probably should be done by caller
    // the fact that lambda parameter scope actually starts here is kind of weird even though it works correctly
    // but just feels bit like a happy accident
    An.step(paramCons)(parentEnv.startNestedScope) { (currentEnv, paramCon) =>
      currentEnv.addTypeCon(paramCon.name, paramCon, paramCon.definedAt)
    }
}
