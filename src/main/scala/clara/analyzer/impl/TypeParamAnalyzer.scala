package clara.analyzer.impl

import clara.asg.TypeCons
import clara.ast.Ast

case class TypeParamAnalyzer(parentEnv: Env) {
  def walkTypeParams(typeParams: Seq[Ast.TypeParam]): An[(Env, Seq[TypeCons.ParamCon])] =
    withTypeParams(typeParams.map { case Ast.TypeParam(name, pos) => TypeCons.ParamCon(name, pos) })

  // a duplicate type param is reported and left out
  def withTypeParams(paramCons: Seq[TypeCons.ParamCon]): An[(Env, Seq[TypeCons.ParamCon])] =
    // FIXME starting the nested scope probably should be done by caller
    // the fact that lambda parameter scope actually starts here is kind of weird even though it works correctly
    // but just feels bit like a happy accident
    An.stepDeferring(paramCons)((parentEnv.startNestedScope, Vector.empty[TypeCons.ParamCon])) {
      case ((env, addedParamCons), paramCon) =>
        env.addTypeCon(paramCon.name, paramCon, paramCon.definedAt).map(nextEnv => (nextEnv, addedParamCons :+ paramCon))
    }
}
