package clara.analyzer.impl

import clara.asg.TypeCons
import clara.ast.Ast

case class TypeParamAnalyzer(parentEnv: Env) {
  def walkTypeParams(typeParams: Seq[Ast.TypeParam]): An[(Env, Seq[TypeCons.ParamCon])] =
    An.step(typeParams)(
      // FIXME starting the nested scope probably should be done by caller
      // the fact that lambda parameter scope actually starts here is kind of weird even though it works correctly
      // but just feels bit like a happy accident
      (parentEnv.startNestedScope, Vector[TypeCons.ParamCon]())
    ) { case ((currentEnv, currentParams), Ast.TypeParam(name, pos)) =>
      val paramCon = TypeCons.ParamCon(name, pos)

      currentEnv.addOrShadowTypeCon(name, paramCon, pos).map { nextEnv =>
        (nextEnv, currentParams :+ paramCon)
      }
    }
}
