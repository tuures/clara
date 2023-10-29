package clara.analyzer.impl

import clara.asg.TypeCons
import clara.ast.Ast

case class TypeParamAnalyzer(parentEnv: Env) {
  def walkTypeParams(typeParams: Seq[Ast.TypeParam]): An[(Env, Seq[TypeCons.ParamCon])] =
    An.step(typeParams)((parentEnv, Vector[TypeCons.ParamCon]())) { case ((currentEnv, currentParams), Ast.TypeParam(name, pos)) =>
      val paramCon = TypeCons.ParamCon(name, pos)

      currentEnv.addOrShadowTypeCon((name, paramCon), parentEnv, pos).map { nextEnv =>
        (nextEnv, currentParams :+ paramCon)
      }
    }
}
