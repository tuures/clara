package clara.analyzer.impl

import ai.x.safe._

import clara.ast.{Pos, SourceMessage}
import clara.asg.Asg.{TypeCon, TypeInst}

case class Env(types: Namespace[TypeCon], values: Namespace[TypeInst]) {
  def getValue(name: String): Option[TypeInst] = values.get(name)
  def useValue(name: String, pos: Pos): An[TypeInst] = An.someOrError(getValue(name), SourceMessage(pos, safe"Not found: value `$name`"))
  def getType(name: String): Option[TypeCon] = types.get(name)
  def useType(name: String, pos: Pos): An[TypeCon] = An.someOrError(getType(name), SourceMessage(pos, safe"Not found: type `$name`"))
  def addValue(binding: (String, TypeInst), pos: Pos) = addOrShadowValue(binding, Env.empty, pos)
  def addOrShadowValue(binding: (String, TypeInst), allowShadow: Env, pos: Pos): An[Env] = {
    val ns = values.addOrShadow(binding, allowShadow.values)
    lazy val error = SourceMessage(pos, safe"Already defined: value `${binding._1}`")

    An.someOrError(ns, error).map(v => this.copy(values = v))
  }
  def addType(binding: (String, TypeCon), pos: Pos) = addOrShadowType(binding, Env.empty, pos)
  def addOrShadowType(binding: (String, TypeCon), allowShadow: Env, pos: Pos): An[Env] = {
    val ns = types.addOrShadow(binding, allowShadow.types)
    lazy val error = SourceMessage(pos, safe"Already defined: type `${binding._1}`")

    An.someOrError(ns, error).map(t => this.copy(types = t))
  }
}

object Env {
  def empty = Env(Namespace.empty[TypeCon], Namespace.empty[TypeInst])
}
