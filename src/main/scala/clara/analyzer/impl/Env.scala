package clara.analyzer.impl

import ai.x.safe._

import clara.ast.{Pos, SourceMessage}
import clara.asg.{Terms, Namespace}
import clara.asg.Types.Typ

case class Env(types: Namespace[Typ], values: Namespace[Typ], methods: TypeInfo[Terms.MethodSection]) {
  // def getValue(name: String): Option[Typ] = values.get(name)
  def useValue(name: String, pos: Pos): An[Typ] = An.someOrError(values.get(name), SourceMessage(pos, safe"Not found: value `$name`"))
  // def getType(name: String): Option[Typ] = types.get(name)
  def useType(name: String, pos: Pos): An[Typ] = An.someOrError(types.get(name), SourceMessage(pos, safe"Not found: type `$name`"))


  // def addValue(binding: (String, Typ), pos: Pos) = addOrShadowValue(binding, Env.empty, pos)
  def addOrShadowValue(binding: (String, Typ), allowShadow: Env, pos: Pos): An[Env] = {
    val ns = values.addOrShadow(binding, allowShadow.values)
    lazy val error = SourceMessage(pos, safe"Already defined: value `${binding._1}`")

    An.someOrError(ns, error).map(v => this.copy(values = v))
  }
  // def addType(binding: (String, Typ), pos: Pos) = addOrShadowType(binding, Env.empty, pos)
  def addOrShadowType(binding: (String, Typ), allowShadow: Env, pos: Pos): An[Env] = {
    val ns = types.addOrShadow(binding, allowShadow.types)
    lazy val error = SourceMessage(pos, safe"Already defined: type `${binding._1}`")

    An.someOrError(ns, error).map(t => this.copy(types = t))
  }
  def addMethods(binding: (Typ, Terms.MethodSection), pos: Pos): An[Env] = {
    lazy val error = SourceMessage(pos, "Already defined: methods")
    An.someOrError(methods.add(binding), error).map(m => this.copy(methods = m))
  }
  def getMethods(typ: Typ) = methods.get(typ)
}

object Env {
  def empty = Env(Namespace.empty[Typ], Namespace.empty[Typ], TypeInfo.empty[Terms.MethodSection])
}
