package clara.analyzer.impl

import ai.x.safe._

import clara.ast.{Pos, SourceMessage}
import clara.asg.{Terms, Types, Namespace}
import clara.asg.Types.Typ

case class Env(types: Namespace[Typ], values: Namespace[Typ], methods: TypeInfo[Terms.MethodSection]) {
  // def getValue(name: String): Option[Typ] = values.get(name)
  def useValue(name: String, pos: Pos): An[Typ] = An.someOrError(values.get(name), SourceMessage(pos, safe"Not found: value `$name`"))
  // def getType(name: String): Option[Typ] = types.get(name)
  def useType(name: String, pos: Pos): An[Typ] = An.someOrError(types.get(name), SourceMessage(pos, safe"Not found: type `$name`"))

  // def addValue(binding: (String, Typ), pos: Pos) = addOrShadowValue(binding, Env.empty, pos)
  def addOrShadowValue(binding: (String, Typ), allowShadow: Env, pos: Pos): An[Env] = {
    val ns = values.addOrShadow(binding, allowShadow.values)
    lazy val error = SourceMessage(pos, safe"Cannot shadow existing value with same name `${binding._1}`")

    An.someOrError(ns, error).map(v => this.copy(values = v))
  }
  // def addType(binding: (String, Typ), pos: Pos) = addOrShadowType(binding, Env.empty, pos)
  def addOrShadowType(binding: (String, Typ), allowShadow: Env, pos: Pos): An[Env] = {
    val ns = types.addOrShadow(binding, allowShadow.types)
    lazy val error = SourceMessage(pos, safe"Cannot shadow existing type with same name `${binding._1}`")

    An.someOrError(ns, error).map(t => this.copy(types = t))
  }
  def addMethods(binding: (Typ, Terms.MethodSection), pos: Pos): An[Env] = {
    lazy val error = SourceMessage(pos, safe"Methods already given for type `${Types.toSource(binding._1)}`")
    An.someOrError(methods.add(binding), error).map(m => this.copy(methods = m))
  }
}

object Env {
  def empty = Env(Namespace.empty[Typ], Namespace.empty[Typ], TypeInfo.empty[Terms.MethodSection])
}
