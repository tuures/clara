package clara.analyzer.impl

import ai.x.safe._

import clara.ast.{Pos, SourceMessage}
import clara.asg.{Terms, Types, Namespace}
import clara.asg.Types.{MonoType, Type}

case class Env(types: Namespace[Type], values: Namespace[MonoType], methods: TypeInfo[Terms.MethodSection]) {
  // def getValue(name: String): Option[Type] = values.get(name)
  def useValue(name: String, pos: Pos): An[MonoType] = An.someOrError(values.get(name), SourceMessage(pos, safe"Not found: value `$name`"))
  // def getType(name: String): Option[Type] = types.get(name)
  def useType(name: String, typeArgs: Seq[MonoType], pos: Pos): An[MonoType] =
    An.someOrError(types.get(name), SourceMessage(pos, safe"Not found: type `$name`")).flatMap { typ =>
      TypeAnalyzer.instantiate(typ, typeArgs, pos)
    }

  // def addValue(binding: (String, Type), pos: Pos) = addOrShadowValue(binding, Env.empty, pos)
  def addOrShadowValue(binding: (String, MonoType), allowShadow: Env, pos: Pos): An[Env] = {
    val ns = values.addOrShadow(binding, allowShadow.values)
    lazy val error = SourceMessage(pos, safe"Cannot shadow existing value with same name `${binding._1}`")

    An.someOrError(ns, error).map(v => this.copy(values = v))
  }
  // def addType(binding: (String, Type), pos: Pos) = addOrShadowType(binding, Env.empty, pos)
  def addOrShadowType(binding: (String, Type), allowShadow: Env, pos: Pos): An[Env] = {
    val ns = types.addOrShadow(binding, allowShadow.types)
    lazy val error = SourceMessage(pos, safe"Cannot shadow existing type with same name `${binding._1}`")

    An.someOrError(ns, error).map(t => this.copy(types = t))
  }
  def addMethods(binding: (Type, Terms.MethodSection), pos: Pos): An[Env] = {
    lazy val error = SourceMessage(pos, safe"Methods already given for type `${Types.toSource(binding._1)}`")
    An.someOrError(methods.add(binding), error).map(m => this.copy(methods = m))
  }
}

object Env {
  def empty = Env(Namespace.empty[Type], Namespace.empty[MonoType], TypeInfo.empty[Terms.MethodSection])
}
