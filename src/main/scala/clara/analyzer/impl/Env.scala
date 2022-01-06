package clara.analyzer.impl

import clara.util.Safe._

import clara.ast.{Pos, SourceMessage}
import clara.asg.{Types, Namespace}
import clara.asg.Attributes.MethodAttributes
import clara.asg.Types.{MonoType, Type}

// TODO: is attributes actually used/needed here?
case class EnvMethod(attributes: MethodAttributes, typ: Type)

case class Env(types: Namespace[Type], values: Namespace[MonoType], methods: UniqInfo[Namespace[EnvMethod]]) {
  // def getValue(name: String): Option[Type] = values.get(name)
  def useValue(name: String, pos: Pos): An[MonoType] = An.fromSomeOrError(values.get(name), SourceMessage(pos, safe"Not found: value `$name`"))
  // def getType(name: String): Option[Type] = types.get(name)
  def useType(name: String, pos: Pos): An[Type] =
    An.fromSomeOrError(types.get(name), SourceMessage(pos, safe"Not found: type `$name`"))

  def useTypeInst(name: String, typeArgs: Seq[MonoType], pos: Pos): An[MonoType] =
    useType(name, pos).flatMap { typ =>
      TypeAnalyzer.instantiate(typ, typeArgs, pos)
    }

  // def addValue(binding: (String, Type), pos: Pos) = addOrShadowValue(binding, Env.empty, pos)
  def addOrShadowValue(binding: (String, MonoType), allowShadow: Env, pos: Pos): An[Env] = {
    val ns = values.addOrShadow(binding, allowShadow.values)
    lazy val error = SourceMessage(pos, safe"Cannot shadow existing value with same name `${binding._1}`")

    An.fromSomeOrError(ns, error).map(v => this.copy(values = v))
  }
  // def addType(binding: (String, Type), pos: Pos) = addOrShadowType(binding, Env.empty, pos)
  def addOrShadowType(binding: (String, Type), allowShadow: Env, pos: Pos): An[Env] = {
    val ns = types.addOrShadow(binding, allowShadow.types)
    lazy val error = SourceMessage(pos, safe"Cannot shadow existing type with same name `${binding._1}`")

    An.fromSomeOrError(ns, error).map(t => this.copy(types = t))
  }
  def addMethod(u: Types.Unique, binding: (String, EnvMethod), pos: Pos): An[Env] = {
    lazy val error = SourceMessage(pos, safe"Method with name `${binding._1}` already given for type `${Types.toSource(u)}`")
    val o = methods.get(u.uniq).getOrElse(Namespace.empty[EnvMethod]).add(binding).map(ns => methods.addOrModify((u.uniq, ns)))
    An.fromSomeOrError(o, error).map(m => this.copy(methods = m))
  }
}

object Env {
  def empty = Env(Namespace.empty[Type], Namespace.empty[MonoType], UniqInfo.empty[Namespace[EnvMethod]])
}
