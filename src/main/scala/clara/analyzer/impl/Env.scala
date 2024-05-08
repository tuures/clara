package clara.analyzer.impl

import clara.util.Safe._

import clara.ast.{Pos, SourceMessage}
import clara.asg.{Namespace, TypeCons}
import clara.asg.Types.Type
import clara.asg.TypeCons.TypeCon
import clara.asg.Attributes.MethodAttributes


// TODO: is attributes actually used/needed here?
case class EnvMethod(attributes: MethodAttributes, typ: Type)

case class Env(typeCons: Namespace[TypeCon], values: Namespace[Type], methods: UniqInfo[Namespace[EnvMethod]]) {
  def addOrShadowValue(binding: (String, Type), allowShadow: Env, pos: Pos): An[Env] = {
    val ns = values.addOrShadow(binding, allowShadow.values)
    lazy val error = SourceMessage(pos, safe"Cannot shadow existing value with same name `${binding._1}`")

    An.fromSomeOrError(ns, error).map(v => this.copy(values = v))
  }
  def addOrShadowTypeCon(binding: (String, TypeCon), allowShadow: Env, pos: Pos): An[Env] = {
    val ns = typeCons.addOrShadow(binding, allowShadow.typeCons)
    lazy val error = SourceMessage(pos, safe"Cannot shadow existing type with same name `${binding._1}`")

    An.fromSomeOrError(ns, error).map(t => this.copy(typeCons = t))
  }
  def addMethod(con: TypeCon, binding: (String, EnvMethod), pos: Pos): An[Env] = {
    lazy val error =
      SourceMessage(pos, safe"Method with name `${binding._1}` already exists for type `${TypeCons.toSource(con)}`")
    val methodsUpdated = methods.get(con.uniq).getOrElse(Namespace.empty[EnvMethod]).
      add(binding).map(ns => methods.addOrModify((con.uniq, ns)))

    An.fromSomeOrError(methodsUpdated, error).map(m => this.copy(methods = m))
  }
}

object Env {
  def empty: Env = Env(Namespace.empty, Namespace.empty, UniqInfo.empty)
}
