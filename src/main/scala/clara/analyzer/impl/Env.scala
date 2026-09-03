package clara.analyzer.impl

import clara.util.Safe._

import clara.ast.{Pos, SourceMessage}
import clara.asg.{Namespace, TypeCons, Uniq}
import clara.asg.Types.Type
import clara.asg.TypeCons.TypeCon
import clara.asg.Attributes.MethodAttributes


// TODO: is attributes actually used/needed here?
case class EnvValue(typ: Type, definedAt: Pos, uniq: Uniq)
case class EnvMethod(attributes: MethodAttributes, typ: Type)

case class BlockScope(values: Namespace[EnvValue], typeCons: Namespace[TypeCon])

object BlockScope {
  def empty: BlockScope = BlockScope(Namespace.empty, Namespace.empty)
}

case class Env(
  parent: BlockScope,
  local: BlockScope,
  private val methods: UniqInfo[Namespace[EnvMethod]],
) {
  def addOrShadowValue(name: String, typ: Type, pos: Pos): An[Env] = {
    local.values.add((name, EnvValue(typ, pos, Uniq()))) match {
      case Some(updatedNs) => An.result(this.copy(local = local.copy(values = updatedNs)))
      case None => An.error(SourceMessage(pos, safe"Cannot shadow existing value with same name `${name}`"))
    }
  }
  def addOrShadowTypeCon(name: String, con: TypeCon, pos: Pos): An[Env] = {
    local.typeCons.add((name, con)) match {
      case Some(updatedNs) => An.result(this.copy(local = local.copy(typeCons = updatedNs)))
      case None => An.error(SourceMessage(pos, safe"Cannot shadow existing type with same name `${name}`"))
    }
  }
  def addMethod(con: TypeCon, name: String, m: EnvMethod, pos: Pos): An[Env] = {
    lazy val error =
      SourceMessage(pos, safe"Method with name `${name}` already exists for type `${TypeCons.toSource(con)}`")
    val methodsUpdated = methods.get(con.uniq).getOrElse(Namespace.empty[EnvMethod]).
      add((name, m)).map(ns => methods.addOrModify((con.uniq, ns)))

    An.fromSomeOrError(methodsUpdated, error).map(m => this.copy(methods = m))
  }
  def getValue(name: String): Option[EnvValue] = local.values.get(name).orElse(parent.values.get(name))
  def getTypeCon(name: String): Option[TypeCon] = local.typeCons.get(name).orElse(parent.typeCons.get(name))
  def startNestedScope: Env = this.copy(
    parent = BlockScope(
      values = parent.values.mergeShadowingEverything(local.values),
      typeCons = parent.typeCons.mergeShadowingEverything(local.typeCons),
    ),
    local = BlockScope.empty,
  )
}

object Env {
  def empty: Env = Env(BlockScope.empty, BlockScope.empty, UniqInfo.empty)
}
