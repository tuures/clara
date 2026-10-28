package clara.analyzer.impl

import clara.util.Safe._

import clara.ast.{Pos, SourceMessage}
import clara.asg.{DefId, Namespace, NsId, TypeConId, TypeCons}
import clara.asg.Types.Type
import clara.asg.TypeCons.TypeCon
import clara.asg.Attributes.MethodAttributes


sealed trait EnvValueEntry {
  def defId: DefId
}
case class EnvDeclaredValue(defId: DefId, signature: Option[Type], definedInNs: Seq[String], definedAt: Pos)
  extends EnvValueEntry
case class EnvValue(typ: Type, defId: DefId, definedInNs: Seq[String], definedAt: Pos) extends EnvValueEntry
case class EnvNamespaceRef(namespace: NsId, defId: DefId) extends EnvValueEntry

sealed trait EnvTypeEntry {
  def defId: DefId
}
case class EnvTypeCon(con: TypeCon, defId: DefId) extends EnvTypeEntry
case class EnvNamespace(namespace: NsId, defId: DefId, definedAt: Pos) extends EnvTypeEntry
case class EnvCompanion(con: TypeCon, namespace: NsId, defId: DefId) extends EnvTypeEntry
// TODO: is attributes actually used/needed here?
case class EnvMethod(attributes: MethodAttributes, typ: Type)

case class BlockScope(values: Namespace[EnvValueEntry], types: Namespace[EnvTypeEntry])

object BlockScope {
  def empty: BlockScope = BlockScope(Namespace.empty, Namespace.empty)
}

case class InNamespace(nsPath: Seq[String], nsId: NsId)

case class Env(
  parent: BlockScope,
  local: BlockScope,
  private val methods: UniqInfo[TypeConId, Namespace[EnvMethod]],
  private val namespaces: UniqInfo[NsId, BlockScope],
  private val inNs: Option[InNamespace],
  // see allowForwardReferences
  private val forwardReferable: Set[DefId],
) {
  def nsPath: Seq[String] = inNs.fold(Seq.empty[String])(_.nsPath)

  // scopes
  def startNestedScope: Env = this.copy(
    parent = BlockScope(
      values = parent.values.mergeShadowingEverything(local.values),
      types = parent.types.mergeShadowingEverything(local.types),
    ),
    local = BlockScope.empty,
    inNs = None,
  )
  // the body of a namespace, entered in each pass of its scope: its members so far (none in the names pass) are the
  // local scope, so they're reachable unqualified and the pass adds to them
  def enterNamespace(name: String, nsId: NsId): Env =
    startNestedScope.copy(local = namespaceMembers(nsId), inNs = Some(InNamespace(nsPath :+ name, nsId)))
  // back from the body of a namespace entered from this env: keeps the members the pass added and the namespaces the
  // pass added or updated (e.g. nested ones), the namespaces of the body env are the newer version of this env's
  def leaveNamespace(bodyEnv: Env): Env = this.copy(namespaces = bodyEnv.namespaces)
  // the declared values that the value expressions analyzed with this env can refer to before their definitions
  def allowForwardReferences(defIds: Set[DefId]): Env = this.copy(forwardReferable = forwardReferable ++ defIds)
  def isForwardReferable(defId: DefId): Boolean = forwardReferable(defId)

  // adding names
  def addValue(name: String, typ: Type, pos: Pos, defId: DefId = DefId()): An[Env] =
    addValueEntry(name, EnvValue(typ, defId, nsPath, pos), pos)
  def declareValue(name: String, pos: Pos): An[Env] = addValueEntry(name, EnvDeclaredValue(DefId(), None, nsPath, pos), pos)
  def declareValueSignature(name: String, signature: Type): Env = local.values.get(name) match {
    case Some(declared @ EnvDeclaredValue(_, None, _, _)) =>
      withLocal(local.copy(values = local.values.addOrModify((name, declared.copy(signature = Some(signature))))))
    case _ => throw new java.lang.AssertionError(safe"value `$name` hasn't been declared without a signature")
  }
  def defineValue(name: String, typ: Type, pos: Pos): An[Env] = local.values.get(name) match {
    case Some(EnvDeclaredValue(defId, _, _, _)) =>
      An.valid(withLocal(local.copy(values = local.values.addOrModify((name, EnvValue(typ, defId, nsPath, pos))))))
    case _ => throw new java.lang.AssertionError(safe"value `$name` hasn't been declared")
  }
  def addTypeCon(name: String, con: TypeCon, pos: Pos, defId: DefId = DefId()): An[Env] =
    addTypeEntry(name, EnvTypeCon(con, defId), pos)
  // adding namespaces
  // members are added separately by entering its body (enterNamespace, leaveNamespace)
  def addStandaloneNamespace(name: String, nsId: NsId, pos: Pos): An[Env] = {
    val defId = DefId()

    addTypeEntry(name, EnvNamespace(nsId, defId, pos), pos).
      flatMap(_.addValueEntry(name, EnvNamespaceRef(nsId, defId), pos))
  }
  // a companion namespace attached to the type con of the same name in the local scope, the type and its namespace
  // become one declaration (the type's)
  def addCompanionNamespace(name: String, nsId: NsId): An[Env] = local.types.get(name) match {
    case Some(EnvTypeCon(con, defId)) =>
      withLocal(local.copy(types = local.types.addOrModify((name, EnvCompanion(con, nsId, defId))))).
        addValueEntry(name, EnvNamespaceRef(nsId, defId), con.definedAt)
    case _ => throw new java.lang.AssertionError(safe"no type `$name` to attach a companion namespace to")
  }
  // the compiler generated `apply` of a tagged or boxed type's companion, its constructor
  // the use of the constructor is the use of the type
  def addConstructor(name: String, con: TypeCons.WrapperTypeCon, constructorType: Type, pos: Pos): Env = local.types.get(name) match {
    case Some(EnvCompanion(_, namespace, defId)) =>
      val members = namespaceMembers(namespace)
      val constructor = EnvValue(constructorType, defId, con.definedInNs :+ con.name, pos)

      members.values.add((TypeCons.ConstructorName, constructor)) match {
        case Some(values) => updateNamespaceMembers(namespace, members.copy(values = values))
        case None => throw new java.lang.AssertionError(safe"the companion of `$name` already has an `${TypeCons.ConstructorName}`")
      }
    case _ => throw new java.lang.AssertionError(safe"no companion `$name` to add a constructor to")
  }

  // looking up names
  def getValueEntry(name: String): Option[EnvValueEntry] = local.values.get(name).orElse(parent.values.get(name))
  def getValue(name: String): Option[EnvValue] = getValueEntry(name).collect { case value: EnvValue => value }
  def getTypeEntry(name: String): Option[EnvTypeEntry] = local.types.get(name).orElse(parent.types.get(name))
  // the type con declared with this name in the local scope, with or without a companion
  def localTypeCon(name: String): Option[TypeCon] = local.types.get(name).collect {
    case EnvTypeCon(con, _) => con
    case EnvCompanion(con, _, _) => con
  }
  // the standalone namespace declared with this name in the local scope
  def localStandaloneNamespace(name: String): Option[NsId] = local.types.get(name).collect {
    case EnvNamespace(namespace, _, _) => namespace
  }
  // the companion namespace of the type declared with this name in the local scope
  def localCompanionNamespace(name: String): Option[NsId] = local.types.get(name).collect {
    case EnvCompanion(_, namespace, _) => namespace
  }
  def namespaceMembers(namespace: NsId): BlockScope = namespaces.get(namespace).getOrElse(BlockScope.empty)

  // methods of the standalone ::methods sections (obsolete, see Roadmap 3c)
  def addMethod(con: TypeCon, name: String, m: EnvMethod, pos: Pos): An[Env] = {
    lazy val error =
      SourceMessage(pos, safe"Method with name `${name}` already exists for type `${TypeCons.toSource(con)}`")
    val methodsUpdated = methods.get(con.uniq).getOrElse(Namespace.empty[EnvMethod]).
      add((name, m)).map(ns => methods.addOrModify((con.uniq, ns)))

    An.fromSomeOrError(methodsUpdated, error).map(m => this.copy(methods = m))
  }

  private def updateNamespaceMembers(namespace: NsId, members: BlockScope): Env =
    this.copy(namespaces = namespaces.addOrModify((namespace, members)))
  private def withLocal(nextLocal: BlockScope): Env =
    inNs.foldLeft(this.copy(local = nextLocal))((env, inNs) => env.updateNamespaceMembers(inNs.nsId, nextLocal))

  private def addValueEntry(name: String, entry: EnvValueEntry, pos: Pos): An[Env] =
    local.values.add((name, entry)) match {
      case Some(updatedNs) => An.valid(withLocal(local.copy(values = updatedNs)))
      case None => An.error(SourceMessage(pos, safe"Cannot shadow existing value with same name `${name}`"))
    }
  private def addTypeEntry(name: String, entry: EnvTypeEntry, pos: Pos): An[Env] =
    local.types.add((name, entry)) match {
      case Some(updatedNs) => An.valid(withLocal(local.copy(types = updatedNs)))
      case None => An.error(SourceMessage(pos, safe"Cannot shadow existing type with same name `${name}`"))
    }
}

object Env {
  def empty: Env = Env(BlockScope.empty, BlockScope.empty, UniqInfo.empty, UniqInfo.empty, None, Set.empty)
}
