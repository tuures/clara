package clara.analyzer.impl

import clara.asg.{DefId, NsId, Terms, TypeCons}
import clara.ast.{Ast, Pos, SourceMessage}

import clara.util.Safe._


case class NameResolverImpl(env: Env) {

  sealed trait Lookup {
    def valueEntry(name: String): Option[EnvValueEntry]
    def typeEntry(name: String): Option[EnvTypeEntry]
    def qualified(name: String): String
    def notFound(kind: String, name: String, pos: Pos): SourceMessage
    def enterNs(name: String, members: BlockScope): NamespaceLookup
  }

  case object PathlessLookup extends Lookup {
    def valueEntry(name: String): Option[EnvValueEntry] = env.getValueEntry(name)
    def typeEntry(name: String): Option[EnvTypeEntry] = env.getTypeEntry(name)
    def qualified(name: String): String = name
    def notFound(kind: String, name: String, pos: Pos): SourceMessage = SourceMessage(pos, safe"Unknown $kind `$name`")
    def enterNs(name: String, members: BlockScope): NamespaceLookup = NamespaceLookup(Vector(name), members)
  }

  case class NamespaceLookup(nsPath: Vector[String], members: BlockScope) extends Lookup {
    def valueEntry(name: String): Option[EnvValueEntry] = members.values.get(name)
    def typeEntry(name: String): Option[EnvTypeEntry] = members.types.get(name)
    def qualified(name: String): String = (nsPath :+ name).safeString(".")
    def notFound(kind: String, name: String, pos: Pos): SourceMessage =
      SourceMessage(pos, safe"`$name` is not a member of namespace `${nsPath.safeString(".")}`")
    def enterNs(name: String, members: BlockScope): NamespaceLookup = NamespaceLookup(nsPath :+ name, members)
  }

  def value(nsPath: Seq[Ast.NameWithPos], name: String, pos: Pos): An[Terms.NamedValue] =
    lookupIn(nsPath, lookupNsValueSide).flatMap(valueIn(_, name, pos))

  def valueIn(lookup: Lookup, name: String, pos: Pos): An[Terms.NamedValue] = lookup.valueEntry(name) match {
    case Some(EnvValue(typ, defId, definedInNs, _)) =>
      An.valid(Terms.NamedValue(name, definedInNs, typ)).tellUsage(defId, pos)
    // a bare namespace `Bar` is `Bar.apply`, a tagged or boxed type's companion has its constructor as `apply`
    case Some(EnvNamespaceRef(nsId, defId)) if hasApply(nsId) =>
      valueIn(lookup.enterNs(name, env.namespaceMembers(nsId)), TypeCons.ConstructorName, pos).tellUsage(defId, pos)
    case Some(_: EnvNamespaceRef) =>
      An.error(SourceMessage(pos, safe"`${lookup.qualified(name)}` is a namespace, not a value"))
    case Some(_: EnvDeclaredValue) =>
      An.error(SourceMessage(pos, safe"`${lookup.qualified(name)}` is used before its definition"))
    case None => lookup.typeEntry(name) match {
      case Some(_) =>
        An.error(SourceMessage(pos, safe"Unknown value `${lookup.qualified(name)}`. Type `${lookup.qualified(name)}` cannot be used as a value."))
      case None => An.error(lookup.notFound("value", name, pos))
    }
  }

  def hasApply(nsId: NsId): Boolean = env.namespaceMembers(nsId).values.get(TypeCons.ConstructorName).isDefined

  def typeCon(nsPath: Seq[Ast.NameWithPos], name: String, pos: Pos): An[TypeCons.TypeCon] =
    lookupIn(nsPath, lookupNsTypeSide).flatMap { lookup =>
      lookup.typeEntry(name) match {
        case Some(EnvTypeCon(con, defId)) => An.valid(con).tellUsage(defId, pos)
        case Some(EnvCompanion(con, _, defId)) => An.valid(con).tellUsage(defId, pos)
        case Some(_: EnvNamespace) =>
          An.error(SourceMessage(pos, safe"`${lookup.qualified(name)}` is a namespace, not a type"))
        case None => An.error(lookup.notFound("type", name, pos))
      }
    }

  // check if obj is a namespace without logging any usage
  // returns Some(nsPath) if obj is a namespace, None if obj is a value
  def valueNsPath(obj: Ast.ValueExpr): Option[Vector[Ast.NameWithPos]] =
    namePath(obj).filter(isNamespace(_, lookupNsValueSide))

  // `Outer.Inner` as a namePath, None for other expressions
  def namePath(valueExpr: Ast.ValueExpr): Option[Vector[Ast.NameWithPos]] = valueExpr match {
    case Ast.NamedValue(name, pos) => Some(Vector(Ast.NameWithPos(name, pos)))
    case Ast.MemberSelection(obj, Ast.NamedValue(name, pos), _) => namePath(obj).map(_ :+ Ast.NameWithPos(name, pos))
    case _ => None
  }

  // where the last name of a qualified name is looked up, the use of each namespace of the nsPath is logged
  def lookupIn(
    nsPath: Seq[Ast.NameWithPos],
    lookupNs: (Lookup, Ast.NameWithPos) => Either[NamespaceLookupFailure, PathNamespace],
  ): An[Lookup] =
    resolvePathToNs(nsPath, lookupNs) match {
      case Right(ResolvedPath(lookup, uses)) => An.valid(lookup).tellUsages(uses)
      case Left(failure) => An.error(failure.message)
    }

  // only a query, logs nothing
  def isNamespace(
    nsPath: Seq[Ast.NameWithPos],
    lookupNs: (Lookup, Ast.NameWithPos) => Either[NamespaceLookupFailure, PathNamespace],
  ): Boolean =
    nsPath.nonEmpty && resolvePathToNs(nsPath, lookupNs).isRight

  case class ResolvedPath(lookup: Lookup, uses: Vector[(DefId, Pos)])

  // segment by segment look up each name of the nsPath to find the final (deepest) namespace
  def resolvePathToNs(
    nsPath: Seq[Ast.NameWithPos],
    lookupNs: (Lookup, Ast.NameWithPos) => Either[NamespaceLookupFailure, PathNamespace],
  ): Either[NamespaceLookupFailure, ResolvedPath] = {
    // if nsPath is empty this will be returned
    val start: Either[NamespaceLookupFailure, ResolvedPath] = Right(ResolvedPath(PathlessLookup, Vector.empty))

    nsPath.foldLeft(start) { (either, name) =>
      either.flatMap { case ResolvedPath(lookup, uses) =>
        lookupNs(lookup, name).map { case PathNamespace(nsId, defId) =>
          val nextLookup = lookup.enterNs(name.name, env.namespaceMembers(nsId))
          val nextUses = uses :+ (defId -> name.pos)

          ResolvedPath(nextLookup, nextUses)
        }
      }
    }
  }

  // the namespace a name of an nsPath names, and its DefId for logging its use
  case class PathNamespace(nsId: NsId, defId: DefId)

  // The value side is used for values and patterns, the type side for types, so an inner value `Color` shadows an outer
  // namespace `Color` in values but not in types
  def lookupNsValueSide(lookup: Lookup, name: Ast.NameWithPos): Either[NamespaceLookupFailure, PathNamespace] =
    lookup.valueEntry(name.name) match {
      case Some(EnvNamespaceRef(nsId, defId)) => Right(PathNamespace(nsId, defId))
      case Some(_: EnvValue | _: EnvDeclaredValue) => Left(NotNamespace(lookup, name))
      case None => Left(NoSuchName(lookup, name))
    }

  def lookupNsTypeSide(lookup: Lookup, name: Ast.NameWithPos): Either[NamespaceLookupFailure, PathNamespace] =
    lookup.typeEntry(name.name) match {
      case Some(EnvNamespace(nsId, defId, _)) => Right(PathNamespace(nsId, defId))
      case Some(EnvCompanion(_, nsId, defId)) => Right(PathNamespace(nsId, defId))
      case Some(_: EnvTypeCon) => Left(NotNamespace(lookup, name))
      case None => Left(NoSuchName(lookup, name))
    }

  // the reason why an nsPath doesn't name a namespace as data so that isNamespace can also answer whether it does
  sealed trait NamespaceLookupFailure {
    def message: SourceMessage
  }
  case class NoSuchName(lookup: Lookup, name: Ast.NameWithPos) extends NamespaceLookupFailure {
    def message = lookup.notFound("namespace", name.name, name.pos)
  }
  case class NotNamespace(lookup: Lookup, name: Ast.NameWithPos) extends NamespaceLookupFailure {
    def message = SourceMessage(name.pos, safe"`${lookup.qualified(name.name)}` is not a namespace")
  }

}

object NameResolver {
  def value(env: Env, nsPath: Seq[Ast.NameWithPos], name: String, pos: Pos): An[Terms.NamedValue] =
    NameResolverImpl(env).value(nsPath, name, pos)
  def typeCon(env: Env, nsPath: Seq[Ast.NameWithPos], name: String, pos: Pos): An[TypeCons.TypeCon] =
    NameResolverImpl(env).typeCon(nsPath, name, pos)
  def valueNsPath(env: Env, obj: Ast.ValueExpr): Option[Vector[Ast.NameWithPos]] =
    NameResolverImpl(env).valueNsPath(obj)
}
