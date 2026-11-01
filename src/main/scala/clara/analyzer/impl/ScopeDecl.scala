package clara.analyzer.impl

import clara.asg.{DefId, NsId}
import clara.ast.Ast

// What a scope declares, with the ids of the declarations. They are created once, before the passes of the scope, and
// each pass gets the same ones: a pass acts on an entry of the Env only if it has its declaration's id, as a name can be
// taken by another declaration (a duplicate) or by none (a declaration that failed).
sealed trait ScopeDecl

// a namespace's contents go into nsId whether a name refers to it or not (the name can fail to be registered), so each
// pass finds what the previous ones did and errors in the body are reported either way
case class NamespaceBody(name: Ast.NameWithPos, nsId: NsId, decls: Vector[ScopeDecl])

// a name a value def binds, a pattern can bind several and each is its own declaration
case class DeclaredName(name: Ast.NameWithPos, defId: DefId)

object ScopeDecl {
  // companion: the contents of all the `::extend`s of the type as one body, a type with a constructor always has one
  case class TypeDef(ast: Ast.TypeDef, defId: DefId, companion: Option[NamespaceBody]) extends ScopeDecl {
    def name: Ast.NameWithPos = ast.target.name
  }
  case class Namespace(defId: DefId, body: NamespaceBody) extends ScopeDecl
  // an `::extend` with a type is a part of the type's companion, a type and its companion are one declaration
  case class ExtendWithoutType(name: Ast.NameWithPos) extends ScopeDecl
  case class ValueDecl(ast: Ast.ValueDecl, defId: DefId) extends ScopeDecl
  case class ValueDef(ast: Ast.ValueDef, names: Seq[DeclaredName]) extends ScopeDecl

  def of(bcs: Seq[Ast.BlockContent]): Vector[ScopeDecl] = {
    val typeNames = bcs.collect { case typeDef: Ast.TypeDef => typeDef.target.name.name }.toSet
    // all the type defs of a name get the same companion, only the first one gets the name (the others are duplicates)
    val extendBodies = bcs.collect { case namespaceDef @ Ast.NamespaceDef(true, _, _, _) => namespaceDef }.
      groupBy(_.name.name).
      map { case (name, extendDefs) => name -> NamespaceBody(extendDefs.head.name, NsId(), of(extendDefs.flatMap(_.body))) }

    def companion(typeDef: Ast.TypeDef): Option[NamespaceBody] = extendBodies.get(typeDef.target.name.name).orElse {
      if (constructibleKind(typeDef)) Some(NamespaceBody(typeDef.target.name, NsId(), Vector.empty)) else None
    }

    bcs.toVector.flatMap {
      case typeDef: Ast.TypeDef => Some(TypeDef(typeDef, DefId(), companion(typeDef)))
      case Ast.NamespaceDef(true, name, _, _) => if (typeNames(name.name)) None else Some(ExtendWithoutType(name))
      case Ast.NamespaceDef(false, name, body, _) => Some(Namespace(DefId(), NamespaceBody(name, NsId(), of(body))))
      case valueDecl: Ast.ValueDecl => Some(ValueDecl(valueDecl, DefId()))
      case valueDef: Ast.ValueDef =>
        Some(ValueDef(valueDef, PatternAnalyzer.boundNames(valueDef.target).map(DeclaredName(_, DefId()))))
      case _: Ast.MethodSection | _: Ast.ValueExpr => None
    }
  }

  // FIXME: belongs in Types?
  def constructibleKind(typeDef: Ast.TypeDef): Boolean = typeDef.typeDefKind match {
    case Ast.TypeDefKind.Tagged | Ast.TypeDefKind.Boxed => true
    case _ => false
  }

  // the name and type of a value def whose target is a single annotated capture (`f: T = ...`): its type is known
  // before its value expression is analyzed. The annotations inside other patterns (e.g. a tuple's) are only checked
  // by the values pass.
  def topLevelSignature(target: Ast.Pattern): Option[(Ast.NameWithPos, Ast.TypeExpr)] = target match {
    case Ast.PatternAs(Ast.CapturePattern(name, pos), t, _) => Some((Ast.NameWithPos(name, pos), t))
    case _: Ast.WildcardPattern | _: Ast.UnitPattern | _: Ast.IntegerPattern | _: Ast.FloatPattern |
      _: Ast.StringPattern | _: Ast.TuplePattern | _: Ast.NamedConstantPattern | _: Ast.CapturePattern |
      _: Ast.GuardPattern | _: Ast.DefaultValuePattern | _: Ast.PatternAs | _: Ast.RecordPattern | _: Ast.OrPattern |
      _: Ast.ConstructPattern => None
  }
}
