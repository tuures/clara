package clara.analyzer.impl

import clara.asg.{DefId, NsId}
import clara.ast.Ast

// the contents of a block (a block expression, a namespace body, the `::extend`s of a type) enriched with the ids
// created once so that every block analysis pass finds the same ids
object IndexedBlock {
  case class DeclaredName(name: Ast.NameWithPos, defId: DefId)
  case class NamespaceBody(name: String, nsId: NsId, block: IndexedBlock)

  sealed trait Content {
    def ast: Ast.BlockContent
  }

  case class TypeDef(ast: Ast.TypeDef, defId: DefId, companionNsId: NsId) extends Content {
    def name: Ast.NameWithPos = ast.target.name
  }
  case class Namespace(ast: Ast.NamespaceDef, defId: DefId, body: NamespaceBody) extends Content
  case class Extend(ast: Ast.NamespaceDef, contents: Vector[Content]) extends Content
  case class MethodDeclSection(ast: Ast.MethodDeclSection) extends Content
  case class MethodDefSection(ast: Ast.MethodDefSection) extends Content
  case class ValueDecl(ast: Ast.ValueDecl, defId: DefId) extends Content
  case class SingleTypedValueDef(ast: Ast.ValueDef, name: DeclaredName, signature: Ast.TypeExpr) extends Content
  case class ValueDef(ast: Ast.ValueDef, names: Seq[DeclaredName]) extends Content {
    // a name bound twice is the first one's, the other is a duplicate
    def defIdsByName: Map[String, DefId] = names.distinctBy(_.name.name).map(name => name.name.name -> name.defId).toMap
  }
  case class ValueExpr(ast: Ast.ValueExpr) extends Content

  def of(bcs: Seq[Ast.BlockContent]): IndexedBlock = IndexedBlock(contentsOf(bcs))

  private def contentsOf(bcs: Seq[Ast.BlockContent]): Vector[Content] = bcs.toVector.map {
    case typeDef: Ast.TypeDef => TypeDef(typeDef, DefId(), NsId())
    case namespaceDef @ Ast.NamespaceDef(true, _, body, _) => Extend(namespaceDef, contentsOf(body))
    case namespaceDef @ Ast.NamespaceDef(false, name, body, _) =>
      Namespace(namespaceDef, DefId(), NamespaceBody(name.name, NsId(), of(body)))
    case methodDeclSection: Ast.MethodDeclSection => MethodDeclSection(methodDeclSection)
    case methodDefSection: Ast.MethodDefSection => MethodDefSection(methodDefSection)
    case valueDecl: Ast.ValueDecl => ValueDecl(valueDecl, DefId())
    case valueDef @ Ast.ValueDef(Ast.PatternAs(Ast.CapturePattern(name, pos), signature, _), _, _) =>
      SingleTypedValueDef(valueDef, DeclaredName(Ast.NameWithPos(name, pos), DefId()), signature)
    case valueDef: Ast.ValueDef =>
      ValueDef(valueDef, PatternAnalyzer.boundNames(valueDef.target).map(DeclaredName(_, DefId())))
    case valueExpr: Ast.ValueExpr => ValueExpr(valueExpr)
  }

  // FIXME: belongs in Types?
  def constructibleKind(typeDef: Ast.TypeDef): Boolean = typeDef.typeDefKind match {
    case Ast.TypeDefKind.Tagged | Ast.TypeDefKind.Boxed => true
    case _ => false
  }
}

case class IndexedBlock(contents: Vector[IndexedBlock.Content]) {
  private val firstTypeDefs: Map[String, IndexedBlock.TypeDef] =
    contents.collect {
      case typeDef: IndexedBlock.TypeDef => typeDef
    }.distinctBy(_.name.name).map(t => t.name.name -> t).toMap
  // the `::extend`s of a name extend its first type def
  private val extendsByDefId: Map[DefId, Vector[IndexedBlock.Extend]] = {
    val extendsByName = contents.collect { case e: IndexedBlock.Extend => e }.groupBy(_.ast.name.name)

    firstTypeDefs.map { case (name, typeDef) => typeDef.defId -> extendsByName.getOrElse(name, Vector.empty) }
  }

  def firstTypeDef(name: String): Option[IndexedBlock.TypeDef] = firstTypeDefs.get(name)

  def companion(typeDef: IndexedBlock.TypeDef): Option[IndexedBlock.NamespaceBody] = {
    val extensions = extendsByDefId.getOrElse(typeDef.defId, Vector.empty)

    if (extensions.isEmpty && !IndexedBlock.constructibleKind(typeDef.ast)) None
    else Some(IndexedBlock.NamespaceBody(typeDef.name.name, typeDef.companionNsId, IndexedBlock(extensions.flatMap(_.contents))))
  }

  def companionByName(name: String): Option[IndexedBlock.NamespaceBody] = firstTypeDef(name).flatMap(companion)
}
