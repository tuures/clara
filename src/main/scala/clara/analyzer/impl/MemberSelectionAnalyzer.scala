package clara.analyzer.impl

import clara.ast.{SourceMessage, Pos}
import clara.asg.{Terms, Types}

import clara.util.Safe._

case class MemberSelectionAnalyzer(env: Env, name: String, memberPos: Pos) {
  def walkMemberSelection(objectTerm: Terms.ValueExpr): An[(Terms.SelectedMember, Types.MonoType)] = {
    lazy val memberNotFound = SourceMessage(memberPos, safe"`$name` is not a member of type `${Types.toSource(objectTerm.typ)}`")

    An.someOrError(memberOfType(objectTerm.typ), memberNotFound)
  }

  def memberOfType(typ: Types.Type): Option[(Terms.SelectedMember, Types.MonoType)] = typ match {
    case Types.Record(fields) => fields.get(name).map(typ => (Terms.SelectedField, typ))
    case Types.Alias(_, wrappedType) => memberOfType(wrappedType)
    case u @ Types.Unique(_, wrappedType, _) => methodOfType(u).orElse(memberOfType(wrappedType))
    case _ => None
  }

  // .flatMap {
  //   case declSection: Terms.MethodDeclSection => declSection.methodDecls.get(name).map(m => (m.attributes, m.typ))
  //   case defSection: Terms.MethodDefSection => defSection.methodDefs.get(name).map(m => (m.attributes, m.body.typ))
  // }
  def methodOfType(typ: Types.Unique): Option[(Terms.SelectedMember, Types.MonoType)] =
    env.methods.get(typ.uniq).flatMap { ns =>
      ns.get(name).map { case EnvMethod(attributes, typ) =>
        (Terms.SelectedMethod(attributes), ???/*typ*/)
      }
    }
}
