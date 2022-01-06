package clara.analyzer.impl

import clara.ast.{SourceMessage, Pos}
import clara.asg.{Terms, Types}

import clara.util.Safe._

case class MemberSelectionAnalyzer(env: Env, name: String, memberPos: Pos) {
  def walkMemberSelection(objectTerm: Terms.ValueExpr): An[(Terms.SelectedMember, Types.MonoType)] = {
    lazy val memberNotFound = SourceMessage(memberPos, safe"`$name` is not a member of type `${Types.toSource(objectTerm.typ)}`")

    memberOfType(objectTerm.typ).flatMap(memberOpt => An.fromSomeOrError(memberOpt, memberNotFound))
  }

  def memberOfType(objectType: Types.Type): An[Option[(Terms.SelectedMember, Types.MonoType)]] = objectType match {
    case Types.Record(fields) => An.result(fields.get(name).map(typ => (Terms.SelectedField, typ)))
    case Types.Alias(_, wrappedType) => memberOfType(wrappedType)
    case u @ Types.Unique(_, wrappedType, _) => methodOfType(u).flatMap {
      case m @ Some(_) => An.result(m)
      case None => memberOfType(wrappedType)
    }
    case _ => An.result(None)
  }

  def methodOfType(objectType: Types.Unique): An[Option[(Terms.SelectedMember, Types.MonoType)]] = {
    val methodOpt = env.methods.get(objectType.uniq).flatMap(ns => ns.get(name))

    methodOpt match {
      case Some(EnvMethod(attributes, typ)) =>
        val typeArgs = Nil // FIXME get from objectTerm.typ if it's Applied
        TypeAnalyzer.instantiate(typ, typeArgs, memberPos).map { typeInst =>
          Some((Terms.SelectedMethod(attributes), typeInst))
        }
      case None => An.result(None)
    }
  }
}
