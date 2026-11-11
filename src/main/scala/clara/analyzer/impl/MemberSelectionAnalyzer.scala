package clara.analyzer.impl

import clara.ast.{SourceMessage, Pos}
import clara.asg.{Terms, TypeCons, Types}

import clara.util.Safe._

case class MemberSelectionAnalyzerImpl(env: Env, name: String, memberPos: Pos) {
  def memberSelection(objectTerm: Terms.ValueExpr): An[(Terms.SelectedMember, Types.Type)] = {
    lazy val memberNotFound = SourceMessage(memberPos, safe"`$name` is not a member of type `${Types.toSource(objectTerm.typ)}`")

    memberOfType(objectTerm.typ).flatMap(memberOpt => An.fromSomeOrError(memberOpt, memberNotFound))
  }

  def memberOfType(objectType: Types.Type): An[Option[(Terms.SelectedMember, Types.Type)]] = objectType match {
    case Types.ErrorType => An.valid(Some((Terms.SelectedField, Types.ErrorType)))
    case Types.Record(fields) => An.valid(fields.get(name).map(typ => (Terms.SelectedField, typ)))
    case Types.Union(ts) =>
      // TODO: fields of records would be quite straightforward
      // TODO: methods of union types requires pattern match in runtime to resolve the location of the method
      ???
    case Types.Intersection(ts) =>
      // TODO: ts.find(t => memberOfType(t))
      ???
    case t: Types.Nominal => methodOfType(t)
    case _ => An.valid(None)
  }

  def methodOfType(objectType: Types.Nominal): An[Option[(Terms.SelectedMethod, Types.Type)]] = objectType match {
    case _: Types.Param | _: Types.Singleton => An.valid(None)
    case t: Types.Alias => ???
    case t: Types.Tagged => method(t.con, t.typeArgs)
    case t: Types.Boxed => method(t.con, t.typeArgs)
    case t: Types.Opaque => method(t.con, t.typeArgs)
  }

  def method(con: TypeCons.TypeCon, typeArgs: Seq[Types.Type]): An[Option[(Terms.SelectedMethod, Types.Type)]] =
    env.method(con, name) match {
      case None => An.valid(None)
      case Some(EnvMethod(attributes, typeParams, typ, defId, definedInNs)) =>
        val term = Terms.SelectedMethod(definedInNs, attributes)
        val typeInst = Types.substituteParams(typeParams.map(_.tcId).zip(typeArgs).toMap, typ)

        An.valid(Some((term, typeInst))).tellUsage(defId, memberPos)
    }
}

object MemberSelectionAnalyzer {
  def memberSelection(env: Env, objectTerm: Terms.ValueExpr, name: String, memberPos: Pos) =
    MemberSelectionAnalyzerImpl(env, name, memberPos).memberSelection(objectTerm)
}
