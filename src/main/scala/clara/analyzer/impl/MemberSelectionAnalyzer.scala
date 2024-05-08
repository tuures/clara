package clara.analyzer.impl

import clara.ast.{SourceMessage, Pos}
import clara.asg.{Terms, Types}

import clara.util.Safe._

case class MemberSelectionAnalyzerImpl(env: Env, name: String, memberPos: Pos) {
  def memberSelection(objectTerm: Terms.ValueExpr): An[(Terms.SelectedMember, Types.Type)] = {
    lazy val memberNotFound = SourceMessage(memberPos, safe"`$name` is not a member of type `${Types.toSource(objectTerm.typ)}`")

    memberOfType(objectTerm.typ).flatMap(memberOpt => An.fromSomeOrError(memberOpt, memberNotFound))
  }

  def memberOfType(objectType: Types.Type): An[Option[(Terms.SelectedMember, Types.Type)]] = objectType match {
    case Types.Record(fields) => An.result(fields.get(name).map(typ => (Terms.SelectedField, typ)))
    case Types.Union(ts) =>
      // TODO: fields of records would be quite straightforward
      // TODO: methods of union types requires pattern match in runtime to resolve the location of the method
      ???
    case Types.Intersection(ts) =>
      // TODO: ts.find(t => memberOfType(t))
      ???
    case t: Types.Nominal => methodOfType(t)
    case _ => An.result(None)
  }

  def methodOfType(objectType: Types.Nominal): An[Option[(Terms.SelectedMethod, Types.Type)]] = objectType match {
    case t: Types.Param => ???
    case t: Types.Alias => ???
    case t: Types.Tagged => ???
    case t: Types.Boxed => ???
    case t: Types.Opaque => ???
    case t: Types.Singleton => ???

    // case Types.Alias(_, wrappedType) => memberOfType(wrappedType)
    // case u @ Types.Unique(_, wrappedType, _) => methodOfType(u).flatMap {
    //   case m @ Some(_) => An.result(m)
    //   case None => memberOfType(wrappedType)
    // }

  //   val methodOpt = env.methods.get(objectType.uniq).flatMap(ns => ns.get(name))

  //   methodOpt match {
  //     case Some(EnvMethod(attributes, typ)) =>
  //       val typeArgs = Nil // FIXME get from objectTerm.typ if it's Applied
  //       TypeAnalyzer.instantiate(typ, typeArgs, memberPos).map { typeInst =>
  //         Some((Terms.SelectedMethod(attributes), typeInst))
  //       }
  //     case None => An.result(None)
  //   }
  }
}

object MemberSelectionAnalyzer {
  def memberSelection(env: Env, objectTerm: Terms.ValueExpr, name: String, memberPos: Pos) =
    MemberSelectionAnalyzerImpl(env, name, memberPos).memberSelection(objectTerm)
}
