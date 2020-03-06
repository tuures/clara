package clara.analyzer.impl

import clara.asg.{Terms, Types}
import clara.ast.{Ast, Pos, SourceMessage}

import ai.x.safe._
import clara.ast.Ast.Method

case class ValueExprAnalyzer(env: Env) {
  def walkValueExpr(valueExpr: Ast.ValueExpr): An[Terms.ValueExpr] = valueExpr match {
    case Ast.UnitLiteral(_) => An.result(Terms.UnitLiteral())
    case Ast.IntegerLiteral(value, pos) => env.useType("Int", pos).map { typ =>
      Terms.IntegerLiteral(value, typ)
    }
    case Ast.FloatLiteral(value, pos) => env.useType("Float", pos).map { typ =>
      Terms.FloatLiteral(value, typ)
    }
    case Ast.StringLiteral(parts, pos) => env.useType("String", pos).map { typ =>
      Terms.StringLiteral(parts, typ)
    }
    case Ast.Block(bcs, pos) => BlockAnalyzer(env).walkBlock(bcs, pos)
    case Ast.NamedValue(name, pos) => env.useValue(name, pos).map(typ => Terms.NamedValue(name, typ))
    case Ast.MemberSelection(obj, Ast.NamedMember(name, typeArgs, memberPos), pos) =>
      walkValueExpr(obj).flatMap { objectTerm =>
        walkMemberSelection(objectTerm, name, memberPos).map { case (member, typ) =>
          Terms.MemberSelection(objectTerm, name, member, typ)
        }
      }
    case Ast.Call(callee, argument, pos) =>
      walkValueExpr(callee).zip(walkValueExpr(argument)).flatMap { case (calleeTerm, argumentTerm) =>
        calleeTerm.typ match {
          case Types.Func(parameterType, resultType) =>
            Checks.expectAssignable(argumentTerm.typ, parameterType, argument.pos).map { _: Unit =>
              Terms.Call(calleeTerm, argumentTerm, resultType)
            }
          case _ => An.error(SourceMessage(callee.pos, safe"Cannot call type `${calleeTerm.typ.toString}`"))
        }
      }
  }

  def walkMemberSelection(
    objectTerm: Terms.ValueExpr,
    name: String,
    memberPos: Pos
  ): An[(Terms.Member, Types.Typ)] = {
    lazy val memberNotFound = SourceMessage(memberPos, safe"`$name` is not a member of ${objectTerm.typ.toString}")

    val methodType = env.getMethods(objectTerm.typ).flatMap {
      case declSection: Terms.MethodDeclSection => declSection.methodDecls.get(name).map(m => (m, m.typ))
      case defSection: Terms.MethodDefSection => defSection.methodDefs.get(name).map(m => (m, m.body.typ))
    }

    An.someOrError(methodType, memberNotFound)
  }

}
