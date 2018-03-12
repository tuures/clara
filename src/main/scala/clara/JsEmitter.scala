package clara

import ai.x.safe._

object JsEmitter {

  def emitString(valueExpr: Ast.ValueExpr): String = Impl.walkValueExpr(new Impl.Indent())(valueExpr)

  object Impl {
    import Ast._

    class Indent(val chars: String = "  ", val level: Int = 0) {
      def add = new Indent(chars, level + 1)
      override val toString = chars * level
    }

    def walkValueExpr(indent: Indent)(valueExpr: ValueExpr): String = valueExpr match {
      // optimizations
      case Block(Seq(b @ Block(_))) => walkValueExpr(indent)(b)

      // normal rules
      case UnitLiteral() => "undefined"
      case IntegerLiteral(value) => s"$value"
      case StringLiteral(value) => s""""$value""""
      case Tuple(es) => es.map(walkValueExpr(indent)).mkString("[", ", ", "]")
      case Block(bcs) => {
        val indentItems = indent.add

        val items = bcs.map {
          case ValueDef(target, e) => walkValueDef(indentItems)(target, e)
          // case ClassDef(name, contents) => walkClassDef(indentItems)(name, contents)
          case Comment(text) => s"//${text}"
          case e: ValueExpr => walkValueExpr(indentItems)(e)
        }

        val (initValues, lastValue) = bcs.last match {
          case e: ValueExpr => (items.init, items.last)
          case _ => (items, "undefined")
        }

        val body = (initValues :+ s"return $lastValue").map(i => s"${indentItems}$i;\n")

        s"(() => {\n${body.init.mkString}\n${body.last}$indent})()"
      }
      case NamedValue(name) => s"$name"
      case ValueAs(e, _) => walkValueExpr(indent)(e)
      case Lambda(parameter, body) => {
        val p = ???
        val b = walkValueExpr(indent)(body)

        s"($p) => $b"
      }
      case MemberSelection(e, member) => s"${walkValueExpr(indent)(e)}.$member"
      case Call(callee, argument) => s"${walkValueExpr(indent)(callee)}(${walkValueExpr(indent)(argument)})"
    }

    def walkValueDef(indent: Indent)(target: Pattern, e: ValueExpr): String = target match {
      case NamePattern(name) => s"const $name = ${walkValueExpr(indent)(e)}"
    }

    // def walkClassDef(indent: Indent)(name: String, contents: Seq[ClassContent]): String = {
    //   val names: Seq[String] = contents map {
    //     case ClassField(name, _) => name
    //   }
    //   val paramList = names.mkString(", ")
    //   val body = names.map(name => s"this.$name = $name").map(i => s"${indent.add.add}$i;\n").mkString
    //   s"class $name {\n${indent.add}constructor($paramList) {\n$body${indent.add}}\n$indent}"
    // }

  }
}
