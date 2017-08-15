package clara

object JsEmitter {

  class Indent(val chars: String = "  ", val level: Int = 0) {
    def add = new Indent(chars, level + 1)
    override val toString = chars * level
  }

  def emitString(valueExpr: Parser.ValueExpr): String = Impl.walkValueExpr(new Indent())(valueExpr)

  object Impl {
    import Parser._

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
          case e: ValueExpr => walkValueExpr(indentItems)(e)
        }

        val body = (items.init :+ s"return ${items.last}").map(i => s"${indentItems}$i;\n")

        s"(() => {\n${body.init.mkString}\n${body.last}$indent})()"
      }
      case NamedValue(name) => s"$name"
      case ValueAs(e, _) => walkValueExpr(indent)(e)
      case Lambda(parameter, body) => {
        val p = ???
        val b = walkValueExpr(indent)(body)

        s"($p) => $b"
      }
      case Member(e, member) => s"${walkValueExpr(indent)(e)}.$member"
      case Call(callee, argument) => s"${walkValueExpr(indent)(callee)}(${walkValueExpr(indent)(argument)})"
    }

    def walkValueDef(indent: Indent)(target: Pattern, e: ValueExpr): String = target match {
      case NamePattern(name) => s"const $name = ${walkValueExpr(indent)(e)}"
    }

  }
}
