// package clara
//
// import ai.x.safe._
//
// object JsEmitter {
//
//   def emitString(valueExpr: Ast.ValueExpr): String = Impl.walkValueExpr(new Impl.Indent())(valueExpr)
//
//   object Impl {
//     import Ast._
//
//     class Indent(val chars: String = "  ", val level: Int = 0) {
//       def add = new Indent(chars, level + 1)
//       override val toString = chars * level
//     }
//
//     def walkValueExpr(indent: Indent)(valueExpr: ValueExpr): String = valueExpr match {
//       // optimizations
//       case Block(Seq(b @ Block(_))) => walkValueExpr(indent)(b)
//
//       // normal rules
//       case UnitLiteral() => "undefined"
//       case IntegerLiteral(value) => safe"$value"
//       case StringLiteral(value) => safe""""$value""""
//       case Tuple(es) => es.map(walkValueExpr(indent)).mkString("[", ", ", "]")
//       case Block(bcs) => {
//         val indentItems = indent.add
//
//         val items = bcs.map {
//           case ValueDef(target, e) => walkValueDef(indentItems)(target, e)
//           // case ClassDef(name, contents) => walkClassDef(indentItems)(name, contents)
//           case Comment(text) => safe"//${text}"
//           case e: ValueExpr => walkValueExpr(indentItems)(e)
//         }
//
//         val (initValues, lastValue) = bcs.last match {
//           case e: ValueExpr => (items.init, items.last)
//           case _ => (items, "undefined")
//         }
//
//         val body = (initValues :+ safe"return $lastValue").map(i => safe"${indentItems}$i;\n")
//
//         safe"(() => {\n${body.init.mkString}\n${body.last}$indent})()"
//       }
//       case NamedValue(name) => safe"$name"
//       case ValueAs(e, _) => walkValueExpr(indent)(e)
//       case Lambda(parameter, body) => {
//         val p = ???
//         val b = walkValueExpr(indent)(body)
//
//         safe"($p) => $b"
//       }
//       case MemberSelection(e, member, _) => safe"${walkValueExpr(indent)(e)}.$member"
//       case Call(callee, argument) => safe"${walkValueExpr(indent)(callee)}(${walkValueExpr(indent)(argument)})"
//     }
//
//     def walkValueDef(indent: Indent)(target: Pattern, e: ValueExpr): String = target match {
//       case NamePattern(name) => safe"const $name = ${walkValueExpr(indent)(e)}"
//     }
//
//     // def walkClassDef(indent: Indent)(name: String, contents: Seq[ClassContent]): String = {
//     //   val names: Seq[String] = contents map {
//     //     case ClassField(name, _) => name
//     //   }
//     //   val paramList = names.mkString(", ")
//     //   val body = names.map(name => safe"this.$name = $name").map(i => safe"${indent.add.add}$i;\n").mkString
//     //   safe"class $name {\n${indent.add}constructor($paramList) {\n$body${indent.add}}\n$indent}"
//     // }
//
//   }
// }
