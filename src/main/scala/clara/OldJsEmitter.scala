// package clara
//
// import clara.util.Safe._
//
// object JsEmitter {
//
//   def emitString(valueExpr: Ast.ValueExpr): String = Impl.walkValueExpr(Impl.Indent())(valueExpr)
//
//   object Impl {
//     import Ast._
//
//     case class Indent(val chars: String = "  ", val level: Int = 0) {
//       def add = Indent(chars, level + 1)
//       val s = chars * level
//     }
//
//     def walkValueExpr(indent: Indent)(valueExpr: ValueExpr): String = valueExpr match {
//       // optimizations
//       case Block(Seq(ve: ValueExpr), _) => walkValueExpr(indent)(ve)
//
//       // normal rules
//       case UnitLiteral(_) => "undefined"
//       case IntegerLiteral(value, _) => safe"$value"
//       case StringLiteral(value, _) => safe""""$value""""
//       case Tuple(es, _) => es.map(walkValueExpr(indent)).mkString("[", ", ", "]")
//       case Block(bcs, _) => {
//         val rows = walkBlockContents(indent.add)(bcs)
//
//         safe"(() => {\n${rows.init.mkString}\n${rows.last}${indent.s}})()"
//       }
//       case NamedValue(name, _) => name
//       case ValueAs(e, _, _) => walkValueExpr(indent)(e)
//       case Lambda(parameter, body, _) => walkLambda(indent)(parameter, body)
//       case MemberSelection(e, member, _, _, _) => safe"${walkValueExpr(indent)(e)}.$member" // FIXME if target is native value type like String, must call function instead
//       case Call(callee, argument, _) => safe"${walkValueExpr(indent)(callee)}(${walkValueExpr(indent)(argument)})" // FIXME if delegated must call .apply() instead
//     }
//
//     def walkBlockContents(indent: Indent)(bcs: Seq[BlockContent]): Seq[String] = {
//       val items = bcs.flatMap {
//         case ValueDef(target, e, _) => walkValueDef(indent)(target, e)
//         // case ClassDef(name, contents) => walkClassDef(indentItems)(name, contents)
//         case e: ValueExpr => Seq(walkValueExpr(indent)(e))
//       }
//
//       val (initValues, lastValue) = bcs.last match {
//         case e: ValueExpr => (items.init, items.last)
//         case _ => (items, "undefined") //FIXME this matches on Comment
//       }
//
//       (initValues :+ safe"return $lastValue").map(item => safe"${indent.s}$item\n")
//     }
//
//     def walkLambdaParameterPattern(parameter: Pattern): String = parameter match {
//       case UnitPattern(_) => "()"
//       case NamePattern(name, _) => name
//       case PatternAs(p, _, _) => walkLambdaParameterPattern(p)
//     }
//
//     def walkLambda(indent: Indent)(parameter: Pattern, body: ValueExpr) = {
//       val p = walkLambdaParameterPattern(parameter)
//       val b = body match {
//         case Block(bcs, _) => {
//           val rows = walkBlockContents(indent.add)(bcs)
//
//           safe"{\n${rows.init.mkString}\n${rows.last}${indent.s}}"
//         }
//         case _ => walkValueExpr(indent)(body)
//       }
//
//       safe"$p => $b"
//     }
//
//     def walkValueDef(indent: Indent)(target: Pattern, e: ValueExpr): Seq[String] = target match {
//       case UnitPattern(_) => Nil
//       case NamePattern(name, _) => Seq(safe"const $name = ${walkValueExpr(indent)(e)}")
//       case PatternAs(p, _, _) => walkValueDef(indent)(p, e)
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
