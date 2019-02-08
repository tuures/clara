// package clara
//
// object Prelude {
//   import Ast._
//
//   val Prelude: Seq[BlockContent] = Seq(
//     ClassDef("Unit", Nil, None, Nil),
//     ClassDef("Int", Nil, None, Seq(
//       // FIXME need support for mutually recursive defs
//       // ValueDecl("toString", NamedType("String", Nil))
//     )),
//     // TODO maybe String should be just an alias for List[Char] where Char is single unicode codepoint
//     ClassDef("String", Nil, None, Seq(
//       ValueDecl("length", NamedType("Int", Nil))
//     )),
//     ClassDef("Function", Seq(TypeParam(Contravariant, "P", 0), TypeParam(Covariant, "R", 0)), None, Seq(
//       ValueDecl("apply", FuncType(NamedType("P", Nil), NamedType("R", Nil)))
//     ))
//   )
//
//   def prependTo(ve: ValueExpr) = Block(Prelude :+ ve)
// }
//
// // object Stdlib {
// //
// //   val baseEnv = {
// //     import collection.immutable.HashMap
// //     import Analyzer._
// //
// //     val intType = new NormalTypeCon(NormalTypeConDesc(explicitParent=None))
// //     Env.empty.copy(types = HashMap(
// //       "()" -> new NormalTypeCon(NormalTypeConDesc(explicitParent=None)),
// //       "Int" -> intType,
// //       "String" -> new NormalTypeCon(NormalTypeConDesc(explicitParent=None).
// //         addMember("length", PlainValueMember(intType.inst(Nil, Env.empty).getOrElse(???))).right.get
// //       ),
// //       // "Tuple" -> new NormalTypeCon(NormalTypeConDesc().
// //       //   addMember
// //       // ),
// //       "Function" -> new NormalTypeCon(NormalTypeConDesc(explicitParent=None).
// //         addTypeParam("P", TypeParam(0, Ast.Contravariant)).right.get.
// //         addTypeParam("R", TypeParam(0, Ast.Covariant)).right.get.
// //         addMember("apply", Method(TypeParamRef("P"), TypeParamRef("R"))).right.get
// //       )
// //     ))
// //   }
// // }
