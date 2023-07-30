package io.joern.jssrc2cpg.passes

import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

object Defines {
  val Any: String               = "ANY"
  val Number: String            = "__ecma.Number"
  val String: String            = "__ecma.String"
  val Boolean: String           = "__ecma.Boolean"
  val Null: String              = "__ecma.Null"
  val Math: String              = "__ecma.Math"
  val Symbol: String            = "__ecma.Symbol"
  val Console: String           = "__whatwg.console"
  val Object: String            = "object"
  val BigInt: String            = "bigint"
  val Unknown: String           = "unknown"
  val Void: String              = "void"
  val Never: String             = "never"
  val Undefined: String         = "undefined"
  val NodeModulesFolder: String = "node_modules"
  val GlobalNamespace: String   = NamespaceTraversal.globalNamespaceName

  val JsTypes: List[String] =
    List(Any, Number, String, Boolean, Null, Math, Symbol, Console, Object, BigInt, Unknown, Never, Void, Undefined)
}
