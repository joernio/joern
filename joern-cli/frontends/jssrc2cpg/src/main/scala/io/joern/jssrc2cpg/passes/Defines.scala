package io.joern.jssrc2cpg.passes

import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

object Defines {
  val Any: String               = "ANY"
  val Array: String             = "__ecma.Array"
  val Number: String            = "__ecma.Number"
  val String: String            = "__ecma.String"
  val Boolean: String           = "__ecma.Boolean"
  val Null: String              = "__ecma.Null"
  val Math: String              = "__ecma.Math"
  val Symbol: String            = "__ecma.Symbol"
  val Console: String           = "__whatwg.console"
  val Object: String            = "__ecma.Object"
  val BigInt: String            = "__ecma.BigInt"
  val Unknown: String           = Any
  val Void: String              = Any
  val Never: String             = Any
  val Undefined: String         = Any
  val NodeModulesFolder: String = "node_modules"
  val GlobalNamespace: String   = NamespaceTraversal.globalNamespaceName

  val JsTypes: List[String] =
    List(
      Any,
      Array,
      Number,
      String,
      Boolean,
      Null,
      Math,
      Symbol,
      Console,
      Object,
      BigInt,
      Unknown,
      Never,
      Void,
      Undefined
    )

  def isBuiltinType(tpe: String): Boolean = JsTypes.contains(tpe.stripSuffix("[]"))
}
