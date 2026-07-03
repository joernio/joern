package io.joern.x2cpg.frontendspecific.jssrc2cpg

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
  val Program: String           = ":program"
  val GlobalNamespace: String   = NamespaceTraversal.globalNamespaceName
  val OperatorsNew: String      = "<operator>.new" // TODO: place "<operator>.new" into the schema

  // EJS <%= %> escaped output, modeled as a call for sink detection.
  // Same as the actual EJS compiler output:
  // https://github.com/mde/ejs/blob/2c41365ca042a70978e5795adc05fbb8b35fb013/lib/esm/ejs.js#L878
  val EscapedOutputName: String = "escapeFn"

  // EJS <%- %> unescaped (raw) output, modeled as a call for sink detection.
  // Same as the actual EJS compiler output:
  // https://github.com/mde/ejs/blob/2c41365ca042a70978e5795adc05fbb8b35fb013/lib/esm/ejs.js#L586
  val UnescapedOutputName: String = "__append"

  // first two bytes of an EJS output tag in the original source
  val EjsOutputTagPrefix: String = "<%"

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
