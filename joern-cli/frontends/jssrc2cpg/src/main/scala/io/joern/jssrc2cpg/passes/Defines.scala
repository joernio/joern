package io.joern.jssrc2cpg.passes

import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

object Defines {
  val ANY: String                 = "ANY"
  val NUMBER: String              = "__ecma.Number"
  val STRING: String              = "__ecma.String"
  val BOOLEAN: String             = "__ecma.Boolean"
  val NULL: String                = "__ecma.Null"
  val MATH: String                = "__ecma.Math"
  val SYMBOL: String              = "__ecma.Symbol"
  val CONSOLE: String             = "__whatwg.console"
  val OBJECT: String              = "object"
  val NODE_MODULES_FOLDER: String = "node_modules"
  val GLOBAL_NAMESPACE: String    = NamespaceTraversal.globalNamespaceName

  val JSTYPES: List[String] = List(ANY, NUMBER, STRING, BOOLEAN, NULL, MATH, SYMBOL, CONSOLE, OBJECT)
}
