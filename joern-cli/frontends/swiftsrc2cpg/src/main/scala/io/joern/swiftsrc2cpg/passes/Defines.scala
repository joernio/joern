package io.joern.swiftsrc2cpg.passes

import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

object Defines {
  val Any: String             = "ANY"
  val Character: String       = "Character"
  val String: String          = "String"
  val Int: String             = "Int"
  val Float: String           = "Float"
  val Double: String          = "Double"
  val Bool: String            = "Bool"
  val Nil: String             = "Nil"
  val GlobalNamespace: String = NamespaceTraversal.globalNamespaceName

  val SwiftTypes: List[String] =
    List(Any, Nil, Character, String, Int, Float, Double, Bool)
}
