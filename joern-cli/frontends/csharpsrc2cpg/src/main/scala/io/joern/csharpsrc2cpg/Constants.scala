package io.joern.csharpsrc2cpg

object Constants {
  val This: String                   = "this"
  val Global: String                 = "global"
  val TopLevelMainMethodName: String = "<Main>$"
}

object CSharpOperators {
  val throws: String  = "<operators>.throw"
  val unknown: String = "<operators>.unknown"
  val await: String   = "<operator>.await"
}

object CSharpModifiers {
  final val CONST: String = "const"
}

object CSharpDefines {
  final val AnonymousTypePrefix = "<anon>"
}
