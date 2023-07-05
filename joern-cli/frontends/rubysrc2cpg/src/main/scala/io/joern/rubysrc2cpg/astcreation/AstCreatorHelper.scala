package io.joern.rubysrc2cpg.astcreation

trait AstCreatorHelper {

  import GlobalTypes._

  def isBuiltin(x: String): Boolean = builtinFunctions.contains(x)

  def prefixAsBuiltin(x: String): String = s"$builtinPrefix:$x"

  def isExcluded(x: String): Boolean = excludeMethodName.contains(x)

}

object GlobalTypes {
  val builtinPrefix = "__builtin"
  // https://ruby-doc.org/docs/ruby-doc-bundle/Manual/man-1.4/function.html
  // TODO: Complete this list
  val builtinFunctions = Set("puts", "chomp", "exec")

  val excludeMethodName = Set("new")
}
