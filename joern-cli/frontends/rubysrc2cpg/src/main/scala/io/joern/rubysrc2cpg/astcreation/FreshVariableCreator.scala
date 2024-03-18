package io.joern.rubysrc2cpg.astcreation

trait FreshVariableCreator { this: AstCreator =>
  // This is in a single-threaded context.
  private var varCounter: Int = 0

  private def tmpVariableTemplate(id: Int): String = s"<tmp-$id>"
  protected def freshVariableName(template: Int => String = tmpVariableTemplate): String = {
    val name = template(varCounter)
    varCounter += 1
    name
  }
}
