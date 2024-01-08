package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.astcreation.AstCreator
import io.joern.rubysrc2cpg.astcreation.RubyIntermediateAst.*

trait FreshVariableCreator { this: AstCreator =>
  // This is in a single-threaded context.
  var tmpCounter: Int                      = 0
  private def tmpTemplate(id: Int): String = s"<tmp-${id}>"
  protected def freshName: String = {
    val name = tmpTemplate(tmpCounter)
    tmpCounter += 1
    name
  }
}
