package io.joern.jssrc2cpg.passes

import io.joern.x2cpg.passes.frontend.XInheritanceFullNamePass
import io.shiftleft.codepropertygraph.generated.Cpg

/** Using some basic heuristics, will try to resolve type full names from types found within the CPG. Requires
  * ImportPass as a pre-requisite.
  */
class JavaScriptInheritanceNamePass(cpg: Cpg) extends XInheritanceFullNamePass(cpg) {

  override val pathSep: Char      = ':'
  override val moduleName: String = Defines.Program
  override val fileExt: String    = ".js"

}
