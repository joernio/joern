package io.joern.swiftsrc2cpg.passes

import io.joern.x2cpg.passes.frontend.XInheritanceFullNamePass
import io.shiftleft.codepropertygraph.Cpg

/** Using some basic heuristics, will try to resolve type full names from types found within the CPG. Requires
  * ImportPass as a pre-requisite.
  */
class SwiftInheritanceNamePass(cpg: Cpg) extends XInheritanceFullNamePass(cpg) {

  override val pathSep: Char      = ':'
  override val moduleName: String = "<global>"
  override val fileExt: String    = ".swift"

}
