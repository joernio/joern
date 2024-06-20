package io.joern.x2cpg.frontendspecific.swiftsrc2cpg

import io.joern.x2cpg.passes.frontend.XInheritanceFullNamePass
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

/** Using some basic heuristics, will try to resolve type full names from types found within the CPG. Requires
  * ImportPass as a pre-requisite.
  */
class SwiftInheritanceNamePass(cpg: Cpg) extends XInheritanceFullNamePass(cpg) {

  override val pathSep: Char      = ':'
  override val moduleName: String = NamespaceTraversal.globalNamespaceName
  override val fileExt: String    = ".swift"

}
