package io.joern.swiftsrc2cpg.passes

import io.joern.x2cpg
import io.shiftleft.codepropertygraph.generated.{Cpg, DispatchTypes, PropertyNames}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language.*

/** Pass that resolves extension-method-style calls and rewrites them to use static dispatch to the corresponding
  * extension method fullName.
  *
  * The pass scans call nodes in the provided CPG and for each call methodFullName present in `extensionFullNameMapping`
  * it:
  *   - updates the call's dispatch type to STATIC_DISPATCH
  *   - replaces the call's methodFullName with the mapped extension method full name
  *
  * @param cpg
  *   the code property graph to operate on
  * @param extensionFullNameMapping
  *   mapping from compiler generated methodFullName -> CPG extension methodFullName (for uniqueness)
  */
class ExtensionCallPass(cpg: Cpg, extensionFullNameMapping: Map[String, String])
    extends ForkJoinParallelCpgPass[Call](cpg) {

  /** Selects calls that are not already static dispatch and the methodFullName is not unknown. These are candidates for
    * being rewritten to extension calls.
    */
  override def generateParts(): Array[Call] =
    cpg.call
      .methodFullNameNot(x2cpg.Defines.DynamicCallUnknownFullName)
      .dispatchTypeNot(DispatchTypes.STATIC_DISPATCH)
      .toArray

  /** For a given call (part), if a mapping exists for its methodFullName, set the call to static dispatch and update
    * the methodFullName to the extension's fullName.
    *
    * @param builder
    *   DiffGraphBuilder used to apply modifications to the graph
    * @param part
    *   the call node being processed
    */
  override def runOnPart(builder: DiffGraphBuilder, part: Call): Unit = {
    extensionFullNameMapping.get(part.methodFullName).foreach { methodFullNameExt =>
      builder.setNodeProperty(part, PropertyNames.DispatchType, DispatchTypes.STATIC_DISPATCH)
      builder.setNodeProperty(part, PropertyNames.MethodFullName, methodFullNameExt)
    }
  }

}
