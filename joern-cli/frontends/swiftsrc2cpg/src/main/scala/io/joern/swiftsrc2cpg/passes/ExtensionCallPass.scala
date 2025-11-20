package io.joern.swiftsrc2cpg.passes

import io.joern.x2cpg
import io.shiftleft.codepropertygraph.generated.{Cpg, DispatchTypes, PropertyNames}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language.*

import scala.collection.immutable.HashSet

/** A pass that detects calls which should dispatch to Swift extension methods.
  *
  * Extension methods are represented in the CPG with a `<extension>` marker in their fullName. This pass scans calls
  * that are currently non-static and whose `methodFullName` is known at call-site but not present among declared
  * methods. If inserting `<extension>` before the method name yields a known method full name, the call's dispatch type
  * is changed to `STATIC_DISPATCH` and the call's `methodFullName` is updated accordingly.
  *
  * @param cpg
  *   the code property graph being transformed
  */
class ExtensionCallPass(cpg: Cpg) extends ForkJoinParallelCpgPass[Call](cpg) {

  /** Cache of existing method full names in the CPG. */
  private lazy val methodFullNames = HashSet.from(cpg.method.fullName)

  override def generateParts(): Array[Call] =
    cpg.call
      .methodFullNameNot(x2cpg.Defines.DynamicCallUnknownFullName)
      .dispatchTypeNot(DispatchTypes.STATIC_DISPATCH)
      .toArray

  /** For each call, if the original `methodFullName` is not present among defined methods but the transformed name with
    * `<extension>` inserted is present, update the call to use static dispatch and the extension method fullName.
    */
  override def runOnPart(builder: DiffGraphBuilder, part: Call): Unit = {
    if (!methodFullNames.contains(part.methodFullName)) {
      val methodFullNameWithExtension =
        part.methodFullName.replaceFirst(s"\\.${part.name}:", s"<extension>.${part.name}:")
      if (methodFullNames.contains(methodFullNameWithExtension)) {
        builder.setNodeProperty(part, PropertyNames.DispatchType, DispatchTypes.STATIC_DISPATCH)
        builder.setNodeProperty(part, PropertyNames.MethodFullName, methodFullNameWithExtension)
      }
    }
  }

}
