package io.joern.swiftsrc2cpg.passes

import io.joern.swiftsrc2cpg.astcreation.AstCreatorHelper
import io.shiftleft.codepropertygraph.generated.{Cpg, PropertyDefaults, PropertyNames}
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language.*

/** A CPG pass that restores Objective-C constructor call node properties.
  *
  * This pass looks for static `init` calls whose `methodFullName` indicates they originate from Objective-C interop
  * (e.g. prefixed with `cobjc`, `(cs)`, or `(cswift)`). For such calls, it resolves the matching constructor method on
  * the target `typeDecl` and updates the call node properties so they reflect the resolved constructor.
  *
  * @param cpg
  *   the code property graph to operate on
  */
class ObjcCallFullNamePass(cpg: Cpg) extends CpgPass(cpg) {

  override def run(diffGraph: DiffGraphBuilder): Unit = {
    for {
      call <- cpg.call.isStatic.nameExact("init").methodFullName(".+\\.init:\\(.*\\)->.+")
      if call.typeFullName != PropertyDefaults.TypeFullName &&
        call.signature != PropertyDefaults.Signature &&
        AstCreatorHelper.isObjcCall(call.methodFullName)
      constructorMethod <- cpg.typeDecl
        .fullNameExact(call.typeFullName)
        .method
        .isConstructor
        .filter(_.fullName.startsWith(s"${call.typeFullName}.init:"))
        .signatureExact(call.signature)
      if call.methodFullName != constructorMethod.fullName
    } {
      diffGraph.setNodeProperty(call, PropertyNames.MethodFullName, constructorMethod.fullName)
      diffGraph.setNodeProperty(call, PropertyNames.Signature, constructorMethod.signature)
    }
  }

}
