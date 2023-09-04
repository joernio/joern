package io.joern.gosrc2cpg.passes

import io.joern.gosrc2cpg.astcreation.Defines
import io.joern.gosrc2cpg.datastructures.GoGlobal
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.PropertyNames
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language.*
import overflowdb.BatchedUpdate

class CallTypeAndSigntureLinkerPass(cpg: Cpg) extends CpgPass(cpg) {
  override def run(builder: BatchedUpdate.DiffGraphBuilder): Unit =
    cpg.call
      .typeFullNameExact(Defines.tobeFilled)
      .foreach(call => {
        val (returnTypeFullName, signature) =
          GoGlobal.methodFullNameReturnTypeMap
            .getOrDefault(call.methodFullName, (Defines.anyTypeName, Option.empty[String]))
        builder.setNodeProperty(call, PropertyNames.TYPE_FULL_NAME, returnTypeFullName)
        signature match
          case Some(sig) =>
            builder.setNodeProperty(call, PropertyNames.SIGNATURE, sig)
          case _ =>
      })
}
