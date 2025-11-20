package io.joern.swiftsrc2cpg.passes

import io.joern.x2cpg
import io.shiftleft.codepropertygraph.generated.{Cpg, DispatchTypes, PropertyNames}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language.*

import scala.collection.immutable.HashSet

class ExtensionCallPass(cpg: Cpg) extends ForkJoinParallelCpgPass[Call](cpg) {

  private lazy val methodFullNames = HashSet.from(cpg.method.fullName)

  override def generateParts(): Array[Call] =
    cpg.call
      .methodFullNameNot(x2cpg.Defines.DynamicCallUnknownFullName)
      .dispatchTypeNot(DispatchTypes.STATIC_DISPATCH)
      .toArray

  override def runOnPart(builder: DiffGraphBuilder, part: Call): Unit = {
    if (!methodFullNames.contains(part.methodFullName)) {
      val methodFullNameWithExtension = part.methodFullName.replace(s".${part.name}:", s"<extension>.${part.name}:")
      if (methodFullNames.contains(methodFullNameWithExtension)) {
        builder.setNodeProperty(part, PropertyNames.DispatchType, DispatchTypes.STATIC_DISPATCH)
      }
    }

  }

}
