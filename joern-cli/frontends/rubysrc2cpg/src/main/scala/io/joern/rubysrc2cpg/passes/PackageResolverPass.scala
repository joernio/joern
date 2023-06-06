package io.joern.rubysrc2cpg.passes

import io.joern.rubysrc2cpg.utils.PackageTable
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.codepropertygraph.generated.{Cpg, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.Call.PropertyNames
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language._

import scala.jdk.CollectionConverters.CollectionHasAsScala

class PackageResolverPass(cpg: Cpg, packageTable: PackageTable) extends ForkJoinParallelCpgPass[Call](cpg) {
  override def generateParts(): Array[Call] = getUnresolvedCallNode(cpg)

  override def runOnPart(builder: DiffGraphBuilder, call: Call): Unit = {
    if (packageTable.contains(call.name)) {
      updateCallNode(
        builder,
        call,
        s"${packageTable.get(call.name).head.parentClassPath}${call.name}" + ":<unresolvedSignature>"
      )
    }
  }

  private def getUnresolvedCallNode(cpg: Cpg): Array[Call] = {
    cpg.call
      .whereNot(_.name(Operators.ALL.asScala.toSeq: _*))
      .where(_.methodFullName(".*<unknownfullname>.*"))
      .whereNot(_.name(".*(require|load).*"))
      .toArray
  }

  private def updateCallNode(builder: DiffGraphBuilder, callNode: Call, methodName: String): Unit = {
    builder.setNodeProperty(callNode, PropertyNames.MethodFullName, methodName)
  }
}
