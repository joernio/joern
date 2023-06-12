package io.joern.rubysrc2cpg.passes

import io.joern.rubysrc2cpg.utils.PackageTable
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.codepropertygraph.generated.{Cpg, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.Call.PropertyNames
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language._

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.CollectionHasAsScala

class PackageResolverPass(cpg: Cpg, packageTable: PackageTable) extends ForkJoinParallelCpgPass[Call](cpg) {
  override def generateParts(): Array[Call] = getUnresolvedCallNode(cpg)

  override def runOnPart(builder: DiffGraphBuilder, call: Call): Unit = {
    val sourceFile = call.file.head.name
    if (packageTable.checkIfInternalDependency(sourceFile, call.name)) {
      packageTable
        .getPackageMethod(sourceFile, call.name)
        .foreach(method => {
          updateCallNode(builder, call, s"${method.parentClassPath}${call.name}:<unresolvedSignature>")
        })
    }

    packageTable
      .getPackageCallInFile(sourceFile)
      .foreach(module => {
        packageTable
          .getPackageMethod(module, call.name)
          .foreach(method => {
            val prefixName = if (!Files.isRegularFile(Paths.get(module))) s"${module}." else ""
            updateCallNode(builder, call, s"${prefixName}${method.parentClassPath}${call.name}:<unresolvedSignature>")
          })
      })
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
