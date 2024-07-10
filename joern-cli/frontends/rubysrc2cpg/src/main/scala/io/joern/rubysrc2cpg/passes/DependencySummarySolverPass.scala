package io.joern.rubysrc2cpg.passes;

import io.joern.rubysrc2cpg.datastructures.RubyProgramSummary
import io.joern.x2cpg.passes.base.{MethodStubCreator, TypeDeclStubCreator}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Dependency
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language.*
import io.joern.x2cpg.Defines
import io.joern.rubysrc2cpg.passes.Defines as RDefines
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, NodeTypes}

class DependencySummarySolverPass(cpg: Cpg, dependencySummary: RubyProgramSummary)
    extends ForkJoinParallelCpgPass[Dependency](cpg) {
  override def generateParts(): Array[Dependency] = cpg.dependency.toArray
  override def runOnPart(diffGraph: DiffGraphBuilder, dependency: Dependency): Unit = {
    dependencySummary.namespaceToType.filter(_._1.startsWith(dependency.name)).flatMap(_._2).foreach { x =>
      val typeDeclName =
        if x.name.endsWith(RDefines.Main) then RDefines.Main
        else x.name.split("[.]").lastOption.getOrElse(Defines.Unknown)

      val dependencyTypeDecl = TypeDeclStubCreator
        .createTypeDeclStub(name = typeDeclName, fullName = x.name, fileName = s"${dependency.name}.rb")

      diffGraph.addNode(dependencyTypeDecl)

      x.methods.foreach { x =>
        MethodStubCreator.createMethodStub(
          name = x.name.split("[:]").lastOption.getOrElse(Defines.Unknown),
          fullName = x.name,
          signature = "",
          dispatchType = DispatchTypes.DYNAMIC_DISPATCH,
          parameterCount = 1,
          dstGraph = diffGraph,
          astParentType = NodeTypes.TYPE_DECL,
          astParentFullName = dependencyTypeDecl.fullName
        )
      }
    }
  }
}
