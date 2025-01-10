package io.joern.javasrc2cpg.passes;

import io.joern.javasrc2cpg.util.NameConstants
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{Method, TypeDecl}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language.*

class OuterClassRefPass(cpg: Cpg) extends ForkJoinParallelCpgPass[TypeDecl](cpg) {
  override def generateParts(): Array[TypeDecl] = cpg.typeDecl.toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, typeDecl: TypeDecl): Unit = {
    typeDecl.method.nameExact(Defines.ConstructorMethodName).foreach { constructor =>
      constructor.ast.isIdentifier.nameExact(NameConstants.OuterClass).filter(_.refsTo.isEmpty).foreach {
        outerClassIdentifier =>
          constructor.parameter.nameExact(NameConstants.OuterClass).foreach { outerClassParam =>
            diffGraph.addEdge(outerClassIdentifier, outerClassParam, EdgeTypes.REF)
          }
      }
    }
  }
}
