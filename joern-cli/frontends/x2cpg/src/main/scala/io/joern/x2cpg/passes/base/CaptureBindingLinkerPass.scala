package io.joern.x2cpg.passes.base

import io.shiftleft.codepropertygraph.generated.nodes.{Block, ClosureBinding, Declaration, MethodRef}
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language.*

/** Attempts to automatically link closure bindings to their captured locals and parameters if they are not yet defined.
  */
class CaptureBindingLinkerPass(cpg: Cpg) extends ForkJoinParallelCpgPass[MethodRef](cpg) {

  override def generateParts(): Array[MethodRef] =
    cpg.methodRef.where(_.and(_._captureOut, _.not(_._captureOut._refOut))).toArray

  override def runOnPart(builder: DiffGraphBuilder, part: MethodRef): Unit =
    part.captureOut.foreach(runOnClosureBinding(builder, part, _))

  private def runOnClosureBinding(
    builder: DiffGraphBuilder,
    methodRef: MethodRef,
    closureBinding: ClosureBinding
  ): Unit = {
    closureBinding.closureOriginalName.foreach { originalName =>
      methodRef.start
        .repeat(_.parentBlock)(
          _.emit.until(_.or(_.collectAll[Block].local.nameExact(originalName), _.astParent.isMethod))
        )
        .isBlock
        .local
        .nameExact(originalName)
        .headOption match
        case Some(local) => setEdges(builder, local, closureBinding)
        case None => methodRef.method.parameter.nameExact(originalName).foreach(setEdges(builder, _, closureBinding))
    }
  }

  private def setEdges(builder: DiffGraphBuilder, decl: Declaration, closureBinding: ClosureBinding): Unit = {
    if (!closureBinding.refOut.contains(decl)) builder.addEdge(closureBinding, decl, EdgeTypes.REF)
    if (!closureBinding._capturedByIn.contains(decl)) builder.addEdge(decl, closureBinding, EdgeTypes.CAPTURED_BY)
  }

}
