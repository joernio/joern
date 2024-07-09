package io.joern.php2cpg.passes

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{ClosureBinding, Method, MethodRef}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.AstNode
import io.shiftleft.codepropertygraph.generated.nodes.Local

class ClosureRefPass(cpg: Cpg) extends ForkJoinParallelCpgPass[ClosureBinding](cpg) {
  private val logger = LoggerFactory.getLogger(this.getClass)

  override def generateParts(): Array[ClosureBinding] = cpg.all.collectAll[ClosureBinding].toArray

  /** The AstCreator adds closureBindingIds and ClosureBindings for captured locals, but does not add the required REF
    * edges from the ClosureBinding to the captured node since the captured node may be a Local that is created by the
    * LocalCreationPass and does not exist during AST creation.
    *
    * This pass attempts to find the captured node in the method containing the MethodRef to the closure method, since
    * that is the scope in which the closure would have originally been created.
    */
  override def runOnPart(diffGraph: DiffGraphBuilder, closureBinding: ClosureBinding): Unit = {
    closureBinding._methodRefViaCaptureIn.toList match {
      case Nil =>
        logger.error(s"No MethodRef corresponding to closureBinding ${closureBinding.closureBindingId}")

      case methodRef :: Nil =>
        addRefToCapturedNode(diffGraph, closureBinding, getMethod(methodRef))

      case methodRefs =>
        logger.error(s"Mutliple MethodRefs corresponding to closureBinding ${closureBinding.closureBindingId}")
        logger.debug(s"${closureBinding.closureBindingId} MethodRefs = ${methodRefs}")
    }
  }

  private def getMethod(methodRef: MethodRef): Option[Method] = {
    methodRef.start.repeat(_.astParent)(_.until(_.isMethod).emit(_.isMethod)).isMethod.headOption
  }

  private def addRefToCapturedNode(
    diffGraph: DiffGraphBuilder,
    closureBinding: ClosureBinding,
    method: Option[Method]
  ): Unit = {
    method match {
      case None =>
        logger.warn(s"No parent method for methodRef for ${closureBinding.closureBindingId}. REF edge will be missing")

      case Some(method) =>
        closureBinding.closureOriginalName.foreach { name =>
          lazy val locals =
            method.start.repeat(_.astChildren.filterNot(_.isMethod))(_.emit(_.isLocal)).collectAll[Local]
          val maybeCaptured =
            method.parameter
              .find(_.name == name)
              .orElse(locals.find(_.name == name))

          maybeCaptured.foreach { captured =>
            diffGraph.addEdge(closureBinding, captured, EdgeTypes.REF)
          }
        }
    }
  }
}
