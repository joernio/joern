package io.joern.x2cpg.layers

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.passes.CpgPassBase
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.layers.{LayerCreator, LayerCreatorContext, LayerCreatorOptions}
import io.joern.x2cpg.passes.controlflow.CfgCreationPass
import io.joern.x2cpg.passes.controlflow.cfgdominator.CfgDominatorPass
import io.joern.x2cpg.passes.controlflow.codepencegraph.CdgPass

object ControlFlow {
  val overlayName: String = "controlflow"
  val description: String = "Control flow layer (including dominators and CDG edges)"
  def defaultOpts         = new LayerCreatorOptions()

  def passes(cpg: Cpg): Iterator[CpgPassBase] = {
    val cfgCreationPass = cpg.metaData.language.lastOption match {
      case Some(Languages.GHIDRA) => Iterator[CpgPassBase]()
      case Some(Languages.LLVM)   => Iterator[CpgPassBase]()
      case _                      => Iterator[CpgPassBase](new CfgCreationPass(cpg))
    }
    cfgCreationPass ++ Iterator(new CfgDominatorPass(cpg), new CdgPass(cpg))
  }

}

class ControlFlow extends LayerCreator {
  override val overlayName: String     = ControlFlow.overlayName
  override val description: String     = ControlFlow.description
  override val dependsOn: List[String] = List(Base.overlayName)

  override def create(context: LayerCreatorContext): Unit = {
    val cpg = context.cpg
    ControlFlow.passes(cpg).zipWithIndex.foreach { case (pass, index) =>
      runPass(pass, context, index)
    }
  }

  // LayerCreators need one-arg constructor, because they're called by reflection from io.joern.console.Run
  def this(optionsUnused: LayerCreatorOptions) = this()
}
