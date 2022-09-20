package io.joern.console

import io.shiftleft.passes.CpgPassBase
import io.shiftleft.passes.SimpleCpgPass
import io.shiftleft.semanticcpg.layers.{LayerCreator, LayerCreatorContext, LayerCreatorOptions}
import overflowdb.BatchedUpdate.DiffGraphBuilder

object Commit {
  val overlayName: String = "commit"
  val description: String = "Apply current custom diffgraph"
  def defaultOpts         = new CommitOptions(new DiffGraphBuilder)
}

class CommitOptions(var diffGraphBuilder: DiffGraphBuilder) extends LayerCreatorOptions

class Commit(opts: CommitOptions) extends LayerCreator {

  override val overlayName: String       = Commit.overlayName
  override val description: String       = Commit.description
  override val storeOverlayName: Boolean = false

  override def create(context: LayerCreatorContext, storeUndoInfo: Boolean): Unit = {
    val pass: CpgPassBase = new SimpleCpgPass(context.cpg) {
      override val name = "commit"
      override def run(diffGraphBuilder: DiffGraphBuilder): Unit = {
        diffGraphBuilder.absorb(opts.diffGraphBuilder)
      }
    }
    runPass(pass, context, storeUndoInfo)
    opts.diffGraphBuilder = new DiffGraphBuilder
  }

}
