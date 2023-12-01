package io.joern.console

import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.layers.{LayerCreator, LayerCreatorContext, LayerCreatorOptions}
import flatgraph.DiffGraphBuilder
import io.shiftleft.codepropertygraph.generated.Cpg

object Commit {
  val overlayName: String = "commit"
  val description: String = "Apply current custom diffgraph"
  def defaultOpts         = new CommitOptions(Cpg.newDiffGraphBuilder)
}

class CommitOptions(var diffGraphBuilder: DiffGraphBuilder) extends LayerCreatorOptions

class Commit(opts: CommitOptions) extends LayerCreator {

  override val overlayName: String       = Commit.overlayName
  override val description: String       = Commit.description
  override val storeOverlayName: Boolean = false

  override def create(context: LayerCreatorContext, storeUndoInfo: Boolean): Unit = {
    val pass: CpgPass = new CpgPass(context.cpg) {
      override val name = "commit"
      override def run(builder: DiffGraphBuilder): Unit = {
        builder.absorb(opts.diffGraphBuilder)
      }
    }
    runPass(pass, context, storeUndoInfo)
    opts.diffGraphBuilder = Cpg.newDiffGraphBuilder
  }

}
