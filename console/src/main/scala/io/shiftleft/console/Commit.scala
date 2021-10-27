package io.shiftleft.console

import io.shiftleft.passes.{CpgPass, DiffGraph}
import io.shiftleft.semanticcpg.layers.{LayerCreator, LayerCreatorContext, LayerCreatorOptions}

object Commit {
  val overlayName: String = "commit"
  val description: String = "Apply current custom diffgraph"
  def defaultOpts = new CommitOptions(DiffGraph.newBuilder)
}

class CommitOptions(var diffGraphBuilder: DiffGraph.Builder) extends LayerCreatorOptions

class Commit(opts: CommitOptions) extends LayerCreator {

  override val overlayName: String = Commit.overlayName
  override val description: String = Commit.description

  override def create(context: LayerCreatorContext, storeUndoInfo: Boolean): Unit = {
    val pass: CpgPass = new CpgPass(context.cpg) {
      override val name = "commit"
      override def run(): Iterator[DiffGraph] = Iterator(opts.diffGraphBuilder.build())
    }
    runPass(pass, context, storeUndoInfo)
    opts.diffGraphBuilder = DiffGraph.newBuilder
  }

}
