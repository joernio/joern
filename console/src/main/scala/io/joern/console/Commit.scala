package io.joern.console

import io.shiftleft.passes.{CpgPass, DiffGraph}
import io.shiftleft.semanticcpg.layers.{LayerCreator, LayerCreatorContext, LayerCreatorOptions}

import scala.concurrent.ExecutionContext

object Commit {
  val overlayName: String = "commit"
  val description: String = "Apply current custom diffgraph"
  def defaultOpts = new CommitOptions(DiffGraph.newBuilder)
}

class CommitOptions(var diffGraphBuilder: DiffGraph.Builder) extends LayerCreatorOptions

class Commit(opts: CommitOptions) extends LayerCreator {

  override val overlayName: String = Commit.overlayName
  override val description: String = Commit.description

  override def createWithExecutionContext(context: LayerCreatorContext, storeUndoInfo: Boolean)(
      implicit ec: ExecutionContext): Unit = {
    val pass: CpgPass = new CpgPass(context.cpg) {
      override val name = "commit"
      override def run(): Iterator[DiffGraph] = Iterator(opts.diffGraphBuilder.build())
    }
    runPass(pass, context, storeUndoInfo)
    opts.diffGraphBuilder = DiffGraph.newBuilder
  }

}
