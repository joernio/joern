package io.joern.x2cpg.passes.base

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.SimpleCpgPass

class VersionControlPass(cpg: Cpg, inputDir: Option[String] = None) extends SimpleCpgPass(cpg) {

  override def run(dstGraph: DiffGraphBuilder): Unit = {
    inputDir match {
      case Some(projectRoot) => findVersionControlSystem(projectRoot, dstGraph)
      case None              => // no input path provided, cannot look for VCS
    }
  }

  def findVersionControlSystem(path: String, dstGraph: DiffGraphBuilder): Unit = {}

}
