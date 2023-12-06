package io.joern.jssrc2cpg.passes

import better.files.File
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.NewMetaData
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.passes.CpgPass

class JavaScriptMetaDataPass(cpg: Cpg, hash: String, inputPath: String) extends CpgPass(cpg) {

  override def run(diffGraph: DiffGraphBuilder): Unit = {
    val absolutePathToRoot = File(inputPath).path.toAbsolutePath.toString
    val metaNode           = NewMetaData().language(Languages.JSSRC).root(absolutePathToRoot).hash(hash).version("0.1")
    diffGraph.addNode(metaNode)
  }

}
