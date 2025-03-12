package io.joern.jssrc2cpg.passes

import io.shiftleft.semanticcpg.utils.FileUtil.PathExt
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.NewMetaData
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.passes.CpgPass

import java.nio.file.Paths

class JavaScriptMetaDataPass(cpg: Cpg, hash: String, inputPath: String) extends CpgPass(cpg) {

  override def run(diffGraph: DiffGraphBuilder): Unit = {
    val absolutePathToRoot = Paths.get(inputPath).absolutePathAsString
    val metaNode           = NewMetaData().language(Languages.JSSRC).root(absolutePathToRoot).hash(hash).version("0.1")
    diffGraph.addNode(metaNode)
  }

}
