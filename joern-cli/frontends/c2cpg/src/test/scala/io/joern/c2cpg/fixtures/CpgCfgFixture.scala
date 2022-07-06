package io.joern.c2cpg.fixtures

import better.files.File
import io.joern.c2cpg.Config
import io.joern.c2cpg.passes.AstCreationPass
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.CfgNode
import io.shiftleft.semanticcpg.language._
import io.joern.x2cpg.passes.controlflow.CfgCreationPass
import io.joern.x2cpg.passes.controlflow.cfgcreation.Cfg.CfgEdgeType

import scala.jdk.CollectionConverters._

class CpgCfgFixture(code: String, fileExtension: String = ".c") {

  private val cpg: Cpg = Cpg.emptyCpg

  File.usingTemporaryDirectory("c2cpgtest") { dir =>
    val file = dir / s"file1$fileExtension"
    file.write(s"RET func() { $code }")
    val config = Config(inputPath = dir.path.toString, includePathsAutoDiscovery = false)
    new AstCreationPass(cpg, AstCreationPass.SourceFiles, config)
      .createAndApply()
    new CfgCreationPass(cpg).createAndApply()
  }

  val codeToNode: Map[String, CfgNode] =
    cpg.method.ast.isCfgNode.l.map { node =>
      node.code -> node
    }.toMap

  def expected(pairs: (String, CfgEdgeType)*): Set[String] = {
    pairs.map { case (code, _) =>
      codeToNode(code).code
    }.toSet
  }

  def succOf(code: String): Set[String] = {
    codeToNode(code)._cfgOut.asScala
      .map(_.asInstanceOf[CfgNode])
      .toSet
      .map[String](_.code)
  }

}
