package io.joern.c2cpg.fixtures

import better.files.File
import io.joern.c2cpg.C2Cpg.Config
import io.joern.c2cpg.passes.AstCreationPass
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.CfgNode
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.passes.controlflow.CfgCreationPass
import io.shiftleft.semanticcpg.passes.controlflow.cfgcreation.Cfg.CfgEdgeType

import scala.concurrent.ExecutionContext
import scala.jdk.CollectionConverters._

class CpgCfgFixture(code: String, fileExtension: String = ".c") {
  implicit val ec: ExecutionContext = ExecutionContext.global

  private val cpg: Cpg = Cpg.emptyCpg

  File.usingTemporaryDirectory("c2cpgtest") { dir =>
    val file = dir / s"file1$fileExtension"
    file.write(s"RET func() { $code }")
    new AstCreationPass(cpg, AstCreationPass.SourceFiles, None, Config(inputPaths = Set(dir.path.toString)))
      .createAndApply()
    new CfgCreationPass(cpg).createAndApply()
  }

  val codeToNode: Map[String, CfgNode] =
    cpg.method.ast.isCfgNode.l.map { node =>
      node.code -> node
    }.toMap

  def expected(pairs: (String, CfgEdgeType)*): Set[String] = {
    pairs.map {
      case (code, _) => codeToNode(code).code
    }.toSet
  }

  def succOf(code: String): Set[String] = {
    codeToNode(code)._cfgOut.asScala
      .map(_.asInstanceOf[CfgNode])
      .toSet
      .map[String](_.code)
  }

}
