package io.joern.c2cpg.testfixtures

import io.joern.x2cpg.passes.controlflow.cfgcreation.Cfg.CfgEdgeType
import io.joern.x2cpg.passes.controlflow.CfgCreationPass
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.CfgNode
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal._

trait AbstractCfgPassTest extends AbstractPassTest {
  protected object CfgFixture {
    def apply(code: String, fileExtension: String = ".c")(f: Cpg => Unit): Unit = {
      AstFixture(s"RET func() { $code }", s"file1$fileExtension") { cpg =>
        new CfgCreationPass(cpg).createAndApply()
        f(cpg)
      }
    }
  }

  // index is zero based and describes which node to take if multiple node match the code string.
  def succOf(code: String, index: Int = 0)(implicit cpg: Cpg): Set[String] =
    cpg.method.ast.isCfgNode.toVector
      .collect {
        case node if node.code == code => node
      }
      .lift(index)
      .getOrElse(fail(s"No node found for code = '$code' and index '$index'!"))
      ._cfgOut
      .cast[CfgNode]
      .code
      .toSetImmutable

  def expected(pairs: (String, CfgEdgeType)*)(implicit cpg: Cpg): Set[String] =
    pairs.map { case (code, _) =>
      cpg.method.ast.isCfgNode.toVector.collect {
        case node if node.code == code => node.code
      }.head
    }.toSet

}
