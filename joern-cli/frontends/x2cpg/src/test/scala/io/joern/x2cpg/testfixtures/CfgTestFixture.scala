package io.joern.x2cpg.testfixtures

import io.joern.x2cpg.passes.controlflow.CfgCreationPass
import io.joern.x2cpg.passes.controlflow.cfgcreation.Cfg.CfgEdgeType
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{CfgNode, Method}
import io.shiftleft.semanticcpg.language.*

abstract class CfgTestCpg extends TestCpg {
  override protected def applyPasses(): Unit = {
    new CfgCreationPass(this).createAndApply()
  }
}

class CfgTestFixture[T <: CfgTestCpg](testCpgFactory: () => T) extends Code2CpgFixture(testCpgFactory) {

  private def matchCode(node: CfgNode, code: String): Boolean = node match {
    case method: Method => method.name == code
    case other          => other.code == code
  }

  // index is zero based and describes which node to take if multiple node match the code string.
  case class ExpectationInfo(code: String, index: Int, cfgEdgeKind: CfgEdgeType)

  implicit def toExpectationInfoShort(pair: (String, CfgEdgeType)): ExpectationInfo = {
    ExpectationInfo(pair._1, 0, pair._2)
  }

  implicit def toExpectationInfoFull(pair: (String, Int, CfgEdgeType)): ExpectationInfo = {
    ExpectationInfo(pair._1, pair._2, pair._3)
  }

  def expected(pairs: ExpectationInfo*)(implicit cpg: Cpg): Set[String] = {
    pairs.map { case ExpectationInfo(code, index, _) =>
      cpg.method.ast.isCfgNode.toVector
        .collect {
          case node if matchCode(node, code) => node.code
        }
        .lift(index)
        .getOrElse(fail(s"No node found for code = '$code' and index '$index'!"))
    }.toSet
  }

  // index is zero based and describes which node to take if multiple node match the code string.
  def succOf(code: String, index: Int = 0)(implicit cpg: Cpg): Set[String] = {
    cpg.method.ast.isCfgNode.toVector
      .collect {
        case node if matchCode(node, code) => node
      }
      .lift(index)
      .getOrElse(fail(s"No node found for code = '$code' and index '$index'!"))
      ._cfgOut
      .cast[CfgNode]
      .code
      .toSetImmutable
  }

  def succOf(code: String, nodeType: String)(implicit cpg: Cpg): Set[String] = {
    cpg.method.ast.isCfgNode
      .label(nodeType)
      .toVector
      .collectFirst {
        case node if matchCode(node, code) => node
      }
      .getOrElse(fail(s"No node found for code = '$code' and type '$nodeType'!"))
      ._cfgOut
      .cast[CfgNode]
      .code
      .toSetImmutable
  }
}
