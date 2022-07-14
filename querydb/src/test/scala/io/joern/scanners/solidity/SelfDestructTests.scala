package io.joern.scanners.solidity
import io.joern.suites.SolidityQueryTestSuite
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.semanticcpg.language._
import io.joern.console.scan._
class SelfDestructTests extends SolidityQueryTestSuite {
  override def queryBundle: SelfDestructVulnerability.type = SelfDestructVulnerability

  "Self Destruct not protected" in {
    queryBundle.isSelfDestructVulnerability()(cpg).map(_.evidence) match {
      case List(IndexedSeq(expr: nodes.Expression)) =>
        expr.method.name shouldBe "selfDestructUnsafe"
      case x =>
        println(x)
        fail()
    }
  }

}
