package io.joern.pysrc2cpg.cpg

import io.joern.pysrc2cpg.testfixtures.Py2CpgTestContext
import io.shiftleft.semanticcpg.language.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ModuleFunctionCpgTests extends AnyFreeSpec with Matchers {
  lazy val cpg = Py2CpgTestContext.buildCpg("""pass""".stripMargin)

  "test module method node properties" in {
    val methodNode = cpg.method.fullName("test.py:<module>").head
    methodNode.name shouldBe "<module>"
    methodNode.fullName shouldBe "test.py:<module>"
    methodNode.lineNumber shouldBe Some(1)
  }

  "test method body" in {
    val topLevelExprs = cpg.method.fullName("test.py:<module>").topLevelExpressions.l
    topLevelExprs.size shouldBe 1
    topLevelExprs.isCall.head.code shouldBe "pass"
    topLevelExprs.isCall.head.methodFullName shouldBe "<operator>.pass"
  }
}
