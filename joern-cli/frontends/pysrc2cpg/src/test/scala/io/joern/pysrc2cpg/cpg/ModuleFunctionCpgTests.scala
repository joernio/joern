package io.joern.pysrc2cpg.cpg

import io.shiftleft.semanticcpg.language.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import io.joern.pysrc2cpg.testfixtures.PySrc2CpgFixture

class ModuleFunctionCpgTests extends PySrc2CpgFixture with Matchers {
  val cpg = code("""pass""".stripMargin, "test.py")

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
