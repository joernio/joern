package io.joern.pysrc2cpg.cpg

import io.joern.pysrc2cpg.Py2CpgTestContext
import io.shiftleft.codepropertygraph.generated.nodes.Call
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
    val _ :: (topLevelExprs: Call) :: Nil = cpg.method.fullName("test.py:<module>").topLevelExpressions.l: @unchecked
    topLevelExprs.code shouldBe "pass"
    topLevelExprs.methodFullName shouldBe "<operator>.pass"
  }
}
