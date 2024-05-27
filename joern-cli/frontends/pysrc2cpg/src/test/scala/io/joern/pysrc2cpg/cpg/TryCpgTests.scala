package io.joern.pysrc2cpg.cpg

import io.joern.pysrc2cpg.Py2CpgTestContext
import io.shiftleft.semanticcpg.language.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TryCpgTests extends AnyFreeSpec with Matchers {
  private val cpg = Py2CpgTestContext.buildCpg("""
      |def divide(x, y):
      |  try:
      |    result = x / y
      |  except ZeroDivisionError:
      |    print("division by zero!")
      |  except OtherError:
      |    print("other error")
      |  else:
      |    print("result is", result)
      |  finally:
      |    print("executing finally clause")
      |""".stripMargin)

  "test try node properties" in {
    val List(tryNode)      = cpg.tryBlock.l
    val List(tryBlock)     = tryNode.astChildren.order(1).l
    val List(tryBlockCall) = tryBlock.astChildren.isCall.l
    tryBlockCall.code shouldBe "result = x / y"
    val List(catchA, catchB) = tryNode.astChildren.isControlStructure.isCatch.l
    catchA.order shouldBe 2
    catchA.ast.isCall.code.l shouldBe List("""print("division by zero!")""")
    catchB.order shouldBe 3
    catchB.ast.isCall.code.l shouldBe List("""print("other error")""")
    val List(elseBlock) = tryNode.astChildren.isControlStructure.isElse.l
    elseBlock.order shouldBe 4
    elseBlock.ast.isCall.code.l shouldBe List("""print("result is", result)""")
    val List(finallyBlock) = tryNode.astChildren.isControlStructure.isFinally.l
    finallyBlock.order shouldBe 5
    finallyBlock.ast.isCall.code.l shouldBe List("""print("executing finally clause")""")
  }
}
