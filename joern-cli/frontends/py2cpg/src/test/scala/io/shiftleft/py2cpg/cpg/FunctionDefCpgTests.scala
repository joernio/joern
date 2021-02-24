package io.shiftleft.py2cpg.cpg

import io.shiftleft.py2cpg.Constants
import io.shiftleft.py2cpg.Py2CpgTestContext
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class FunctionDefCpgTests extends AnyFreeSpec with Matchers {
  "normal argument function" - {
    lazy val cpg = Py2CpgTestContext.buildCpg(
      """def func(a, b):
        |  pass
        |""".stripMargin
    )

    "test method node properties" in {
      val methodNode = cpg.method("test.py:module.func").head
      methodNode.name shouldBe "func"
      methodNode.fullName shouldBe "test.py:module.func"
      methodNode.lineNumber shouldBe Some(1)
    }

    "test method modifier" in {
      cpg.method("test.py:module.func").isVirtual.nonEmpty shouldBe true
    }

    "test method parameter nodes" in {
      cpg.method("test.py:module.func").parameter.order(1).name.head shouldBe "a"
      cpg.method("test.py:module.func").parameter.order(1).typeFullName.head shouldBe Constants.ANY
      cpg.method("test.py:module.func").parameter.order(2).name.head shouldBe "b"
      cpg.method("test.py:module.func").parameter.order(2).typeFullName.head shouldBe Constants.ANY
    }

    "test method return node" in {
      cpg.method("test.py:module.func").methodReturn.code.head shouldBe "RET"
      cpg.method("test.py:module.func").methodReturn.typeFullName.head shouldBe Constants.ANY
    }

    "test method body" in {
      val topLevelExprs = cpg.method("test.py:module.func").topLevelExpressions.l
      topLevelExprs.size shouldBe 1
      topLevelExprs.isCall.head.code shouldBe "pass"
      topLevelExprs.isCall.head.methodFullName shouldBe "<operator>.pass"
    }

    "test function method ref" in {
      cpg.methodRef("test.py:module.func").referencedMethod.fullName.head shouldBe
        "test.py:module.func"
    }

    "test corresponding type, typeDecl and binding" in {
      val bindingTypeDecl =
        cpg.method("test.py:module.func").referencingBinding.bindingTypeDecl.head

      bindingTypeDecl.name shouldBe "func"
      bindingTypeDecl.fullName shouldBe "test.py:module.func"

      bindingTypeDecl.referencingType.name.head shouldBe "func"
      bindingTypeDecl.referencingType.fullName.head shouldBe "test.py:module.func"
    }
  }

  "positional argument function" - {
    lazy val cpg = Py2CpgTestContext.buildCpg(
      """def func(a, b, /):
        |  pass
        |""".stripMargin
    )

    "test method parameter nodes" in {
      cpg.method("test.py:module.func").parameter.order(1).name.head shouldBe "a"
      cpg.method("test.py:module.func").parameter.order(1).typeFullName.head shouldBe Constants.ANY
      cpg.method("test.py:module.func").parameter.order(2).name.head shouldBe "b"
      cpg.method("test.py:module.func").parameter.order(2).typeFullName.head shouldBe Constants.ANY
    }
  }

  "mixed argument function" - {
    lazy val cpg = Py2CpgTestContext.buildCpg(
      """def func(a, b, /, c):
        |  pass
        |""".stripMargin
    )

    "test method parameter nodes" in {
      cpg.method("test.py:module.func").parameter.order(1).name.head shouldBe "a"
      cpg.method("test.py:module.func").parameter.order(1).typeFullName.head shouldBe Constants.ANY
      cpg.method("test.py:module.func").parameter.order(2).name.head shouldBe "b"
      cpg.method("test.py:module.func").parameter.order(2).typeFullName.head shouldBe Constants.ANY
      cpg.method("test.py:module.func").parameter.order(3).name.head shouldBe "c"
      cpg.method("test.py:module.func").parameter.order(3).typeFullName.head shouldBe Constants.ANY
    }
  }

}
