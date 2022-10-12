package io.joern.pysrc2cpg.cpg

import io.joern.pysrc2cpg.Constants
import io.joern.pysrc2cpg.Py2CpgTestContext
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class FunctionDefCpgTests extends AnyFreeSpec with Matchers {
  "normal argument function" - {
    lazy val cpg = Py2CpgTestContext.buildCpg("""def func(a, b):
        |  pass
        |""".stripMargin)

    "test method node properties" in {
      val methodNode = cpg.method.fullName("test.py:<module>.func").head
      methodNode.name shouldBe "func"
      methodNode.fullName shouldBe "test.py:<module>.func"
      methodNode.filename shouldBe "<absoluteTestPath>/test.py"
      methodNode.isExternal shouldBe false
      methodNode.lineNumber shouldBe Some(1)
      methodNode.columnNumber shouldBe Some(1)
      methodNode.lineNumberEnd shouldBe Some(2)
      methodNode.columnNumberEnd shouldBe Some(6)
    }

    "test method modifier" in {
      cpg.method.fullName("test.py:<module>.func").isVirtual.nonEmpty shouldBe true
    }

    "test method parameter nodes" in {
      val parameter1 = cpg.method.fullName("test.py:<module>.func").parameter.order(1).head
      parameter1.name shouldBe "a"
      parameter1.index shouldBe 1
      parameter1.typeFullName shouldBe Constants.ANY

      val parameter2 = cpg.method.fullName("test.py:<module>.func").parameter.order(2).head
      parameter2.name shouldBe "b"
      parameter2.index shouldBe 2
      parameter2.typeFullName shouldBe Constants.ANY
    }

    "test method return node" in {
      cpg.method.fullName("test.py:<module>.func").methodReturn.code.head shouldBe "RET"
      cpg.method.fullName("test.py:<module>.func").methodReturn.typeFullName.head shouldBe Constants.ANY
    }

    "test method body" in {
      val topLevelExprs = cpg.method.fullName("test.py:<module>.func").topLevelExpressions.l
      topLevelExprs.size shouldBe 1
      topLevelExprs.isCall.head.code shouldBe "pass"
      topLevelExprs.isCall.head.methodFullName shouldBe "<operator>.pass"
    }

    "test function method ref" in {
      cpg.methodRef("func").referencedMethod.fullName.head shouldBe
        "test.py:<module>.func"
    }

    "test assignment of method ref to local variable" in {
      val assignNode = cpg.methodRef("func").astParent.isCall.head
      assignNode.code shouldBe "func = def func(...)"
    }

    "test existence of local variable in module function" in {
      cpg.method.fullName("test.py:<module>").local.name.l should contain("func")
    }

    "test corresponding type, typeDecl and binding" in {
      val bindingTypeDecl =
        cpg.method.fullName("test.py:<module>.func").referencingBinding.bindingTypeDecl.head

      bindingTypeDecl.name shouldBe "func"
      bindingTypeDecl.fullName shouldBe "test.py:<module>.func"
      bindingTypeDecl.filename shouldBe "<absoluteTestPath>/test.py"
      bindingTypeDecl.lineNumber shouldBe Some(1)
      bindingTypeDecl.columnNumber shouldBe Some(1)

      bindingTypeDecl.referencingType.name.head shouldBe "func"
      bindingTypeDecl.referencingType.fullName.head shouldBe "test.py:<module>.func"
    }
  }

  "positional argument function" - {
    lazy val cpg = Py2CpgTestContext.buildCpg("""def func(a, b, /):
        |  pass
        |""".stripMargin)

    "test method parameter nodes" in {
      val parameter1 = cpg.method.fullName("test.py:<module>.func").parameter.order(1).head
      parameter1.name shouldBe "a"
      parameter1.index shouldBe 1
      parameter1.typeFullName shouldBe Constants.ANY

      val parameter2 = cpg.method.fullName("test.py:<module>.func").parameter.order(2).head
      parameter2.name shouldBe "b"
      parameter2.index shouldBe 2
      parameter2.typeFullName shouldBe Constants.ANY
    }
  }

  "mixed argument function" - {
    lazy val cpg = Py2CpgTestContext.buildCpg("""def func(a, b, /, c):
        |  pass
        |""".stripMargin)

    "test method parameter nodes" in {
      val parameter1 = cpg.method.fullName("test.py:<module>.func").parameter.order(1).head
      parameter1.name shouldBe "a"
      parameter1.index shouldBe 1
      parameter1.typeFullName shouldBe Constants.ANY

      val parameter2 = cpg.method.fullName("test.py:<module>.func").parameter.order(2).head
      parameter2.name shouldBe "b"
      parameter2.index shouldBe 2
      parameter2.typeFullName shouldBe Constants.ANY

      val parameter3 = cpg.method.fullName("test.py:<module>.func").parameter.order(3).head
      parameter3.name shouldBe "c"
      parameter3.index shouldBe 3
      parameter3.typeFullName shouldBe Constants.ANY
    }
  }

  "decorated function" - {
    lazy val cpg = Py2CpgTestContext.buildCpg("""@abc(arg)
        |@staticmethod
        |def func():
        |  pass
        |""".stripMargin)

    "test decorator wrapping of method reference" in {
      cpg.methodRef("func").astParent.astParent.astParent.isCall.head.code shouldBe
        "func = abc(arg)(staticmethod(def func(...)))"
    }

  }

}
