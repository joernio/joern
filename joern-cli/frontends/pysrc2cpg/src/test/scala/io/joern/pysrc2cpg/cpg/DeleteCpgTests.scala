package io.joern.pysrc2cpg.cpg

import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.semanticcpg.language.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import io.joern.pysrc2cpg.testfixtures.PySrc2CpgFixture

class DeleteCpgTests extends PySrc2CpgFixture with Matchers {
  "delete statement" should {
    val cpg = code("""del x, y""".stripMargin)

    "test call node properties" in {
      val callNode = cpg.call.methodFullNameExact("<operator>.delete").head
      callNode.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      callNode.code shouldBe "del x, y"
      callNode.lineNumber shouldBe Some(1)
    }

    "test delete call node ast children" in {
      cpg.call
        .methodFullName("<operator>.delete")
        .astChildren
        .order(1)
        .isIdentifier
        .head
        .code shouldBe "x"
      cpg.call
        .methodFullName("<operator>.delete")
        .astChildren
        .order(2)
        .isIdentifier
        .head
        .code shouldBe "y"
    }

    "test delete call node arguments" in {
      cpg.call
        .methodFullName("<operator>.delete")
        .argument
        .argumentIndex(1)
        .isIdentifier
        .head
        .code shouldBe "x"
      cpg.call
        .methodFullName("<operator>.delete")
        .argument
        .argumentIndex(2)
        .isIdentifier
        .head
        .code shouldBe "y"
    }
  }

  "delete of list" should {
    val cpg = code("""del [a, b]""".stripMargin)

    "have a single delete call node with correct properties" in {
      val callNode = cpg.call.methodFullNameExact("<operator>.delete").loneElement
      callNode.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      callNode.code shouldBe "del [a, b]"
      callNode.lineNumber shouldBe Some(1)
    }

    "flatten list elements as direct arguments" in {
      val deleteCall = cpg.call.methodFullNameExact("<operator>.delete").loneElement
      deleteCall.argument.size shouldBe 2
      deleteCall.argument.argumentIndex(1).isIdentifier.loneElement.code shouldBe "a"
      deleteCall.argument.argumentIndex(2).isIdentifier.loneElement.code shouldBe "b"
    }
  }

  "delete of tuple" should {
    val cpg = code("""del (a, b)""".stripMargin)

    "have a single delete call node with correct properties" in {
      val callNode = cpg.call.methodFullNameExact("<operator>.delete").loneElement
      callNode.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      callNode.code shouldBe "del (a, b)"
      callNode.lineNumber shouldBe Some(1)
    }

    "flatten tuple elements as direct arguments" in {
      val deleteCall = cpg.call.methodFullNameExact("<operator>.delete").loneElement
      deleteCall.argument.size shouldBe 2
      deleteCall.argument.argumentIndex(1).isIdentifier.loneElement.code shouldBe "a"
      deleteCall.argument.argumentIndex(2).isIdentifier.loneElement.code shouldBe "b"
    }
  }

  "delete of single-element tuple" should {
    val cpg = code("""del (a,)""".stripMargin)

    "have a single delete call node with correct properties" in {
      val callNode = cpg.call.methodFullNameExact("<operator>.delete").loneElement
      callNode.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      callNode.code shouldBe "del (a,)"
      callNode.lineNumber shouldBe Some(1)
    }

    "have a single identifier argument" in {
      val deleteCall = cpg.call.methodFullNameExact("<operator>.delete").loneElement
      deleteCall.argument.size shouldBe 1
      deleteCall.argument.argumentIndex(1).isIdentifier.loneElement.code shouldBe "a"
    }
  }

  "delete of mixed plain and grouped targets" should {
    val cpg = code("""del a, [b, c]""".stripMargin)

    "have a single delete call node with correct properties" in {
      val callNode = cpg.call.methodFullNameExact("<operator>.delete").loneElement
      callNode.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      callNode.code shouldBe "del a, [b, c]"
      callNode.lineNumber shouldBe Some(1)
    }

    "flatten all targets as direct arguments in order" in {
      val deleteCall = cpg.call.methodFullNameExact("<operator>.delete").loneElement
      deleteCall.argument.size shouldBe 3
      deleteCall.argument.argumentIndex(1).isIdentifier.loneElement.code shouldBe "a"
      deleteCall.argument.argumentIndex(2).isIdentifier.loneElement.code shouldBe "b"
      deleteCall.argument.argumentIndex(3).isIdentifier.loneElement.code shouldBe "c"
    }
  }

  "delete of non-name targets inside a list" should {
    val cpg = code("""del [a.b, c[0]]""".stripMargin)

    "have a single delete call node with correct properties" in {
      val callNode = cpg.call.methodFullNameExact("<operator>.delete").loneElement
      callNode.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      callNode.code shouldBe "del [a.b, c[0]]"
      callNode.lineNumber shouldBe Some(1)
    }

    "flatten attribute and subscript targets as direct arguments" in {
      val deleteCall = cpg.call.methodFullNameExact("<operator>.delete").loneElement
      deleteCall.argument.size shouldBe 2
      deleteCall.argument.argumentIndex(1).head.code shouldBe "a.b"
      deleteCall.argument.argumentIndex(2).head.code shouldBe "c[0]"
    }
  }

  "delete of arbitrarily nested list/tuple" should {
    val cpg = code("""del [[a, [b]], (c,)]""".stripMargin)

    "have a single delete call node with correct code" in {
      val callNode = cpg.call.methodFullNameExact("<operator>.delete").loneElement
      callNode.code shouldBe "del [[a, [b]], (c,)]"
    }

    "flatten all nested elements as direct arguments in order" in {
      val deleteCall = cpg.call.methodFullNameExact("<operator>.delete").loneElement
      deleteCall.argument.size shouldBe 3
      deleteCall.argument.argumentIndex(1).isIdentifier.loneElement.code shouldBe "a"
      deleteCall.argument.argumentIndex(2).isIdentifier.loneElement.code shouldBe "b"
      deleteCall.argument.argumentIndex(3).isIdentifier.loneElement.code shouldBe "c"
    }
  }

}
