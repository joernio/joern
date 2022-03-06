package io.joern.pysrc2cpg.cpg

import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.joern.pysrc2cpg.Py2CpgTestContext
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import overflowdb.traversal.NodeOps

class CallCpgTests extends AnyFreeSpec with Matchers {
  "call on identifier" - {
    lazy val cpg = Py2CpgTestContext.buildCpg("""func(a, b)""".stripMargin)

    "test call node properties" in {
      val callNode = cpg.call.codeExact("func(a, b)").head
      callNode.name shouldBe ""
      callNode.signature shouldBe ""
      callNode.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      callNode.lineNumber shouldBe Some(1)
    }

    "test call receiver" in {
      val callNode     = cpg.call.codeExact("func(a, b)").head
      val callReceiver = callNode.receiver.isIdentifier.head
      callReceiver.code shouldBe "func"

      callNode.astChildren.order(0).head shouldBe callReceiver
      callNode.start.argument.b should not contain callReceiver
    }

    "test call instance param" in {
      val callNode = cpg.call.codeExact("func(a, b)").head
      callNode.argumentOption(0) shouldBe None
    }

    "test call arguments" in {
      val callNode = cpg.call.codeExact("func(a, b)").head
      val arg1     = callNode.argument(1)
      arg1.code shouldBe "a"

      callNode.astChildren.order(1).head shouldBe arg1

      val arg2 = callNode.argument(2)
      arg2.code shouldBe "b"

      callNode.astChildren.order(2).head shouldBe arg2
    }
  }

  "call on identifier with named argument" - {
    lazy val cpg = Py2CpgTestContext.buildCpg("""func(a, b, namedPar = c)""".stripMargin)

    "test call node properties" in {
      val callNode = cpg.call.codeExact("func(a, b, namedPar = c)").head
      callNode.name shouldBe ""
      callNode.signature shouldBe ""
      callNode.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      callNode.lineNumber shouldBe Some(1)
    }

    "test call arguments" in {
      val callNode = cpg.call.codeExact("func(a, b, namedPar = c)").head
      val arg1     = callNode.argument(1)
      arg1.code shouldBe "a"

      callNode.astChildren.order(1).head shouldBe arg1

      val arg2 = callNode.argument(2)
      arg2.code shouldBe "b"

      callNode.astChildren.order(2).head shouldBe arg2

      var namedArg = callNode.astChildren.order(3).isIdentifier.head
      namedArg.code shouldBe "c"
      namedArg.argumentIndex shouldBe -1
      namedArg.argumentName shouldBe Some("namedPar")
    }
  }

  "call on member" - {
    lazy val cpg = Py2CpgTestContext.buildCpg("""x.func(a, b)""".stripMargin)

    "test call node properties" in {
      val callNode = cpg.call.codeExact("x.func(a, b)").head
      callNode.name shouldBe ""
      callNode.signature shouldBe ""
      callNode.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      callNode.lineNumber shouldBe Some(1)
    }

    "test call receiver" in {
      val callNode     = cpg.call.codeExact("x.func(a, b)").head
      val callReceiver = callNode.receiver.head
      callReceiver.code shouldBe "x.func"

      callNode.astChildren.order(0).head shouldBe callReceiver
      callNode.start.argument.b should not contain callReceiver
    }

    "test call instance param" in {
      val callNode    = cpg.call.codeExact("x.func(a, b)").head
      val instanceArg = callNode.argument(0)
      instanceArg.code shouldBe "x"

      callNode.astChildren.order(1).head shouldBe instanceArg
    }

    "test call arguments" in {
      val callNode = cpg.call.codeExact("x.func(a, b)").head
      val arg1     = callNode.argument(1)
      arg1.code shouldBe "a"

      callNode.astChildren.order(2).head shouldBe arg1

      val arg2 = callNode.argument(2)
      arg2.code shouldBe "b"

      callNode.astChildren.order(3).head shouldBe arg2
    }
  }

  "call on member with named argument" - {
    lazy val cpg = Py2CpgTestContext.buildCpg("""x.func(a, b, namedPar = c)""".stripMargin)

    "test call node properties" in {
      val callNode = cpg.call.codeExact("x.func(a, b, namedPar = c)").head
      callNode.name shouldBe ""
      callNode.signature shouldBe ""
      callNode.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      callNode.lineNumber shouldBe Some(1)
    }

    "test call arguments" in {
      val callNode = cpg.call.codeExact("x.func(a, b, namedPar = c)").head
      val arg1     = callNode.argument(1)
      arg1.code shouldBe "a"

      callNode.astChildren.order(2).head shouldBe arg1

      val arg2 = callNode.argument(2)
      arg2.code shouldBe "b"

      callNode.astChildren.order(3).head shouldBe arg2

      var namedArg = callNode.astChildren.order(4).isIdentifier.head
      namedArg.code shouldBe "c"
      namedArg.argumentIndex shouldBe -1
      namedArg.argumentName shouldBe Some("namedPar")
    }
  }

}
