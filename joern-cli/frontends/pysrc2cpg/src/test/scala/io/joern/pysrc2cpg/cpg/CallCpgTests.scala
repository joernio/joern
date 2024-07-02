package io.joern.pysrc2cpg.cpg

import io.joern.pysrc2cpg.PySrc2CpgFixture
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.semanticcpg.language.*
import overflowdb.traversal.NodeOps

import java.io.File

class CallCpgTests extends PySrc2CpgFixture(withOssDataflow = false) {

  "call on identifier" should {
    lazy val cpg = code("""func(a, b)""".stripMargin, "test.py")

    "test call node properties" in {
      val callNode = cpg.call.codeExact("func(a, b)").head
      callNode.name shouldBe "func"
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

  "call on identifier with named argument" should {
    lazy val cpg = code("""func(a, b, namedPar = c)""".stripMargin, "test.py")

    "test call node properties" in {
      val callNode = cpg.call.codeExact("func(a, b, namedPar = c)").head
      callNode.name shouldBe "func"
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

      val namedArg = callNode.astChildren.order(3).isIdentifier.head
      namedArg.code shouldBe "c"
      namedArg.argumentIndex shouldBe -1
      namedArg.argumentName shouldBe Some("namedPar")
    }
  }

  "call on member" should {
    lazy val cpg = code("""x.func(a, b)""".stripMargin, "test.py")

    "test call node properties" in {
      val callNode = cpg.call.codeExact("x.func(a, b)").head
      callNode.name shouldBe "func"
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

  "call on member with named argument" should {
    lazy val cpg = code("""x.func(a, b, namedPar = c)""".stripMargin, "test.py")

    "test call node properties" in {
      val callNode = cpg.call.codeExact("x.func(a, b, namedPar = c)").head
      callNode.name shouldBe "func"
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

      val namedArg = callNode.astChildren.order(4).isIdentifier.head
      namedArg.code shouldBe "c"
      namedArg.argumentIndex shouldBe -1
      namedArg.argumentName shouldBe Some("namedPar")
    }
  }

  "call following a definition within the same module" should {
    lazy val cpg = code(
      """
        |def func(a, b):
        | return a + b
        |
        |x = func(a, b)
        |""".stripMargin,
      "test.py"
    )

    "test call node properties" in {
      val callNode = cpg.call.codeExact("func(a, b)").head
      callNode.name shouldBe "func"
      callNode.signature shouldBe ""
      callNode.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      callNode.lineNumber shouldBe Some(5)
      callNode.methodFullName shouldBe "test.py:<module>.func"
    }
  }

  "call from a function defined from an imported module" should {

    lazy val cpg = code(
      """
        |from foo import foo_func, faz as baz
        |from foo.bar import bar_func
        |
        |
        |x = foo_func(a, b)
        |y = bar_func(a, b)
        |z = baz(a, b)
        |""".stripMargin,
      "test.py"
    ).moreCode(
      """
        |def foo_func(a, b):
        | return a + b
        |
        |def faz(a, b):
        | return a / b
        |""".stripMargin,
      "foo.py"
    ).moreCode(
      """
          |def bar_func(a, b):
          | return a - b
          |""".stripMargin,
      Seq("foo", "bar", "__init__.py").mkString(File.separator)
    )

    "test that the identifiers are not set to the function pointers but rather the 'ANY' return value" in {
      val List(x, y, z) = cpg.identifier.name("x", "y", "z").l
      x.typeFullName shouldBe "foo.py:<module>.foo_func.<returnValue>"
      y.typeFullName shouldBe Seq("foo", "bar", "__init__.py:<module>.bar_func.<returnValue>").mkString(File.separator)
      z.typeFullName shouldBe "foo.py:<module>.faz.<returnValue>"
    }

    "test call node properties for normal import from module on root path" in {
      val callNode = cpg.call.codeExact("foo_func(a, b)").head
      callNode.name shouldBe "foo_func"
      callNode.signature shouldBe ""
      callNode.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      callNode.lineNumber shouldBe Some(6)
      callNode.methodFullName shouldBe "foo.py:<module>.foo_func"
      callNode.callee(NoResolve).isExternal.headOption shouldBe Some(false)
    }

    "test call node properties for normal import from module deeper on a module path" in {
      val callNode = cpg.call.codeExact("bar_func(a, b)").head
      callNode.name shouldBe "bar_func"
      callNode.signature shouldBe ""
      callNode.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      callNode.lineNumber shouldBe Some(7)
      callNode.methodFullName shouldBe Seq("foo", "bar", "__init__.py:<module>.bar_func").mkString(File.separator)
      callNode.callee(NoResolve).isExternal.headOption shouldBe Some(false)
    }

    "test call node properties for aliased import from module on root path" in {
      val callNode = cpg.call.codeExact("baz(a, b)").head
      callNode.name shouldBe "baz"
      callNode.signature shouldBe ""
      callNode.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      callNode.lineNumber shouldBe Some(8)
      callNode.methodFullName shouldBe "foo.py:<module>.faz"
      callNode.callee(NoResolve).isExternal.headOption shouldBe Some(false)
    }
  }

}
