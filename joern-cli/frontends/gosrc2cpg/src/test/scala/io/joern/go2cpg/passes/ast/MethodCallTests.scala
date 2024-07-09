package io.joern.go2cpg.passes.ast

import io.joern.dataflowengineoss.language.*
import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.joern.gosrc2cpg.astcreation.Defines
import io.shiftleft.codepropertygraph.generated.edges.Ref
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators, nodes}
import io.shiftleft.semanticcpg.language.*
import java.io.File

class MethodCallTests extends GoCodeToCpgSuite(withOssDataflow = true) {

  "Simple method call use case" should {
    val cpg = code("""
        |package main
        |func foo() {
        |  bar()
        |}
        |func bar() {
        |}
        |""".stripMargin)
    "Check call node properties" in {
      cpg.call("bar").size shouldBe 1
      val List(x) = cpg.call("bar").l
      x.code shouldBe "bar()"
      x.methodFullName shouldBe "main.bar"
      x.signature shouldBe "main.bar()"
      x.order shouldBe 1
      x.lineNumber shouldBe Option(4)
    }

    "traversal from call to caller method node" in {
      val List(x) = cpg.call("bar").method.l
      x.name shouldBe "foo"
    }

    "traversal from call to callee method node" in {
      val List(x) = cpg.call("bar").callee.l
      x.name shouldBe "bar"
      x.isExternal shouldBe false
    }

    "traversal from method to call node" in {
      val List(x) = cpg.method("bar").callIn.l
      x.name shouldBe "bar"
    }

    "traversal from method to all the child call nodes" in {
      val List(x) = cpg.method("foo").call.l
      x.name shouldBe "bar"
    }
  }

  "Method defined in the same file and after method is called." should {
    val cpg = code("""
        |package main
        |func foo() {
        |  var a = bar()
        |}
        |func bar() int {
        |  return 0
        |}
        |""".stripMargin)
    "Check call node properties" in {
      cpg.call("bar").size shouldBe 1
      val List(x) = cpg.call("bar").l
      x.code shouldBe "bar()"
      x.methodFullName shouldBe "main.bar"
      x.signature shouldBe "main.bar()int"
      x.order shouldBe 2
      x.lineNumber shouldBe Option(4)
      x.typeFullName shouldBe "int"
    }

    "traversal from call to caller method node" in {
      val List(x) = cpg.call("bar").method.l
      x.name shouldBe "foo"
    }

    "traversal from call to callee method node" in {
      val List(x) = cpg.call("bar").callee.l
      x.name shouldBe "bar"
      x.isExternal shouldBe false
    }
  }

  "Method defined in the same file and before method is called." should {
    val cpg = code("""
        |package main
        |func bar() int {
        |  return 0
        |}
        |func foo() {
        |  var a = bar()
        |}
        |""".stripMargin)
    "Check call node properties" in {
      cpg.call("bar").size shouldBe 1
      val List(x) = cpg.call("bar").l
      x.code shouldBe "bar()"
      x.methodFullName shouldBe "main.bar"
      x.signature shouldBe "main.bar()int"
      x.order shouldBe 2
      x.lineNumber shouldBe Option(7)
      x.typeFullName shouldBe "int"
    }

    "traversal from call to caller method node" in {
      val List(x) = cpg.call("bar").method.l
      x.name shouldBe "foo"
    }

    "traversal from call to callee method node" in {
      val List(x) = cpg.call("bar").callee.l
      x.name shouldBe "bar"
      x.isExternal shouldBe false
    }
  }

  "Method call to method with void return from the same package but from other file" should {
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |""".stripMargin,
      "go.mod"
    ).moreCode(
      """
        |package main
        |func bar() {
        |}
        |""".stripMargin,
      "mainlib.go"
    ).moreCode(
      """
        |package main
        |func foo() {
        |  bar()
        |}
        |""".stripMargin,
      "main.go"
    )
    "Check call node properties" in {
      cpg.call("bar").size shouldBe 1
      val List(x) = cpg.call("bar").l
      x.code shouldBe "bar()"
      x.methodFullName shouldBe "main.bar"
      x.signature shouldBe "main.bar()"
      x.order shouldBe 1
      x.lineNumber shouldBe Option(4)
    }

    "traversal from call to caller method node" in {
      val List(x) = cpg.call("bar").method.l
      x.name shouldBe "foo"
    }

    "traversal from call to callee method node" in {
      val List(x) = cpg.call("bar").callee.l
      x.name shouldBe "bar"
      x.isExternal shouldBe false
    }
  }

  "Method call to method with int return from the same package but from other file" should {
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |""".stripMargin,
      "go.mod"
    ).moreCode(
      """
        |package main
        |func bar() int{
        |  return 0
        |}
        |""".stripMargin,
      "mainlib.go"
    ).moreCode(
      """
        |package main
        |func foo() {
        |  var a = bar()
        |}
        |""".stripMargin,
      "main.go"
    )
    "Check call node properties" in {
      cpg.call("bar").size shouldBe 1
      val List(x) = cpg.call("bar").l
      x.code shouldBe "bar()"
      x.methodFullName shouldBe "main.bar"
      x.signature shouldBe "main.bar()int"
      x.order shouldBe 2
      x.lineNumber shouldBe Option(4)
      x.typeFullName shouldBe "int"
    }

    "traversal from call to caller method node" in {
      val List(x) = cpg.call("bar").method.l
      x.name shouldBe "foo"
    }

    "traversal from call to callee method node" in {
      val List(x) = cpg.call("bar").callee.l
      x.name shouldBe "bar"
      x.isExternal shouldBe false
    }
  }

  "Method call to method with int array return from the same project but another package" should {
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |""".stripMargin,
      "go.mod"
    ).moreCode(
      """
        |package fpkg
        |func bar() []int{
        |  return [2]int{0,10}
        |}
        |""".stripMargin,
      Seq("fpkg", "mainlib.go").mkString(File.separator)
    ).moreCode(
      """
        |package main
        |import "joern.io/sample/fpkg"
        |func foo() {
        |  var a = fpkg.bar()
        |}
        |""".stripMargin,
      "main.go"
    )
    "Check call node properties" in {
      cpg.call("bar").size shouldBe 1
      val List(x) = cpg.call("bar").l
      x.code shouldBe "fpkg.bar()"
      x.methodFullName shouldBe "joern.io/sample/fpkg.bar"
      x.signature shouldBe "joern.io/sample/fpkg.bar()[]int"
      x.order shouldBe 2
      x.lineNumber shouldBe Option(5)
      x.typeFullName shouldBe "[]int"
    }

    "traversal from call to caller method node" in {
      val List(x) = cpg.call("bar").method.l
      x.name shouldBe "foo"
    }

    "traversal from call to callee method node" in {
      val List(x) = cpg.call("bar").callee.l
      x.name shouldBe "bar"
      x.isExternal shouldBe false
    }
  }

  "Method call to method with int return and argument from the same project but another package" should {
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |""".stripMargin,
      "go.mod"
    ).moreCode(
      """
        |package fpkg
        |func bar(a int, b string) int{
        |  return a
        |}
        |""".stripMargin,
      Seq("fpkg", "mainlib.go").mkString(File.separator)
    ).moreCode(
      """
        |package main
        |import "joern.io/sample/fpkg"
        |func foo() {
        |  var c = fpkg.bar(10, "somestr")
        |}
        |""".stripMargin,
      "main.go"
    )
    "Check call node properties" in {
      cpg.call("bar").size shouldBe 1
      val List(x) = cpg.call("bar").l
      x.code shouldBe "fpkg.bar(10, \"somestr\")"
      x.methodFullName shouldBe "joern.io/sample/fpkg.bar"
      x.signature shouldBe "joern.io/sample/fpkg.bar(int, string)int"
      x.order shouldBe 2
      x.lineNumber shouldBe Option(5)
      x.typeFullName shouldBe "int"
    }

    "traversal from call to caller method node" in {
      val List(x) = cpg.call("bar").method.l
      x.name shouldBe "foo"
    }

    "traversal from call to callee method node" in {
      val List(x) = cpg.call("bar").callee.l
      x.name shouldBe "bar"
      x.isExternal shouldBe false
    }

    "traversal from call to arguments" in {
      val List(a, b) = cpg.call("bar").argument.l
      a.isInstanceOf[nodes.Literal] shouldBe true
      a.code shouldBe "10"
      a.order shouldBe 1

      b.isInstanceOf[nodes.Literal] shouldBe true
      b.code shouldBe "\"somestr\""
      b.order shouldBe 2
    }

    "traversal from argument to call node" in {
      val List(x) = cpg.argument("10").inCall.l
      x.name shouldBe "bar"

      val List(y) = cpg.argument("\"somestr\"").inCall.l
      x.name shouldBe "bar"
    }

    "simple data flow test" in {
      val src  = cpg.literal("10").l
      val sink = cpg.identifier("c").l
      sink.reachableByFlows(src).size shouldBe 1
    }
  }

  "Method call to method with struct type return from the same project but another package" should {
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |""".stripMargin,
      "go.mod"
    ).moreCode(
      """
        |package lib
        |type StringAlias string
        |""".stripMargin,
      Seq("lib", "typelib.go").mkString(File.separator)
    ).moreCode(
      """
        |package fpkg
        |import "joern.io/sample/lib"
        |func bar() lib.StringAlias{
        |  return ""
        |}
        |""".stripMargin,
      Seq("fpkg", "mainlib.go").mkString(File.separator)
    ).moreCode(
      """
        |package main
        |import "joern.io/sample/fpkg"
        |func foo() {
        |  var a = fpkg.bar()
        |}
        |""".stripMargin,
      "main.go"
    )
    "Check call node properties" in {
      cpg.call("bar").size shouldBe 1
      val List(x) = cpg.call("bar").l
      x.code shouldBe "fpkg.bar()"
      x.methodFullName shouldBe "joern.io/sample/fpkg.bar"
      x.signature shouldBe "joern.io/sample/fpkg.bar()joern.io/sample/lib.StringAlias"
      x.order shouldBe 2
      x.lineNumber shouldBe Option(5)
      x.typeFullName shouldBe "joern.io/sample/lib.StringAlias"
    }

    "traversal from call to caller method node" in {
      val List(x) = cpg.call("bar").method.l
      x.name shouldBe "foo"
    }

    "traversal from call to callee method node" in {
      val List(x) = cpg.call("bar").callee.l
      x.name shouldBe "bar"
      x.isExternal shouldBe false
    }
  }

  "Method call to method imported from another third party package" should {
    val cpg = code("""
        |package main
        |import "joern.io/sample/fpkg"
        |func foo() {
        |  fpkg.bar()
        |}
        |""".stripMargin)

    "Check call node properties" in {
      cpg.call("bar").size shouldBe 1
      val List(x) = cpg.call("bar").l
      x.code shouldBe "fpkg.bar()"
      x.methodFullName shouldBe "joern.io/sample/fpkg.bar"
      x.signature shouldBe "joern.io/sample/fpkg.bar()"
      x.order shouldBe 1
      x.lineNumber shouldBe Option(5)
      x.typeFullName shouldBe "joern.io/sample/fpkg.bar.<ReturnType>.<unknown>"
    }

    "traversal from call to caller method node" in {
      val List(x) = cpg.call("bar").method.l
      x.name shouldBe "foo"
    }

    "traversal from call to callee method node" in {
      val List(x) = cpg.call("bar").callee.l
      x.name shouldBe "bar"
      x.isExternal shouldBe true
    }
  }

  "Method call to method with arguments imported from another third party package" should {
    val cpg = code("""
        |package main
        |import "joern.io/sample/fpkg"
        |func foo() {
        |  var a = 10
        |  var b = "somestr"
        |  fpkg.bar(a, b)
        |}
        |""".stripMargin)

    "Check call node properties" in {
      cpg.call("bar").size shouldBe 1
      val List(x) = cpg.call("bar").l
      x.code shouldBe "fpkg.bar(a, b)"
      x.methodFullName shouldBe "joern.io/sample/fpkg.bar"
      x.order shouldBe 5
      x.lineNumber shouldBe Option(7)
      x.typeFullName shouldBe "joern.io/sample/fpkg.bar.<ReturnType>.<unknown>"
    }

    "Signature property should be updated with argument types" ignore {
      // TODO: update the signature, keeping it on low priority until we find it is required for flows.
      val List(x) = cpg.call("bar").l
      x.signature shouldBe "joern.io/sample/fpkg.bar(int, string)"
    }

    "traversal from call to caller method node" in {
      val List(x) = cpg.call("bar").method.l
      x.name shouldBe "foo"
    }

    "traversal from call to callee method node" in {
      val List(x) = cpg.call("bar").callee.l
      x.name shouldBe "bar"
      x.isExternal shouldBe true
    }

    "Check arguments nodes" in {
      cpg.call("bar").argument.size shouldBe 2
      val List(a: Identifier, b: Identifier) = cpg.call("bar").argument.l: @unchecked
      a.name shouldBe "a"
      b.name shouldBe "b"
    }
  }

  "Method call to builtin method recover()" should {
    val cpg = code("""
        |package main
        |func foo() {
        |  recover()
        |}
        |""".stripMargin)

    "Check call node properties" in {
      cpg.call("recover").size shouldBe 1
      val List(x) = cpg.call("recover").l
      x.code shouldBe "recover()"
      x.methodFullName shouldBe "recover"
      x.signature shouldBe "recover()any"
      x.order shouldBe 1
      x.lineNumber shouldBe Option(4)
    }

    "traversal from call to caller method node" in {
      val List(x) = cpg.call("recover").method.l
      x.name shouldBe "foo"
    }

    "traversal from call to callee method node" in {
      val List(x) = cpg.call("recover").callee.l
      x.name shouldBe "recover"
      x.isExternal shouldBe true
    }
  }

  "Method call to builtin method cap() and println" should {
    val cpg = code("""
        |package main
        |func foo() {
        |   var a []int = []int{10, 20}
        |	var b = cap(a)
        |	println(b)
        |}
        |""".stripMargin)

    "Check call node properties cap" in {
      cpg.call("cap").size shouldBe 1
      val List(x) = cpg.call("cap").l
      x.code shouldBe "cap(a)"
      x.methodFullName shouldBe "cap"
      x.signature shouldBe "cap(any)int"
      x.order shouldBe 2
      x.lineNumber shouldBe Option(5)
    }

    "Check call node properties println" in {
      cpg.call("println").size shouldBe 1
      val List(x) = cpg.call("println").l
      x.code shouldBe "println(b)"
      x.methodFullName shouldBe "println"
      x.signature shouldBe "println([]any)"
      x.order shouldBe 5
      x.lineNumber shouldBe Option(6)
    }

    "traversal from call to caller method node" in {
      val List(x) = cpg.call("cap").method.l
      x.name shouldBe "foo"
    }

    "traversal from call to callee method node" in {
      val List(x) = cpg.call("cap").callee.l
      x.name shouldBe "cap"
      x.isExternal shouldBe true
    }

    "traversal from call to argument node" in {
      cpg.call("cap").argument.size shouldBe 1
      val List(arg1) = cpg.call("cap").argument.l
      arg1.isInstanceOf[nodes.Identifier] shouldBe true
      arg1.asInstanceOf[nodes.Identifier].name shouldBe "a"
      arg1.code shouldBe "a"
      arg1.order shouldBe 1
      arg1.argumentIndex shouldBe 1
    }

    "traversal from argument to call node" in {
      val List(assignment, call) = cpg.argument("a").inCall.l
      assignment.name shouldBe "<operator>.assignment"
      call.name shouldBe "cap"

      val List(printlnCall) = cpg.argument("b").inCall.lineNumber(6).l
      printlnCall.name shouldBe "println"
    }

    "data flow test" in {
      val src  = cpg.identifier("a").lineNumber(4).l
      val sink = cpg.call("println").l
      sink.reachableByFlows(src).size shouldBe 1
    }
  }

  "Method call check with parameters being passed while they are casted to another type" ignore {
    val cpg = code("""
        |package main
        |func foo() {
        |	var a = bar([]byte("test"))
        |   var b = bar(map[string]interface{}(labels))
        |   var c = bar(reflect.TypeOf((*MockDB)(nil).CheckHealth))
        |}
        |""".stripMargin)
    // TODO: Need to work on how this casting should get translated to AST
    "Check call node properties" in {
      cpg.call("bar").size shouldBe 3
      val List(a, b, c) = cpg.call("bar").l
    }
  }
}
