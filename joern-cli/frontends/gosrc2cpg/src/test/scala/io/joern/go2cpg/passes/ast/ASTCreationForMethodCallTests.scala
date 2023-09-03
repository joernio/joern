package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.joern.gosrc2cpg.astcreation.Defines
import io.shiftleft.codepropertygraph.generated.edges.Ref
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators, nodes}
import io.shiftleft.semanticcpg.language.*
import overflowdb.traversal.{jIteratortoTraversal, toNodeTraversal}

import java.io.File
class ASTCreationForMethodCallTests extends GoCodeToCpgSuite {

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

  "Method call to method with int return from the same project but another package" should {
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |""".stripMargin,
      "go.mod"
    ).moreCode(
      """
        |package fpkg
        |func bar() int{
        |  return 0
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
      x.signature shouldBe "joern.io/sample/fpkg.bar()int"
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

  "Method call to method from builtin packages" should {
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

  // TODO : Unit test with new function
//  package main
//
//  import
//
//  "fmt"
//
//  type Person
//  struct {
//    Name string
//      Age int
//  }
//
//  func main () {
//    // Using the new function to create a pointer to a new zero-initialized Person
//    p := new(Person)
//
//    // You can also initialize the fields directly
//    p.Name = "John"
//    p.Age = 30
//
//    fmt.Println("Name:", p.Name)
//    fmt.Println("Age:", p.Age)
//  }

}
