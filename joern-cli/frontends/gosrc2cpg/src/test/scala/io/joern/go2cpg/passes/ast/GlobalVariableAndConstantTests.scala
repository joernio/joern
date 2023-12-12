package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*

import java.io.File

class GlobalVariableAndConstantTests extends GoCodeToCpgSuite {

  "Global variable declaration check" should {
    val cpg = code("""
        |package main
        |const (
        |	FooConst = "Test"
        |)
        |var (
        |	BarVar = 100
        |)
        |func main() {
        |  println(FooConst)
        |}
        |""".stripMargin)

    "Check package Type Decl" in {
      val List(x) = cpg.typeDecl("main").l
      x.fullName shouldBe "main"
    }

    "Traversal from package type decl to global variable member nodes" in {
      val List(x)    = cpg.typeDecl("main").l
      val List(a, b) = x.member.l
      a.name shouldBe "FooConst"
      a.typeFullName shouldBe "string"
      b.name shouldBe "BarVar"
      b.typeFullName shouldBe "int"
    }

    "Be correct for Field Access CALL Node for Global variable access" in {
      val List(a, b, c) = cpg.call(Operators.fieldAccess).l
      a.lineNumber shouldBe Some(10)
      b.lineNumber shouldBe Some(4)
      c.lineNumber shouldBe Some(7)
    }

    "Create Constructor method for Package level global variable initialisation" in {
      val List(x) = cpg.method(s".*${Defines.StaticInitMethodName}").l
      x.fullName shouldBe s"main${Defines.StaticInitMethodName}"
    }

    "Be correct for Literal nodes " in {
      val List(a, b) = cpg.literal.l
      a.code shouldBe "\"Test\""
      b.code shouldBe "100"
    }
  }

  "Var defined(with type mentioned) in one package used in another package" should {
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |""".stripMargin,
      "go.mod"
    ).moreCode(
      """
        |package lib1
        |
        |var(
        |	SchemeHTTP string =  "http"
        |)
        |
        |""".stripMargin,
      Seq("lib1", "typelib.go").mkString(File.separator)
    ).moreCode(
      """
        |package main
        |import "joern.io/sample/lib1"
        |func main() {
        |	var a = lib1.SchemeHTTP.value()
        |}
        |""".stripMargin,
      "main.go"
    )

    "Be correct for Field Access CALL Node for Global variable access" in {
      val List(x, y) = cpg.call(Operators.fieldAccess).l
      x.code shouldBe "lib1.SchemeHTTP"
      y.code shouldBe "SchemeHTTP"
    }

    "Check methodfullname of variable imported from other package " in {
      val List(callNode) = cpg.call("value").l
      callNode.methodFullName shouldBe "string.value"
    }
  }

  "Var defined(without type mentioned) in one package used in another package" should {
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |""".stripMargin,
      "go.mod"
    ).moreCode(
      """
        |package lib1
        |
        |var(
        |	SchemeHTTP =  "http"
        |)
        |
        |""".stripMargin,
      Seq("lib1", "typelib.go").mkString(File.separator)
    ).moreCode(
      """
        |package main
        |import "joern.io/sample/lib1"
        |func main() {
        |	var a = lib1.SchemeHTTP.value()
        |}
        |""".stripMargin,
      "main.go"
    )

    "Be correct for Field Access CALL Node for Global variable access" in {
      val List(x, y) = cpg.call(Operators.fieldAccess).l
      x.code shouldBe "lib1.SchemeHTTP"
      y.code shouldBe "SchemeHTTP"
    }

    "Check methodfullname of variable imported from other package " in {
      val List(callNode) = cpg.call("value").l
      callNode.methodFullName shouldBe "string.value"
    }
  }
  "Const defined(with type mentioned) in one package used in another package" should {
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |""".stripMargin,
      "go.mod"
    ).moreCode(
      """
        |package lib1
        |
        |const (
        |	SchemeHTTP string =  "http"
        |)
        |
        |""".stripMargin,
      Seq("lib1", "typelib.go").mkString(File.separator)
    ).moreCode(
      """
        |package main
        |import "joern.io/sample/lib1"
        |func main() {
        |	var a = lib1.SchemeHTTP.value()
        |}
        |""".stripMargin,
      "main.go"
    )

    "Be correct for Field Access CALL Node for Global variable access" in {
      val List(x, y) = cpg.call(Operators.fieldAccess).l
      x.code shouldBe "lib1.SchemeHTTP"
      y.code shouldBe "SchemeHTTP"
    }

    "Check methodfullname of constant imported from other package " in {
      val List(callNode) = cpg.call("value").l
      callNode.methodFullName shouldBe "string.value"
    }
  }

  "const defined(without type mentioned) in one package used in another package" should {
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |""".stripMargin,
      "go.mod"
    ).moreCode(
      """
        |package lib1
        |
        |const (
        |	SchemeHTTP =  "http"
        |)
        |
        |""".stripMargin,
      Seq("lib1", "typelib.go").mkString(File.separator)
    ).moreCode(
      """
        |package main
        |import "joern.io/sample/lib1"
        |import "joern.io/sample/lib2"
        |func main() {
        |	var a = lib1.SchemeHTTP.value()
        |   var b = lib2.SchemeHTTP.value()
        |}
        |""".stripMargin,
      "main.go"
    )

    "Be correct for Field Access CALL Node for Global variable access" in {
      val List(a, b, c) = cpg.call(Operators.fieldAccess).l
      a.typeFullName shouldBe "string"
      b.typeFullName shouldBe "joern.io/sample/lib2.SchemeHTTP.<FieldAccess>.<unknown>"
      c.code shouldBe "SchemeHTTP"
    }

    "Check methodfullname of constant imported from other package " in {
      val List(callNode, callNode2) = cpg.call("value").l
      callNode.methodFullName shouldBe "string.value"
      callNode2.methodFullName shouldBe "joern.io/sample/lib2.SchemeHTTP.<FieldAccess>.<unknown>.value"
    }
  }

  "global variable reference" should {
    val cpg = code("""package main
        |var x = 1
        |func main(){
        | y := x
        |}
        |""".stripMargin)

    "check Global Member node" in {
      val List(x) = cpg.typeDecl("main").l
      val List(a) = x.member.l
      a.name shouldBe "x"
      a.typeFullName shouldBe "int"
    }

    "test local variable exists" in {
      val List(localNode) = cpg.local.l
      localNode.name shouldBe "y"
      localNode.typeFullName shouldBe "int"
    }
  }

  "when constant is used in initializing struct" should {
    val cpg = code("""
        |package main
        |
        |var person = Person()
        |
        |type Name struct {
        |   name string
        |}
        |
        |const (
        | personName string = "peter"
        |)
        |
        |func Person() Name {
        |   return Name{
        |     name: personName,
        |   }
        |}
        |""".stripMargin)

    "test basic ast structure for Person" in {
      val List(method) = cpg.method.name("Person").l
      method.signature shouldBe "main.Person()main.Name"

      val List(typeDeclNode) = cpg.typeDecl.name("Name").l
      typeDeclNode.fullName shouldBe "main.Name"
      typeDeclNode.member.size shouldBe 1
      val List(name) = typeDeclNode.member.l
      name.code shouldBe "name"
      name.typeFullName shouldBe "string"
    }

    "check Global Member node" in {
      val List(x)    = cpg.typeDecl("main").l
      val List(a, b) = x.member.l
      a.name shouldBe "person"
      a.typeFullName shouldBe "main.Name"
      b.name shouldBe "personName"
      b.typeFullName shouldBe "string"
    }

    "Check fieldAccess node for global variable access" in {
      val List(a, b, c) = cpg.call(Operators.fieldAccess).l
      a.typeFullName shouldBe "string"
      b.typeFullName shouldBe "main.Name"
      b.code shouldBe "person"
      c.typeFullName shouldBe "string"
      c.code shouldBe "personName"
    }
  }
}
