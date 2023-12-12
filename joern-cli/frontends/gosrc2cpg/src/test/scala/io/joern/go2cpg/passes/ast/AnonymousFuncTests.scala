package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.joern.x2cpg.Defines
import io.shiftleft.semanticcpg.language.*

import java.io.File
import scala.collection.immutable.List

class AnonymousFuncTests extends GoCodeToCpgSuite {

  "Simple Lambda expression" should {
    val cpg = code("""
        |package main
        |
        |import "fmt"
        |
        |func main() {
        |	// Define a lambda function and assign it to a variable
        |	add := func(a, b int) int {
        |		return a + b
        |	}
        |
        |	// Call the lambda function
        |	result := add(3, 5)
        |	fmt.Println("Result:", result) // Output: 8
        |}
        |""".stripMargin)
    "have proper methodRef node created along with its properties" in {
      cpg.methodRef.l.size shouldBe 1
      val List(mr) = cpg.methodRef.l
      mr.methodFullName shouldBe s"main.main.${Defines.ClosurePrefix}0"
      mr.typeFullName shouldBe s"main.main.${Defines.ClosurePrefix}0"
    }

    "able to traverse to wrapping Method Node from methodRef and have expected properties" in {
      cpg.methodRef.method.fullName.l shouldBe List(s"main.main")
    }

    "have proper Method node created along with its properties" in {
      cpg.method.isLambda.l.size shouldBe 1
      val List(m) = cpg.method.isLambda.l
      m.fullName shouldBe s"main.main.${Defines.ClosurePrefix}0"
      m.signature shouldBe s"${Defines.ClosurePrefix}(int, int)int"
    }

    "able to traverse to referenced Method node" in {
      cpg.methodRef.referencedMethod.fullName.l shouldBe List(s"main.main.${Defines.ClosurePrefix}0")
    }

    "reflects into lhs side TypeFullName" in {
      cpg.local("add").typeFullName.l shouldBe List(s"main.main.${Defines.ClosurePrefix}0")
    }

    "have call node created for lambda invocation" in {
      cpg.call("add").methodFullName.l shouldBe List(s"main.main.${Defines.ClosurePrefix}0")
    }

    "able to traverse from call node to callee" in {
      cpg.call("add").callee.fullName.l shouldBe List(s"main.main.${Defines.ClosurePrefix}0")
    }

    "have TypeDecl created for respective lambda" in {
      cpg.typeDecl(s"${Defines.ClosurePrefix}0").fullName.l shouldBe List(s"main.main.${Defines.ClosurePrefix}0")
    }

    "traversal from TypeDecl to lambda method" in {
      cpg.typeDecl(s"${Defines.ClosurePrefix}0").method.fullName.l shouldBe List(s"main.main.${Defines.ClosurePrefix}0")
    }
  }

  "Simple Lambda expression defined in package" should {
    val cpg = code("""
        |package main
        |
        |import "fmt"
        |
        |// Define a lambda function and assign it to a variable
        |var add = func(a, b int) int {
        |   return a + b
        |}
        |
        |func main() {
        |
        |	// Call the lambda function
        |	result := add(3, 5)
        |	fmt.Println("Result:", result) // Output: 8
        |}
        |""".stripMargin)

    "have proper methodRef node created along with its properties" in {
      cpg.methodRef.l.size shouldBe 1
      val List(mr) = cpg.methodRef.l
      mr.methodFullName shouldBe s"main.${Defines.ClosurePrefix}0"
      mr.typeFullName shouldBe s"main.${Defines.ClosurePrefix}0"
    }

    "have proper Method node created along with its properties" in {
      cpg.method.isLambda.l.size shouldBe 1
      val List(m) = cpg.method.isLambda.l
      m.fullName shouldBe s"main.${Defines.ClosurePrefix}0"
      m.signature shouldBe s"${Defines.ClosurePrefix}(int, int)int"
    }

    "able to traverse to referenced Method node" in {
      cpg.methodRef.referencedMethod.fullName.l shouldBe List(s"main.${Defines.ClosurePrefix}0")
    }

    "reflects into lhs side TypeFullName" in {
      cpg.member("add").typeFullName.l shouldBe List(s"main.${Defines.ClosurePrefix}0")
    }

    "have call node created for lambda invocation" in {
      cpg.call("add").methodFullName.l shouldBe List(s"main.${Defines.ClosurePrefix}0")
    }

    "able to traverse from call node to callee" in {
      cpg.call("add").callee.fullName.l shouldBe List(s"main.${Defines.ClosurePrefix}0")
    }

    "have TypeDecl created for respective lambda" in {
      cpg.typeDecl(s"${Defines.ClosurePrefix}0").fullName.l shouldBe List(s"main.${Defines.ClosurePrefix}0")
    }

    "traversal from TypeDecl to lambda method" in {
      cpg.typeDecl(s"${Defines.ClosurePrefix}0").method.fullName.l shouldBe List(s"main.${Defines.ClosurePrefix}0")
    }
  }

  "Simple Lambda expression defined in package in different file" should {
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |""".stripMargin,
      "go.mod"
    ).moreCode(
      """
        |package lib
        |// Define a lambda function and assign it to a variable
        |var Add = func(a, b int) int {
        |   return a + b
        |}
        |
        |""".stripMargin,
      Seq("lib", "lib.go").mkString(File.separator)
    ).moreCode("""
        |package main
        |import "joern.io/sample/lib"
        |func main() {
        |	// Call the lambda function
        |	result := lib.Add(3, 5)
        |	fmt.Println("Result:", result) // Output: 8
        |}
        |""".stripMargin)

    "have proper methodRef node created along with its properties" in {
      cpg.methodRef.l.size shouldBe 1
      val List(mr) = cpg.methodRef.l
      mr.methodFullName shouldBe s"joern.io/sample/lib.${Defines.ClosurePrefix}0"
      mr.typeFullName shouldBe s"joern.io/sample/lib.${Defines.ClosurePrefix}0"
    }

    "have proper Method node created along with its properties" in {
      cpg.method.isLambda.l.size shouldBe 1
      val List(m) = cpg.method.isLambda.l
      m.fullName shouldBe s"joern.io/sample/lib.${Defines.ClosurePrefix}0"
      m.signature shouldBe s"${Defines.ClosurePrefix}(int, int)int"
    }

    "able to traverse to referenced Method node" in {
      cpg.methodRef.referencedMethod.fullName.l shouldBe List(s"joern.io/sample/lib.${Defines.ClosurePrefix}0")
    }

    "reflects into lhs side TypeFullName" in {
      cpg.member("Add").typeFullName.l shouldBe List(s"joern.io/sample/lib.${Defines.ClosurePrefix}0")
    }

    "have call node created for lambda invocation" in {
      cpg.call("Add").methodFullName.l shouldBe List(s"joern.io/sample/lib.${Defines.ClosurePrefix}0")
    }

    "able to traverse from call node to callee" in {
      cpg.call("Add").callee.fullName.l shouldBe List(s"joern.io/sample/lib.${Defines.ClosurePrefix}0")
    }

    "have TypeDecl created for respective lambda" in {
      cpg.typeDecl(s"${Defines.ClosurePrefix}0").fullName.l shouldBe List(
        s"joern.io/sample/lib.${Defines.ClosurePrefix}0"
      )
    }

    "traversal from TypeDecl to lambda method" in {
      cpg.typeDecl(s"${Defines.ClosurePrefix}0").method.fullName.l shouldBe List(
        s"joern.io/sample/lib.${Defines.ClosurePrefix}0"
      )
    }
  }

  "Lambda Type example" should {
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |""".stripMargin,
      "go.mod"
    ).moreCode(
      """
        |package fpkg
        |type Sample func(int, int) int
        |
        |""".stripMargin,
      Seq("lib", "lib.go").mkString(File.separator)
    ).moreCode(
      """
        |package libtwo
        |type SampleTwo func(int, int) int
        |
        |type SampleThree func(int) string
        |""".stripMargin,
      Seq("libtwo", "libtwo.go").mkString(File.separator)
    ).moreCode(
      """
        |package main
        |
        |import "fmt"
        |
        |func main() {
        |	// Define a lambda function and assign it to a variable
        |	add := func(a, b int) int {
        |		return a + b
        |	}
        |
        |	// Call the lambda function
        |	result := add(3, 5)
        |	fmt.Println("Result:", result) // Output: 8
        |}
        |""".stripMargin,
      "main.go"
    )

    "create TypeDecls for lambada types defined in another packages" in {
      cpg.typeDecl.fullName.l shouldBe List(
        "joern.io/sample/lib",
        "joern.io/sample/libtwo",
        "main",
        "joern.io/sample/lib.Sample",
        "joern.io/sample/libtwo.SampleTwo",
        "joern.io/sample/libtwo.SampleThree",
        "main.main.<lambda>0"
      )
    }

    "map lambda TypeDecl to matching with the signature" in {
      val inheritedFrom = cpg.typeDecl(s"${Defines.ClosurePrefix}0").inheritsFromTypeFullName.l
      inheritedFrom contains "joern.io/sample/lib.Sample"
      inheritedFrom contains "joern.io/sample/libtwo.SampleTwo"
    }
  }
}
