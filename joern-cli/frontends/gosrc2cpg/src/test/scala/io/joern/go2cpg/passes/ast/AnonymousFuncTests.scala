package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.joern.x2cpg.Defines
import io.shiftleft.semanticcpg.language.*

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
    // TODO: some of tests are ignore, which will require Struct Type constructor handling for the Package initialisation
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
    "have proper methodRef node created along with its properties" ignore {
      cpg.methodRef.l.size shouldBe 1
      val List(mr) = cpg.methodRef.l
      mr.methodFullName shouldBe s"main.${Defines.ClosurePrefix}0"
      mr.typeFullName shouldBe s"main.${Defines.ClosurePrefix}0"
    }

    "have proper Method node created along with its properties" ignore {
      cpg.method.isLambda.l.size shouldBe 1
      val List(m) = cpg.method.isLambda.l
      m.fullName shouldBe s"main.${Defines.ClosurePrefix}0"
      m.signature shouldBe s"${Defines.ClosurePrefix}(int, int)int"
    }

    "able to traverse to referenced Method node" ignore {
      cpg.methodRef.referencedMethod.fullName.l shouldBe List(s"main.${Defines.ClosurePrefix}0")
    }

    "reflects into lhs side TypeFullName" ignore {
      cpg.local("add").typeFullName.l shouldBe List(s"main.${Defines.ClosurePrefix}0")
    }

    "have call node created for lambda invocation" ignore {
      cpg.call("add").methodFullName.l shouldBe List(s"main.${Defines.ClosurePrefix}0")
    }

    "able to traverse from call node to callee" ignore {
      cpg.call("add").callee.fullName.l shouldBe List(s"main.${Defines.ClosurePrefix}0")
    }

    "have TypeDecl created for respective lambda" in {
      cpg.typeDecl(s"${Defines.ClosurePrefix}0").fullName.l shouldBe List(s"main.${Defines.ClosurePrefix}0")
    }

    "traversal from TypeDecl to lambda method" in {
      cpg.typeDecl(s"${Defines.ClosurePrefix}0").method.fullName.l shouldBe List(s"main.${Defines.ClosurePrefix}0")
    }
  }
}
