package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.{EvaluationStrategies, NodeTypes}
import io.shiftleft.semanticcpg.language.*
import io.joern.x2cpg.Defines

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

    "reflectes into lhs side TypeFullName" in {
      cpg.local("add").typeFullName.l shouldBe List(s"main.main.${Defines.ClosurePrefix}0")
    }
  }
}
