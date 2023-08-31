package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.joern.gosrc2cpg.astcreation.Defines
import io.shiftleft.codepropertygraph.generated.{EvaluationStrategies, NodeTypes}
import io.shiftleft.semanticcpg.language.*

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
  }
}
