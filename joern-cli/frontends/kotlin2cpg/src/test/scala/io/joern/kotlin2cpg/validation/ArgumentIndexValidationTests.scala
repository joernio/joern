package io.joern.kotlin2cpg.validation

import io.joern.kotlin2cpg.Kt2CpgTestContext
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.edges.Ast
import io.shiftleft.codepropertygraph.generated.nodes.{Call, ClosureBinding, FieldIdentifier, Identifier, Literal}
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.jIteratortoTraversal

class ArgumentIndexValidationTests extends AnyFreeSpec with Matchers {
  "CPG for code with simple qualified-expression" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |import java.lang.Runtime
        |
        |fun main() {
        |   Runtime.getRuntime().exec("ls -al")
        |}
        |""".stripMargin)

    "should contain the correct argumentIndex values for its arguments" in {
      val List(firstArg: Call, secondArg: Literal) =
        cpg.call.methodFullNameExact("java.lang.Runtime.exec:java.lang.Process(java.lang.String)").argument.l
      firstArg.argumentIndex shouldBe 0
      secondArg.argumentIndex shouldBe 1
    }
  }
}
