package io.joern.kotlin2cpg.validation

import io.joern.kotlin2cpg.Kotlin2CpgTestContext
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.edges.Ast
import io.shiftleft.codepropertygraph.generated.nodes.{ClosureBinding, FieldIdentifier, Identifier}
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.jIteratortoTraversal

class DispatchTypeValidationTests extends AnyFreeSpec with Matchers {
  // TODO: add inputs as arrays

  "CPG for code with simple qualified-expression" - {
    lazy val cpg = Kotlin2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |import java.lang.Runtime
        |
        |fun main() {
        |   Runtime.getRuntime().exec("ls -al")
        |}
        """.stripMargin)

    "should contain the correct DISPATCH_TYPE value" in {
      val List(c) = cpg.call.methodFullNameExact("java.lang.Runtime.exec:java.lang.Process(java.lang.String)").l
      c.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
    }
  }

  // TODO: add example with call to `super`
}
