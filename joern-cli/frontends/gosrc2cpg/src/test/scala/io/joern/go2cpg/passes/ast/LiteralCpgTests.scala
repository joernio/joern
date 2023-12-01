package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*

import scala.util.Random

class LiteralCpgTests extends GoCodeToCpgSuite {
  "be correct for literal code having less than 1000 characters" in {
    val cpg = code(s"""
         |package main
         |func foo() {
         | var value = "${Random.alphanumeric.take(500).mkString}"
         | }
         |""".stripMargin)

    val List(callNode) = cpg.call.l
    callNode.methodFullName shouldBe Operators.assignment
    callNode.typeFullName shouldBe "string"

    val List(literalNode) = cpg.literal.l
    literalNode.typeFullName shouldBe "string"
    literalNode.code.size shouldBe 502
    literalNode.code.endsWith("...") shouldBe false
  }

  "be correct for literal code having more than 1000 characters" in {
    val cpg = code(s"""
           |package main
           |func foo() {
           | var value = "${Random.alphanumeric.take(1001).mkString}"
           | }
           |""".stripMargin)

    val List(callNode) = cpg.call.l
    callNode.methodFullName shouldBe Operators.assignment
    callNode.typeFullName shouldBe "string"

    val List(literalNode) = cpg.literal.l
    literalNode.typeFullName shouldBe "string"
    literalNode.code.size shouldBe 1000
    literalNode.code.endsWith("...") shouldBe true
  }
}
