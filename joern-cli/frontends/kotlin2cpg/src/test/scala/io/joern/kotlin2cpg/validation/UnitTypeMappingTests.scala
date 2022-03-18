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

class UnitTypeMappingTests extends AnyFreeSpec with Matchers {
  "CPG for code with definion of simple fn returning `kotlin.Unit`" - {
    lazy val cpg = Kotlin2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |fun main() {
        |  println("Hello, world")
        |}
        |""".stripMargin)

    "should contain an METHOD node with `void` in its FULL_NAME prop" in {
      val List(m) = cpg.method.name("main").l
      m.fullName shouldBe "mypkg.main:void()"
    }
  }
}
