package io.joern.kotlin2cpg.validation

import io.joern.kotlin2cpg.Kt2CpgTestContext
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.edges.Ast
import io.shiftleft.codepropertygraph.generated.nodes.{FieldIdentifier, Identifier}
import io.shiftleft.proto.cpg.Cpg.DispatchTypes
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.jIteratortoTraversal

class ValidationTests extends AnyFreeSpec with Matchers {
  "CPG for code with simple method containing if-expression" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |fun main(argc: Int): Int {
        |   val z: Int = if(argc > 0) argc else 0
        |   return z
        |}
        |""".stripMargin)

    "should not contain IDENTIFIER nodes with more than one incoming AST edge" in {
      cpg.identifier
        .filter(_.inE.filter { e => e.isInstanceOf[Ast] }.size > 1)
        .code
        .l shouldBe List()
    }

    "should not contain any IDENTIFIER nodes without inbound AST edges" in {
      cpg.identifier
        .filter(_._astIn.size == 0)
        .code
        .l shouldBe Seq()
    }
  }

  "CPG for code with simple method containing simple class declaration" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
      |package mypkg
      |
      |class AClass {
      |    fun main(argc: Int): Int {
      |       val z: Int = if(argc > 0) argc else 0
      |       return z
      |    }
      |}
      |""".stripMargin)

    "should not contain IDENTIFIER nodes with more than one incoming AST edge" in {
      cpg.identifier
        .filter(_.inE.filter { e => e.isInstanceOf[Ast] }.size > 1)
        .code
        .l shouldBe List()
    }

    "should not contain any IDENTIFIER nodes without inbound AST edges" in {
      cpg.identifier
        .filter(_._astIn.size == 0)
        .code
        .l shouldBe Seq()
    }
  }
  "CPG for code with simple lazy blocks" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
      |package mypkg
      |
      |import java.nio.file.Files
      |
      |fun main() {
      |    val customDir = Files.createTempDirectory("custom").toFile()
      |    val foo = lazy { customDir }
      |    println(foo)
      |}
      |""".stripMargin)

    "should not contain any identifiers without an ast parent" in {
      cpg.identifier
        .filter(_._astIn.size == 0)
        .code
        .l shouldBe Seq()
    }
  }
}
