package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.edges.Argument
import io.shiftleft.semanticcpg.language.*
import overflowdb.traversal.jIteratortoTraversal

class ComplexExpressionsTests extends KotlinCode2CpgFixture(withOssDataflow = false) {
  "CPG for code with _and_/_or_ operator and try-catch as one of the arguments" should {
    val cpg = code("""
        |package mypkg
        |
        |import kotlin.random.Random
        |
        |fun main() {
        |    val foo = isSomething && try {
        |        val r = Random.nextInt(0, 100)
        |        r < 50
        |    } catch(e: Exception) {
        |        false
        |    }
        |    println(foo)
        |
        |    val bar = isSomething || try {
        |        val r = Random.nextInt(0, 100)
        |        r < 50
        |    } catch(e: Exception) {
        |        false
        |    }
        |    println(bar)
        |}
        |""".stripMargin)

    "should contain a CALL node for the _and_-expression with arguments set" in {
      cpg.call.methodFullName(Operators.logicalAnd).size shouldBe 1

      cpg.call
        .methodFullName(Operators.logicalAnd)
        .filter(!_.outE.exists { x => x.isInstanceOf[Argument] })
        .code
        .l shouldBe Seq()
    }

    "should contain a CALL node for the _or_-expression with arguments set" in {
      cpg.call.methodFullName(Operators.logicalOr).size shouldBe 1

      cpg.call
        .methodFullName(Operators.logicalOr)
        .filter(!_.outE.exists { x => x.isInstanceOf[Argument] })
        .code
        .l shouldBe Seq()
    }
  }

  "CPG for code with _and_ operator and let inside it" should {
    val cpg = code("""
        |package mypkg
        |
        |class Config {
        |    companion object {
        |        val minQueryLength get() = 10
        |    }
        |}
        |
        |
        |fun main() {
        |    val markImmediately = true
        |    val aString = "PLACEHOLDER"
        |    val res =
        |        !markImmediately &&
        |                aString.let {
        |                    it.length < Config.minQueryLength &&
        |                            it.all(Char::isLetterOrDigit)
        |                }
        |    println(res)
        |}
        |""".stripMargin)

    // test case triggered by a crash in CfgCreator for https://github.com/acejump/AceJump
    // the following query gives back results:
    //    cpg.call.methodFullName(".*operator.*[Aa]nd.*").filter(_.outE.filter{x => x.isInstanceOf[Argument]}.size == 0).l
    "should contain a CALL node for the main expression with arguments set" in {
      cpg.call.methodFullName(Operators.logicalAnd).size shouldBe 2

      cpg.call
        .methodFullName(Operators.logicalAnd)
        .filter(!_.outE.exists { x => x.isInstanceOf[Argument] })
        .code
        .l shouldBe Seq()
    }
  }
}
