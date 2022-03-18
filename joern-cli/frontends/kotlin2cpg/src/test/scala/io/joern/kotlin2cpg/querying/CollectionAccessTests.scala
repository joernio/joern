package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kotlin2CpgTestContext
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class CollectionAccessTests extends AnyFreeSpec with Matchers {

  "CPG for code with simple list access" - {
    lazy val cpg = Kotlin2CpgTestContext.buildCpg("""
        |import kotlin.collections.mutableListOf
        |
        |fun main(args : Array<String>) {
        |  val numbers = mutableListOf(1, 2, 3, 4)
        |  numbers[0] = 0
        |  println(numbers)
        |}
        |""".stripMargin)

    "should contain at least one CALL node" in {
      cpg.call.size should not be 0
    }

    "should contain a CALL to `indexAccess`" in {
      cpg.call.methodFullNameExact(Operators.indexAccess).size should not be 0
    }
  }

  "CPG for code with simple map access" - {
    lazy val cpg = Kotlin2CpgTestContext.buildCpg("""
        |import kotlin.collections.mutableMapOf
        |
        |fun main(args : Array<String>) {
        |  val numbersMap = mutableMapOf("one" to 1, "two" to 2)
        |  numbersMap["one"] = 11
        |  println(numbersMap)
        |}
        |""".stripMargin)

    "should contain correct number of calls" in {
      cpg.call.size should not be 0
    }

    // TODO: handle this case
  }
}
