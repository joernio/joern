package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*

class CollectionAccessTests extends KotlinCode2CpgFixture(withOssDataflow = false) {

  "CPG for code with simple list access" should {
    val cpg = code("""
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

  "CPG for code with simple map access" should {
    val cpg = code("""
        |import kotlin.collections.mutableMapOf
        |
        |fun main(args : Array<String>) {
        |  val numbersMap = mutableMapOf("one" to 1, "two" to 2)
        |  numbersMap["one"] = 11
        |  println(numbersMap)
        |}
        |""".stripMargin)

    "should contain a CALL to `indexAccess`" in {
      cpg.call.methodFullNameExact(Operators.indexAccess).size should not be 0
    }
  }
}
