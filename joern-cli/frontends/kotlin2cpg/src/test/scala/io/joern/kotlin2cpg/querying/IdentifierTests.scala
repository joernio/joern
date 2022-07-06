package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language._

class IdentifierTests extends KotlinCode2CpgFixture(withOssDataflow = false) {
  "CPG for code with two simple methods" should {
    lazy val cpg = code("""
        |fun add(x: Int, y: Int): Int {
        |  return x + y
        |}
        |
        |fun main(args : Array<String>) {
        |  val argc: Int = args.size
        |  println(add(argc, 1))
        |}
        |""".stripMargin)

    "should contain the correct number of IDENTIFIER nodes" in {
      cpg.identifier.size shouldBe 5
    }

    "IDENTIFIER nodes have the correct CODE property set" in {
      cpg.identifier.code.l.toSet shouldBe Set("x", "y", "argc", "args")
    }

    "IDENTIFIER nodes have the correct LINE_NUMBER property set" in {
      cpg.identifier("x").lineNumber.l.head shouldBe 3
      cpg.identifier("y").lineNumber.l.head shouldBe 3
      cpg.identifier("argc").lineNumber.l.head shouldBe 7
      cpg.identifier("args").lineNumber.l.head shouldBe 7
    }

    "IDENTIFIER nodes have the correct COLUMN_NUMBER property set" in {
      cpg.identifier("x").columnNumber.l.head shouldBe 9
      cpg.identifier("y").columnNumber.l.head shouldBe 13
      cpg.identifier("argc").columnNumber.l.head shouldBe 6
      cpg.identifier("args").columnNumber.l.head shouldBe 18
    }
  }
}
