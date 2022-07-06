package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture

class WhenExpressionTests extends KotlinCode2CpgFixture(withOssDataflow = false) {
  "CPG for code with simple `when`-expression" should {
    lazy val cpg = code("""
        |package mypkg
        |
        |fun myfun() {
        |  val x =  Random.nextInt(0, 3)
        |  val foo = when (x) {
        |      1 -> 123
        |      2 -> 234
        |      else -> {
        |          456
        |      }
        |  }
        |  println(foo)
        |}
        | """.stripMargin)

    // TODO: add test case
  }

}
