package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.TestContext
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class WhenExpressionTests extends AnyFreeSpec with Matchers {
  "CPG for code with simple `when`-expression" - {
    lazy val cpg = TestContext.buildCpg("""
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
