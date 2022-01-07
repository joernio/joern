package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
import io.shiftleft.semanticcpg.language._

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class DestructuringTests extends AnyFreeSpec with Matchers {

  implicit val resolver = NoResolve

  lazy val cpg = Kt2CpgTestContext.buildCpg("""
      |data class Bar(val p: Int, val q: Int)
      |
      |fun foo(): Int {
      |    val myList = listOf(Foo(1, 2), Foo(3, 4))
      |    var sum = 0
      |    for ((x, y) in myList) {
      |        sum += x
      |        sum += y
      |    }
      |
      |    val (x, y) = Bar(1, 2)
      |    return sum + x + y
      |}
      |
      |""".stripMargin)

  // TODO: add the test cases
}
