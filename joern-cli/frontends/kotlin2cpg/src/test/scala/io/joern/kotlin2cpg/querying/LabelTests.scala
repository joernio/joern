package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
import io.shiftleft.semanticcpg.language._

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class LabelTests extends AnyFreeSpec with Matchers {

  implicit val resolver = NoResolve

  "CPG for code with label usage" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |fun foo(x: Int, y: Int): Int {
        |    listOf(1, 2, 3, 4, 5).forEach {
        |        if (Random.nextInt(0, 1) == 0) return x // non-local return directly to the caller of foo()
        |        print(it)
        |    }
        |    return 0
        |}
        |
        |fun bar(x: Int, y: Int): Int {
        |    listOf(1, 2, 3, 4, 5).forEach lit@{
        |        if (Random.nextInt(0, 1) == 0) {
        |          return x
        |        } else {
        |          return @lit
        |        }
        |        print(it)
        |    }
        |    return 0
        |}
        |
        |fun baz(x: Int, y: Int): Int {
        |    listOf(1, 2, 3, 4, 5).forEach {
        |        if (Random.nextInt(0, 1) == 0) {
        |          return x
        |        } else {
        |          return@forEach // local return to the caller of the lambda - the forEach loop
        |        }
        |        print(it)
        |    }
        |    return 0
        |}
        |
        |fun bap(x: Int, y: Int): Int {
        |    listOf(1, 2, 3, 4, 5).forEach(fun(value: Int) {
        |        if (Random.nextInt(0, 1) == 0) return  // local return to the caller of the anonymous function - the forEach loop
        |        print(value)
        |    })
        |    print(" done with anonymous function")
        |    return 0
        |}
        |
        |""".stripMargin)

  // TODO: add the test cases
  }
}
