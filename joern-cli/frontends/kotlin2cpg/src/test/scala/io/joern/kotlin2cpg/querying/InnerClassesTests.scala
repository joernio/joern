package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
import io.shiftleft.semanticcpg.language._

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class InnerClassesTests extends AnyFreeSpec with Matchers {

  implicit val resolver = NoResolve

  "CPG for code with a simple inner class definition" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        | class Outer {
        |     private val bar: Int = 1
        |     inner class Inner {
        |         fun foo() = bar
        |     }
        | }
        |
        |fun main(args : Array<String>) {
        |  val demo = Outer().Inner().foo()
        |  println(demo) // outputs `1`
        |}
        |""".stripMargin)

    // TODO: add the test cases
  }
}
