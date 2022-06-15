package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language._
import org.scalatest.Ignore

@Ignore
class InnerClassesTests extends KotlinCode2CpgFixture(withOssDataflow = false) {

  implicit val resolver = NoResolve

  "CPG for code with a simple inner class definition" should {
    lazy val cpg = code("""
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
