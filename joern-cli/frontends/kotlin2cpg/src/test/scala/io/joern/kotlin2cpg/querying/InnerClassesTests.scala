package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language._

class InnerClassesTests extends KotlinCode2CpgFixture(withOssDataflow = false) {

  implicit val resolver: ICallResolver = NoResolve

  "CPG for code with a simple inner class declaration" should {
    val cpg = code("""
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

    "should contain inner class name `Outer$Inner`" in {
      cpg.typ.fullNameExact("Outer$Inner").l.size shouldBe 1
    }
  }
}
