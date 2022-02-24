package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
import io.shiftleft.semanticcpg.language._

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class InheritanceTests extends AnyFreeSpec with Matchers {
  "CPG for code with simple inheritance situation" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |open class Bar {
        |    open fun doX() { print("bar.do") }
        |}
        |
        |class Foo : Bar {
        |    constructor() : super()
        |    override fun doX() {
        |      super.doX()
        |      print("foo.do")
        |    }
        |}
        |
        |fun main() {
        |    val foo = Foo()
        |    foo.doX()
        |}
        |""".stripMargin)

    "should contain a CALL node for `super.doX()` with the correct MFN set" in {
      cpg.call.code("super.doX.*").methodFullName.l shouldBe List("mypkg.Bar.doX:void()")
    }

    // TODO: add more tests
  }
}
