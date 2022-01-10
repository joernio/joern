package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
import io.shiftleft.semanticcpg.language._

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ExtensionTests extends AnyFreeSpec with Matchers {

  "CPG for code with simple extension function definitions" - {
    implicit val resolver = NoResolve

    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |class Example {
        |    fun printBar() { println("class.bar") }
        |}
        |
        |fun Example.printBaz() { println("ext.baz") }
        |
        |fun main(args : Array<String>) {
        |  Example().printBaz()
        |}
        |""".stripMargin)

    "should contain a CALL node for the calls to the extension fns with the correct MFN set" in {
      val List(c) = cpg.call.code(".*printBaz.*").l
      c.methodFullName shouldBe "mypkg.Example.printBaz:kotlin.Unit()"
    }

    "should contain a METHOD node for the extension fn with the correct MFN set" in {
      val List(m) = cpg.method.fullName(".*printBaz.*").l
      m.fullName shouldBe "mypkg.Example.printBaz:kotlin.Unit()"
    }
  }
}
