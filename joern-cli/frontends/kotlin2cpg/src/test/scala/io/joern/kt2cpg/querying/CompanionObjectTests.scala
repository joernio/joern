package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
import io.shiftleft.semanticcpg.language._

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class CompanionObjectTests extends AnyFreeSpec with Matchers {

  lazy val cpg = Kt2CpgTestContext.buildCpg("""
      | class MyClass {
      |   companion object Factory {
      |       fun create(): MyClass = MyClass()
      |   }
      |}
      |
      |fun main(args : Array<String>) {
      |  println(MyClass.create())
      |}
      |""".stripMargin)

  "should contain correct number of calls" in {
    cpg.call.size should not be 0
  }

  // TODO: fill out the actual test cases
}
