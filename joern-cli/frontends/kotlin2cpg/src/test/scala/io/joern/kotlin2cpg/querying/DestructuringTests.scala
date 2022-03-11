package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
import io.shiftleft.semanticcpg.language._

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class DestructuringTests extends AnyFreeSpec with Matchers {

  implicit val resolver = NoResolve

  "CPG for code with destructuring declaration and identifier RHS" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package main
        |
        |data class AClass(val a: String, val b: Int)
        |fun main() {
        |    val aClass = AClass("AMESSAGE", 41414141)
        |    val (myA, myB) = aClass
        |    println(myA)
        |    println(myB)
        |// prints
        |//```
        |//AMESSAGE
        |//41414141
        |//```
        |}
        |""".stripMargin)

    // TODO: add the test cases
  }

  "CPG for code with destructuring expression with a ctor-invocation RHS" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg(
      """
        |package main
        |
        |data class AClass(val a: String, val b: Int)
        |fun main() {
        |    val (myA, myB) = AClass("AMESSAGE", 41414141)
        |    println(myA)
        |    println(myB)
        |// prints
        |//```
        |//AMESSAGE
        |//41414141
        |//```
        |}
        |""".stripMargin)

    // TODO: add the test cases
  }
}
