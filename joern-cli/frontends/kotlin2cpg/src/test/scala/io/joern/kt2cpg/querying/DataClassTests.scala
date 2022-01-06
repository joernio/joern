package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
import io.shiftleft.semanticcpg.language._

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class DataClassTests extends AnyFreeSpec with Matchers {

  lazy val cpg = Kt2CpgTestContext.buildCpg("""
      |package mypkg
      |
      |data class Result(val p: Int, val q: Int)
      |
      |data class Account(val username: String, val password: String) {
      |    var permission: Int = -1
      |
      |    constructor(perm: Int) : this("username", "password"){
      |        permission = perm
      |    }
      |}
      |
      |fun main(args : Array<String>) {
      |  val x = Result(1, 0)
      |  println(x.p)
      |  println(x.q)
      |}
      |""".stripMargin)

  "should contain correct number of calls" in {
    cpg.call.size should not be 0
  }

  // TODO: fill out the actual test cases
}
