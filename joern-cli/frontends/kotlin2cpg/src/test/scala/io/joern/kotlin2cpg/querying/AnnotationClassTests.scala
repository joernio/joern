package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.TestContext
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class AnnotationClassTests extends AnyFreeSpec with Matchers {

  "CPG for code with simple annotation class" - {
    lazy val cpg = TestContext.buildCpg("""
        |package mypkg
        |
        |fun doSomething(p: String): Boolean {
        |   return if (p == "true") true else false
        |}
        |
        |fun main() {
        |    val aMessage = "true"
        |    println(doSomething(aMessage))
        |}
        |
        |""".stripMargin)

    "should XXXX" in {
      val List(c) = cpg.call.code("doSomething.*").l
      c.methodFullName shouldBe "mypkg.doSomething:boolean(java.lang.String)"

      val List(m) = cpg.method.fullName("mypkg.doSomething.*").l
      m.fullName shouldBe "mypkg.doSomething:boolean(java.lang.String)"
    }
  }
}
