package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kotlin2CpgTestContext
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ByTests extends AnyFreeSpec with Matchers {

  implicit val resolver = NoResolve

  "CPG for code with call to lazy block defined using the _by_ keyword" - {
    lazy val cpg = Kotlin2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |fun foo() {
        | val bar: Int by lazy {
        |   val baz = 1 * 2
        |   println(baz)
        |   1 + 2
        | }
        | println(bar)
        |}
        |
        |""".stripMargin)

    "should contain a CALL node for the call to the multiplication op inside the _by_ block" in {
      cpg.call(Operators.multiplication).size shouldBe 1
    }
  }
}
