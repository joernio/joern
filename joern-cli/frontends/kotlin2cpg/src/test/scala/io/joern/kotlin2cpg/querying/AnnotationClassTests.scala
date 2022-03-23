package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kotlin2CpgTestContext
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class AnnotationClassTests extends AnyFreeSpec with Matchers {

  "CPG for code with simple annotation class" - {
    lazy val cpg = Kotlin2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |annotation class Special(val why: String)
        |
        |@Special("reason")
        |fun foo() {
        | println(1)
        |}
        |""".stripMargin)

    "should contain a TYPE_DECL node with the correct properties set" in {
      val List(a) = cpg.typeDecl("Special").l
      a.fullName shouldBe "mypkg.Special"
      a.code shouldBe "Special"
      a.lineNumber shouldBe Some(3)
      a.columnNumber shouldBe Some(17)

      // TODO: test constructor
      // TODO: test members
    }
  }
}
