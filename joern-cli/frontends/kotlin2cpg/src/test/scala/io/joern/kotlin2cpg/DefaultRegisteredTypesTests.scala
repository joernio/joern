package io.joern.kotlin2cpg

import io.joern.kotlin2cpg.TestContext
import io.shiftleft.semanticcpg.language._
import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class DefaultRegisteredTypesTests extends AnyFreeSpec with Matchers with BeforeAndAfterAll {
  "CPG for code with simple user-defined class" - {
    lazy val cpg = TestContext.buildCpg("""
        |package mypkg
        |
        |class AClass
        |""".stripMargin)

    "should contain a TYPE node for `java.lang.Object`" in {
      cpg.typ.fullNameExact("java.lang.Object").size shouldBe 1
    }
  }
}
