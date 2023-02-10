package io.joern.pysrc2cpg.cpg

import io.joern.pysrc2cpg.Py2CpgTestContext
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import io.shiftleft.semanticcpg.language._

class MemberCpgTests extends AnyFreeSpec with Matchers {

  lazy val cpg = Py2CpgTestContext.buildCpg("""
      |class Foo():
      |   x = 'foo'
      |
      |""".stripMargin)

  "member should exist for class variable" in {
    val List(member) = cpg.member.name("x").l
    member.typeDecl.name shouldBe "Foo<meta>"
  }

  "member should be attached to meta class" in {
    val List(typeDecl) = cpg.member.name("x").typeDecl.l
    typeDecl.name shouldBe "Foo<meta>"
  }
}
