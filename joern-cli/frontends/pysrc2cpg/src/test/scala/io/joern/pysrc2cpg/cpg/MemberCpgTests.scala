package io.joern.pysrc2cpg.cpg

import io.joern.pysrc2cpg.Py2CpgTestContext
import io.shiftleft.codepropertygraph.generated.nodes.Member
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import io.shiftleft.semanticcpg.language._

class MemberCpgTests extends AnyFreeSpec with Matchers {

  "A class variable" - {

    lazy val cpg = Py2CpgTestContext.buildCpg("""
                                                |class Foo():
                                                |   x = 'foo'
                                                |
                                                |""".stripMargin)

    "should have a MEMBER" in {
      val List(member: Member) = cpg.member.name("x").l
      member.code shouldBe "x"
    }

    "should have the MEMBER attached to the meta class" in {
      val List(typeDecl) = cpg.member.name("x").typeDecl.l
      typeDecl.name shouldBe "Foo<meta>"
    }
  }

  "An instance variable" - {

    lazy val cpg = Py2CpgTestContext.buildCpg("""
                                                |class Foo():
                                                |   def __init__(self):
                                                |      self.x = 'foo'
                                                |""".stripMargin)

    "should have a MEMBER" in {
      val List(member: Member) = cpg.member.name("x").l
      member.code shouldBe "x"
    }

    "should have the MEMBER attached to the meta class" in {
      val List(typeDecl) = cpg.member.name("x").typeDecl.l
      typeDecl.name shouldBe "Foo<meta>"
    }

  }

}
