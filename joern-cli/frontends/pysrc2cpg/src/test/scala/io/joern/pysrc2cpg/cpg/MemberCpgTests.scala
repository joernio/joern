package io.joern.pysrc2cpg.cpg

import io.joern.pysrc2cpg.Py2CpgTestContext
import io.shiftleft.codepropertygraph.generated.nodes.Member
import io.shiftleft.semanticcpg.language.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class MemberCpgTests extends AnyFreeSpec with Matchers {

  "A class variable" - {

    lazy val cpg = Py2CpgTestContext.buildCpg("""
                                                |class Foo():
                                                |   x = 'foo'
                                                |
                                                |""".stripMargin)

    "should have the MEMBER attached to the class" in {
      val List(member) = cpg.typeDecl.name("Foo").member.name("x").l
      member.name shouldBe "x"
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

    "should have the MEMBER attached to the class" in {
      val List(typeDecl) = cpg.member.name("x").typeDecl.l
      typeDecl.name shouldBe "Foo"
    }

  }

  "An instance variable with additional MEMBER fn" - {

    lazy val cpg = Py2CpgTestContext.buildCpg("""
        |class Foo():
        |   self.x = 'foo'
        |   def replace(self, x):
        |      self.x = x
        |""".stripMargin)

    "should have a MEMBER" in {
      val List(member: Member) = cpg.member.name("x").l
      member.code shouldBe "x"
    }

    "should have the MEMBER attached to the class" in {
      val List(typeDecl) = cpg.member.name("x").typeDecl.l
      typeDecl.name shouldBe "Foo"
      typeDecl.lineNumber shouldBe Some(2)
      typeDecl.columnNumber shouldBe Some(1)
    }

    "should have the MEMBER fn attached to the meta class and have col+line no" in {
      val List(memberFn) = cpg.typeDecl("Foo").where(node => node.member.name("replace")).l
      memberFn.fullName shouldBe "test.py:<module>.Foo"
      memberFn.lineNumber shouldBe Some(2)
      memberFn.columnNumber shouldBe Some(1)
    }

  }

  "A class variable instantiated by some expression" - {

    lazy val cpg = Py2CpgTestContext.buildCpg("""import models
        |
        |class SocialApp():
        |    member1 = "1"
        |
        |class SocialAccount():
        |    member2 = "2"
        |
        |class SocialToken(models.Model):
        |    app = models.ForeignKey(SocialApp, on_delete=models.CASCADE)
        |    account = models.ForeignKey(SocialAccount, on_delete=models.CASCADE)
        |""".stripMargin)

    "should only render the LHS of the expression as the member and not the RHS" in {
      cpg.typeDecl("SocialToken").member.name.l shouldBe List("app", "account")
    }
  }

}
