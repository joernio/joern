package io.joern.pysrc2cpg.passes

import io.joern.pysrc2cpg.PySrc2CpgFixture
import io.shiftleft.semanticcpg.language.*

import java.io.File

class InheritanceFullNamePassTests extends PySrc2CpgFixture(withOssDataflow = false) {

  "inherited type full names" should {
    lazy val cpg = code(
      """
        |class Foo():
        |  pass
        |""".stripMargin,
      "foo.py"
    ).moreCode(
      """
        |from foo import Foo
        |
        |class Bar(Foo):
        | pass
        |""".stripMargin,
      "bar.py"
    )

    "resolve the type being inherited fully" in {
      def bar = cpg.typeDecl("Bar")
      bar.inheritsFromTypeFullName.l shouldBe Seq("foo.py:<module>.Foo")
      bar.baseType.fullName.l shouldBe Seq("foo.py:<module>.Foo")
    }
  }

  "inherited external types" should {
    lazy val cpg = code("""
        |from tortoise.models import Model
        |import tortoise.models as models
        |
        |import foo
        |
        |class User(Model):
        |  pass
        |
        |class CoolUser(models.Cool):
        |  pass
        |
        |class Foo(foo.Bar):
        |  pass
        |""".stripMargin)

    "resolve the type to a type stub from a fully qualified path" in {
      def user = cpg.typeDecl("User")
      user.inheritsFromTypeFullName.l shouldBe Seq(Seq("tortoise", "models.py:<module>.Model").mkString(File.separator))
      // TODO: Empty for now, would require a stub
      user.baseType.fullName.l shouldBe Seq()
    }

    "resolve the type to a type stub from a partially qualified path using an alias" in {
      def user = cpg.typeDecl("CoolUser")

      user.inheritsFromTypeFullName.l shouldBe Seq(Seq("tortoise", "models.py:<module>.Cool").mkString(File.separator))
      // TODO: Empty for now, would require a stub
      user.baseType.fullName.l shouldBe Seq()
    }

    "resolve the type to a type stub from a shorter qualified path that is extended" in {
      def foo = cpg.typeDecl("Foo")

      foo.inheritsFromTypeFullName.l shouldBe Seq("foo.py:<module>.Bar")
      // TODO: Empty for now, would require a stub
      foo.baseType.fullName.l shouldBe Seq()
    }
  }

}
