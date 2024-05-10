package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*
import io.joern.rubysrc2cpg.RubySrc2Cpg
import io.joern.rubysrc2cpg.Config
import scala.util.{Success, Failure}
import org.scalatest.Inspectors

class ImportTests extends RubyCode2CpgFixture with Inspectors {

  "`require 'test'` is a CALL node with an IMPORT node pointing to it" in {
    val cpg = code("""
                     |require 'test'
                     |""".stripMargin)
    val List(importNode) = cpg.imports.l
    importNode.importedEntity shouldBe Some("test")
    importNode.importedAs shouldBe Some("test")
    val List(call) = importNode.call.l
    call.callee.name.l shouldBe List("require")
    call.argument.where(_.argumentIndexGt(0)).code.l shouldBe List("'test'")
  }

  "`begin require 'test' rescue LoadError end` has a CALL node with an IMPORT node pointing to it" in {
    val cpg = code("""
                     |begin 
                     |  require 'test'
                     |rescue LoadError
                     |end
                     |""".stripMargin)
    val List(importNode) = cpg.imports.l
    importNode.importedEntity shouldBe Some("test")
    importNode.importedAs shouldBe Some("test")
    val List(call) = importNode.call.l
    call.callee.name.l shouldBe List("require")
    call.argument.where(_.argumentIndexGt(0)).code.l shouldBe List("'test'")
  }

  "Ambiguous class resolves to required method" in {
    forAll(List("t2", "t3")) { path =>
      val cpg = code(
        s"""
      | require '${path}'
      | Test.new
      |""".stripMargin,
        "t1.rb"
      ).moreCode(
        """
      | class Test
      | end
      |""".stripMargin,
        "t2.rb"
      ).moreCode(
        """
      | class Test
      | end
      |""".stripMargin,
        "t3.rb"
      )

      val List(newCall) =
        cpg.method.name(":program").filename("t1.rb").ast.isCall.methodFullName(".*:<init>").methodFullName.l
      newCall should startWith(s"${path}.rb:")
    }
  }

  "Ambiguous methods resolves to included method" in {
    forAll(List("A", "B")) { moduleName =>
      val cpg = code(s"""
      | module A
      |   def foo
      |   end
      | end
      |
      | module B
      |   def foo
      |   end
      | end
      |
      | module C
      |   include ${moduleName}
      |   def bar
      |     foo()
      |   end
      | end
      |""".stripMargin)

      val List(methodName) =
        cpg.method.name("bar").ast.isCall.methodFullName(".*::program\\.(A|B):foo").methodFullName.l
      methodName should endWith(s"${moduleName}:foo")
    }
  }

  "implicitly imported types (common in frameworks like Ruby on Rails)" should {

    val cpg = code(
      """
        |class A
        |end
        |""".stripMargin,
      "A.rb"
    )
      .moreCode(
        """
          |class B
          | def bar
          | end
          |end
          |
          |B::bar
          |""".stripMargin,
        "bar/B.rb"
      )
      .moreCode(
        """
          |a = A.new
          |""".stripMargin,
        "Foo.rb"
      )
      .moreCode(
        """
          |B.bar
          |""".stripMargin,
        "Bar.rb"
      )
      .moreCode(
        """
          |GEM
          |  remote: https://rubygems.org/
          |  specs:
          |    zeitwerk (2.2.1)
          |""".stripMargin,
        "Gemfile.lock"
      )

    "be explicitly detected and imported for a constructor type" in {
      inside(cpg.imports.where(_.call.file.name(".*Foo.*")).headOption) {
        case Some(i) =>
          i.importedAs shouldBe Some("A")
          i.importedEntity shouldBe Some("A")
        case None =>
          fail("Expected `A` to be explicitly imported into `Foo.rb`")
      }
    }

    "be explicitly detected and imported for a call invocation" in {
      inside(cpg.imports.where(_.call.file.name(".*Bar.*")).headOption) {
        case Some(i) =>
          i.importedAs shouldBe Some("bar/B")
          i.importedEntity shouldBe Some("bar/B")
        case None =>
          fail("Expected `B` to be explicitly imported into `Foo.rb`")
      }
    }

    "not import a type from the type's defining file" in {
      cpg.imports.where(_.call.file.name(".*B.rb")).size shouldBe 0
    }

  }

  "Builtin Types type-map" should {
    val cpg = code("""
        |require 'csv'
        |require 'pp'
        |CSV.parse("")
        |CSV::Table.new()
        |PP.pp(obj)
        |""".stripMargin)

    "resolve calls to builtin functions" in {
      inside(cpg.call.methodFullName("(pp|csv).*").l) {
        case csvParseCall :: csvTableInitCall :: ppCall :: Nil =>
          csvParseCall.methodFullName shouldBe "csv.CSV:parse"
          ppCall.methodFullName shouldBe "pp.PP:pp"
          csvTableInitCall.methodFullName shouldBe "csv.CSV.Table:<init>"
        case xs => fail(s"Expected three calls, got [${xs.code.mkString(",")}] instead")
      }
    }
  }
}
