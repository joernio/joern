package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*
import io.joern.rubysrc2cpg.RubySrc2Cpg
import io.joern.rubysrc2cpg.Config
import scala.util.{Success, Failure}
import org.scalatest.Inspectors

class ImportTests extends RubyCode2CpgFixture(withPostProcessing = true) with Inspectors {

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
        cpg.method.name(":program").filename("t1.rb").ast.isCall.methodFullName(".*:initialize").methodFullName.l
      newCall should startWith(s"${path}.rb:")
    }
  }

  "Ambiguous methods resolves to included method" ignore {
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
          csvTableInitCall.methodFullName shouldBe "csv.CSV.Table:initialize"
        case xs => fail(s"Expected three calls, got [${xs.code.mkString(",")}] instead")
      }
    }
  }

  "`require_all` on a directory" should {
    val cpg = code("""
        |require_all './dir'
        |Module1.foo
        |Module2.foo
        |""".stripMargin)
      .moreCode(
        """
        |module Module1
        | def foo
        | end
        |end
        |""".stripMargin,
        "dir/module1.rb"
      )
      .moreCode(
        """
        |module Module2
        | def foo
        | end
        |end
        |""".stripMargin,
        "dir/module2.rb"
      )

    "allow the resolution for all modules in that directory" in {
      cpg.call("foo").methodFullName.l shouldBe List(
        "dir/module1.rb:<global>::program.Module1:foo",
        "dir/module2.rb:<global>::program.Module2:foo"
      )
    }
  }

  "`require_all`, `require_relative`, and `load`" should {
    val cpg = code("""
        |require_all './dir'
        |require_relative '../foo'
        |load 'pp'
        |""".stripMargin)

    "also create import nodes" in {
      inside(cpg.imports.l) {
        case requireAll :: requireRelative :: load :: Nil =>
          requireAll.importedAs shouldBe Option("./dir")
          requireAll.isWildcard shouldBe Option(true)
          requireRelative.importedAs shouldBe Option("../foo")
          load.importedAs shouldBe Option("pp")
        case xs => fail(s"Expected two imports, got [${xs.code.mkString(",")}] instead")
      }
    }
  }

  // TODO: This will need to be fixed with the Import resolver
  "Modifying `$LOADER` with an additional entry" ignore {
    val cpg = code(
      """
        |lib_dir = File.expand_path('lib', __dir__)
        |src_dir = File.expand_path('src', File.dirname(__FILE__))
        |
        |$LOADER << lib_dir unless $LOADER.include?(lib_dir)
        |$LOAD_PATH.unshift(src_dir) unless $LOAD_PATH.include?(src_dir)
        |
        |require 'file1'
        |require 'file2'
        |require 'file3'
        |
        |File1::foo # lib/file1.rb::program:foo
        |File2::foo # lib/file2.rb::program:foo
        |File3::foo # src/file3.rb::program:foo
        |""".stripMargin,
      "main.rb"
    ).moreCode(
      """
        |module File1
        | def self.foo
        | end
        |end
        |""".stripMargin,
      "lib/file1.rb"
    ).moreCode(
      """
        |module File2
        | def self.foo
        | end
        |end
        |""".stripMargin,
      "lib/file2.rb"
    ).moreCode(
      """
        |module File3
        | def self.foo
        | end
        |end
        |""".stripMargin,
      "src/file3.rb"
    )

    "resolve the calls directly" in {
      inside(cpg.call.name("foo.*").l) {
        case foo1 :: foo2 :: foo3 :: Nil =>
          foo1.methodFullName shouldBe "lib/file1.rb:<global>::program.File1:foo"
          foo2.methodFullName shouldBe "lib/file2.rb:<global>::program.File2:foo"
          foo3.methodFullName shouldBe "src/file3.rb:<global>::program.File3:foo"
        case xs => fail(s"Expected 3 calls, got [${xs.code.mkString(",")}] instead")
      }
    }
  }
}
