package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.passes.Defines
import io.joern.rubysrc2cpg.passes.Defines.{Initialize, Main}
import io.joern.rubysrc2cpg.passes.GlobalTypes.{builtinPrefix, kernelPrefix}
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.joern.x2cpg.frontendspecific.rubysrc2cpg.{ImplicitRequirePass, ImportsPass, TypeImportInfo}
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.nodes.Literal
import io.shiftleft.semanticcpg.language.*
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

  "`require_relative 'test'` is a CALL node with an IMPORT node pointing to it" in {
    val cpg = code("""
        |require_relative 'test'
        |""".stripMargin)
    val List(importNode) = cpg.imports.l
    importNode.importedEntity shouldBe Some("test")
    importNode.importedAs shouldBe Some("test")
    val List(call) = importNode.call.l
    call.callee.name.l shouldBe List("require_relative")
    call.argument.where(_.argumentIndexGt(0)).code.l shouldBe List("'test'")
  }

  "`load 'test'` is a CALL node with an IMPORT node pointing to it" in {
    val cpg = code("""
        |load 'test'
        |""".stripMargin)
    val List(importNode) = cpg.imports.l
    importNode.importedEntity shouldBe Some("test")
    importNode.importedAs shouldBe Some("test")
    val List(call) = importNode.call.l
    call.callee.name.l shouldBe List("load")
    call.argument.where(_.argumentIndexGt(0)).code.l shouldBe List("'test'")
  }

  "`require_all 'test'` is a CALL node with an IMPORT node pointing to it" in {
    val cpg = code("""
        |require_all 'test'
        |""".stripMargin)
    val List(importNode) = cpg.imports.l
    importNode.importedEntity shouldBe Some("test")
    importNode.importedAs shouldBe Some("test")
    val List(call) = importNode.call.l
    call.callee.name.l shouldBe List("require_all")
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
        cpg.method.isModule
          .filename("t1.rb")
          .ast
          .isCall
          .dynamicTypeHintFullName
          .filter(x => x.startsWith(path) && x.endsWith(Initialize))
          .l

      newCall should startWith(s"$path.rb:")
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
        cpg.method.name("bar").ast.isCall.methodFullName(s".*\\.$Main\\.(A|B).foo").methodFullName.l
      methodName should endWith(s"${moduleName}:foo")
    }
  }

  "implicitly imported types in base class" should {
    val cpg = code(
      """
        |class MyController < ApplicationController
        |end
        |""".stripMargin,
      "app/controllers/my_controller.rb"
    )
      .moreCode(
        """
          |class ApplicationController
          |end
          |""".stripMargin,
        "app/controllers/application_controller.rb"
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

    "result in require statement of the file containing the symbol" in {
      inside(cpg.imports.where(_.call.file.name(".*my_controller.rb")).toList) { case List(i) =>
        i.importedAs shouldBe Some("app/controllers/application_controller")
        i.importedEntity shouldBe Some("app/controllers/application_controller")
      }
    }
  }

  "implicitly imported types in base class that are qualified names" should {
    val cpg = code(
      """
        |class MyController < Controllers::ApplicationController
        |end
        |""".stripMargin,
      "app/controllers/my_controller.rb"
    )
      .moreCode(
        """
          |module Controllers
          | class ApplicationController
          | end
          |end
          |""".stripMargin,
        "app/controllers/controllers.rb"
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

    "result in require statement of the file containing the symbol" in {
      inside(cpg.imports.where(_.call.file.name(".*my_controller.rb")).toList) { case List(i) =>
        i.importedAs shouldBe Some("app/controllers/controllers")
        i.importedEntity shouldBe Some("app/controllers/controllers")
      }
    }
  }

  "implicitly imported types that are qualified names in an include statement" should {
    val cpg = code(
      """
        |module MyController
        | include Controllers::ApplicationController
        |end
        |""".stripMargin,
      "app/controllers/my_controller.rb"
    )
      .moreCode(
        """
          |module Controllers
          | class ApplicationController
          | end
          |end
          |""".stripMargin,
        "app/controllers/controllers.rb"
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

    "result in require statement of the file containing the symbol" in {
      inside(cpg.imports.where(_.call.file.name(".*my_controller.rb")).toList) { case List(i) =>
        i.importedAs shouldBe Some("app/controllers/controllers")
        i.importedEntity shouldBe Some("app/controllers/controllers")
      }
    }
  }

  "implicitly imported types in include statement" should {
    val cpg = code(
      """
        |class MyController
        |  include ApplicationController
        |end
        |""".stripMargin,
      "app/controllers/my_controller.rb"
    )
      .moreCode(
        """
          |class ApplicationController
          |end
          |""".stripMargin,
        "app/controllers/application_controller.rb"
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

    "result in require statement of the file containing the symbol" in {
      inside(cpg.imports.where(_.call.file.name(".*my_controller.rb")).toList) { case List(i) =>
        i.importedAs shouldBe Some("app/controllers/application_controller")
        i.importedEntity shouldBe Some("app/controllers/application_controller")
      }
    }
  }

  "implicitly imported types in extend statement" should {
    val cpg = code(
      """
        |class MyController
        |  extend ApplicationController
        |end
        |""".stripMargin,
      "app/controllers/my_controller.rb"
    )
      .moreCode(
        """
          |class ApplicationController
          |end
          |""".stripMargin,
        "app/controllers/application_controller.rb"
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

    "result in require statement of the file containing the symbol" in {
      inside(cpg.imports.where(_.call.file.name(".*my_controller.rb")).toList) { case List(i) =>
        i.importedAs shouldBe Some("app/controllers/application_controller")
        i.importedEntity shouldBe Some("app/controllers/application_controller")
      }
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
          |B::bar()
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
          |def func()
          |  B.bar()
          |end
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

    "create a `require` call following the simplified format" in {
      val require = cpg.call("require").head
      require.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      require.methodFullName shouldBe s"$kernelPrefix.require"

      val strLit = require.argument(1).asInstanceOf[Literal]
      strLit.typeFullName shouldBe s"$kernelPrefix.String"
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
        case csvParseCall :: csvTableCall :: ppCall :: Nil =>
          csvParseCall.methodFullName shouldBe "csv.CSV.parse"
          csvTableCall.methodFullName shouldBe "csv.CSV.Table.initialize"
          ppCall.methodFullName shouldBe "pp.PP.pp"
        case xs => fail(s"Expected calls, got [${xs.code.mkString(",")}] instead")
      }

      // TODO: fixme - set is empty
//      cpg.call(Initialize).dynamicTypeHintFullName.toSet should contain("csv.CSV.Table.initialize")
    }
  }

  "`require_all` on a directory" should {
    val cpg = code("""
        |require_all './dir'
        |Module1.foo()
        |Module2.foo()
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
        s"dir/module1.rb:$Main.Module1.foo",
        s"dir/module2.rb:$Main.Module2.foo"
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
        |File1::foo # lib/file1.rb.<main>.foo
        |File2::foo # lib/file2.rb.<main>.foo
        |File3::foo # src/file3.rb.<main>.foo
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
          foo1.methodFullName shouldBe s"lib/file1.rb:$Main.File1.foo"
          foo2.methodFullName shouldBe s"lib/file2.rb:$Main.File2.foo"
          foo3.methodFullName shouldBe s"src/file3.rb:$Main.File3.foo"
        case xs => fail(s"Expected 3 calls, got [${xs.code.mkString(",")}] instead")
      }
    }
  }
}

class ImportWithAutoloadedExternalGemsTests extends RubyCode2CpgFixture(withPostProcessing = false) {

  "use of a type specified as external" should {

    val cpg = code(
      """
        |x = Base64.encode("Hello, world!")
        |Bar::Foo.new
        |""".stripMargin,
      "encoder.rb"
    )

    ImplicitRequirePass(cpg, TypeImportInfo("Base64", "base64") :: TypeImportInfo("Bar", "foobar") :: Nil)
      .createAndApply()
    ImportsPass(cpg).createAndApply()

    "result in require statement of the file containing the symbol" in {
      inside(cpg.imports.where(_.call.file.name(".*encoder.rb")).toList) { case List(i1, i2) =>
        i1.importedAs shouldBe Some("base64")
        i1.importedEntity shouldBe Some("base64")

        i2.importedAs shouldBe Some("foobar")
        i2.importedEntity shouldBe Some("foobar")
      }
    }
  }

}
