package io.joern.rubysrc2cpg.passes

import io.joern.rubysrc2cpg.Config
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.joern.rubysrc2cpg.utils.PackageTable
import io.joern.x2cpg.passes.frontend.ImportsPass.{
  ResolvedMethod,
  ResolvedTypeDecl,
  TagToResolvedImportExt,
  UnknownImport
}
import io.shiftleft.semanticcpg.language.*

object RubyTypeRecoveryTests {
  def getPackageTable: PackageTable = {
    val packageTable = PackageTable()
    packageTable.addPackageMethod("sendgrid-ruby", "client", "SendGrid.API", "API")
    packageTable.addPackageMethod("dbi", "connect", "DBI", "DBI")
    packageTable.addPackageMethod("dbi", "select_one", "DBI", "DBI")
    packageTable.addPackageMethod("logger", "error", "Logger", "Logger")
    packageTable
  }

}
class RubyTypeRecoveryTests
    extends RubyCode2CpgFixture(withPostProcessing = true, packageTable = Some(RubyTypeRecoveryTests.getPackageTable)) {

  "Type information for nodes with external dependency" should {

    val cpg = code(
      """
        |require "sendgrid-ruby"
        |
        |def func
        |   sg = SendGrid::API.new(api_key: ENV['SENDGRID_API_KEY'])
        |   response = sg.client.mail._('send').post(request_body: data)
        |end
        |""".stripMargin,
      "main.rb"
    )

    "be present in (Case 1)" in {
      cpg.identifier("sg").lineNumber(5).typeFullName.l shouldBe List("sendgrid-ruby::program.SendGrid.API")
      cpg.call("client").methodFullName.l shouldBe List("sendgrid-ruby::program.SendGrid.API.client")
    }

    "be present in (Case 2)" ignore {
      cpg.call("post").methodFullName.l shouldBe List(
        "sendgrid-ruby::program.SendGrid.API.client<returnValue>.mail<returnValue>.anonymous<returnValue>.post"
      )
    }
  }

  "literals declared from built-in types" should {
    val cpg = code(
      """
        |x = 123
        |
        |def newfunc
        | x = "foo"
        |end
        |""".stripMargin,
      "main.rb"
    )
    "resolve 'x' identifier types despite shadowing" in {
      val List(xOuterScope, xInnerScope) = cpg.identifier("x").take(2).l
      xOuterScope.dynamicTypeHintFullName shouldBe Seq("__builtin.Integer", "__builtin.String")
      xInnerScope.dynamicTypeHintFullName shouldBe Seq("__builtin.Integer", "__builtin.String")
    }
  }

  "recovering paths for built-in calls" should {
    lazy val cpg = code(
      """
        |print("Hello world")
        |puts "Hello"
        |
        |def sleep(input)
        |end
        |
        |sleep(2)
        |""".stripMargin,
      "main.rb"
    ).cpg

    "resolve 'print' and 'puts' calls" in {
      val List(printCall) = cpg.call("print").l
      printCall.methodFullName shouldBe "__builtin.print"
      val List(maxCall) = cpg.call("puts").l
      maxCall.methodFullName shouldBe "__builtin.puts"
    }

    "conservatively present either option when an imported function uses the same name as a builtin" ignore {
      val List(absCall) = cpg.call("sleep").l
      absCall.methodFullName shouldBe "main.rb::program.sleep"
      absCall.dynamicTypeHintFullName shouldBe Seq("main.rb::program.sleep")
    }
  }

  "recovering module members across modules" should {
    lazy val cpg = code(
      """
        |require "dbi"
        |
        |module FooModule
        | x = 1
        | y = "test"
        | db = DBI.connect("DBI:Mysql:TESTDB:localhost", "testuser", "test123")
        |end
        |
        |""".stripMargin,
      "foo.rb"
    ).moreCode(
      """
        |require_relative "./foo.rb"
        |
        |z = FooModule::x
        |z = FooModule::y
        |
        |d = FooModule::db
        |
        |row = d.select_one("SELECT VERSION()")
        |
        |""".stripMargin,
      "bar.rb"
    ).cpg

    // TODO: Need to fix it
    "resolve correct imports via tag nodes" ignore {
      val List(foo1: ResolvedMethod, foo2: ResolvedTypeDecl) =
        cpg.file(".*foo.rb").ast.isCall.where(_.referencedImports).tag.toResolvedImport.toList: @unchecked
      foo1.fullName shouldBe "dbi::program.DBI.new"
      foo2.fullName shouldBe "dbi::program.DBI"
      val List(bar: ResolvedTypeDecl) =
        cpg.file(".*bar.rb").ast.isCall.where(_.referencedImports).tag.toResolvedImport.toList: @unchecked
      bar.fullName shouldBe "foo.rb::program.FooModule"
    }

    "resolve 'x' and 'y' locally under foo.rb" in {
      val Some(x) = cpg.identifier("x").where(_.file.name(".*foo.*")).headOption: @unchecked
      x.typeFullName shouldBe "__builtin.Integer"
      val Some(y) = cpg.identifier("y").where(_.file.name(".*foo.*")).headOption: @unchecked
      y.typeFullName shouldBe "__builtin.String"
    }

    "resolve 'FooModule.x' and 'FooModule.y' field access primitive types correctly" ignore {
      val List(z1, z2) = cpg.file
        .name(".*bar.*")
        .ast
        .isIdentifier
        .name("z")
        .l
      z1.typeFullName shouldBe "ANY"
      z1.dynamicTypeHintFullName shouldBe Seq("__builtin.Integer", "__builtin.String")
      z2.typeFullName shouldBe "ANY"
      z2.dynamicTypeHintFullName shouldBe Seq("__builtin.Integer", "__builtin.String")
    }

    "resolve 'FooModule.d' field access object types correctly" ignore {
      val Some(d) = cpg.file
        .name(".*bar.*")
        .ast
        .isIdentifier
        .name("d")
        .headOption: @unchecked
      d.typeFullName shouldBe "dbi::program.DBI.connect.<returnValue>"
      d.dynamicTypeHintFullName shouldBe Seq()
    }

    "resolve a 'select_one' call indirectly from 'FooModule.d' field access correctly" ignore {
      val List(d) = cpg.file
        .name(".*bar.*")
        .ast
        .isCall
        .name("select_one")
        .l
      d.methodFullName shouldBe "dbi::program.DBI.connect.<returnValue>.select_one"
      d.dynamicTypeHintFullName shouldBe Seq()
      d.callee(NoResolve).isExternal.headOption shouldBe Some(true)
    }

  }

  "assignment from a call to a method inside an imported module" should {
    lazy val cpg = code("""
        |require 'logger'
        |
        |log = Logger.new(STDOUT)
        |log.error("foo")
        |
        |""".stripMargin).cpg

    "resolve correct imports via tag nodes" in {
      val List(logging: ResolvedMethod, _) = cpg.call.where(_.referencedImports).tag.toResolvedImport.toList: @unchecked
      logging.fullName shouldBe "logger::program.Logger.new"
    }

    "provide a dummy type" in {
      val Some(log) = cpg.identifier("log").headOption: @unchecked
      log.typeFullName shouldBe "logger::program.Logger"
      val List(errorCall) = cpg.call("error").l
      errorCall.methodFullName shouldBe "logger::program.Logger.error"
    }
  }
}
