package io.joern.rubysrc2cpg.deprecated.passes

import io.joern.rubysrc2cpg.deprecated.utils.PackageTable
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.joern.x2cpg.Defines as XDefines
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.semanticcpg.language.importresolver.*
import io.shiftleft.semanticcpg.language.*

import scala.collection.immutable.List

object RubyTypeRecoveryTests {
  def getPackageTable: PackageTable = {
    val packageTable = PackageTable()
    packageTable.addTypeDecl("sendgrid-ruby", "API", "SendGrid.API")
    packageTable.addModule("dbi", "DBI", "DBI")
    packageTable.addTypeDecl("logger", "Logger", "Logger")
    packageTable.addModule("stripe", "Customer", "Stripe.Customer")
    packageTable
  }

}
class RubyTypeRecoveryTests
    extends RubyCode2CpgFixture(
      withPostProcessing = true,
      packageTable = Some(RubyTypeRecoveryTests.getPackageTable),
      useDeprecatedFrontend = true
    ) {

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

    "be present in (Case 1)" ignore {
      cpg.identifier("sg").lineNumber(5).typeFullName.l shouldBe List("sendgrid-ruby::program.SendGrid.API")
      cpg.call("client").dispatchType.l shouldBe List(DispatchTypes.DYNAMIC_DISPATCH)
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
        |module MyNamespace
        |  MY_CONSTANT = 42
        |end
        |""".stripMargin,
      "main.rb"
    )
    "resolve 'x' identifier types despite shadowing" in {
      val List(xOuterScope, xInnerScope) = cpg.identifier("x").take(2).l
      xOuterScope.dynamicTypeHintFullName shouldBe Seq("__builtin.Integer", "__builtin.String")
      xInnerScope.dynamicTypeHintFullName shouldBe Seq("__builtin.Integer", "__builtin.String")
    }

    "resolve module constant type" in {
      cpg.typeDecl("MyNamespace").size shouldBe 1
      val List(typeDecl) = cpg.typeDecl("MyNamespace").l
      val List(myconst)  = typeDecl.member.l
      myconst.typeFullName shouldBe "__builtin.Integer"
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

    "present the declared method name when a built-in with the same name is used in the same compilation unit" in {
      val List(absCall) = cpg.call("sleep").l
      absCall.methodFullName shouldBe "main.rb::program.sleep"
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

    // TODO Waiting for Module modelling to be done
    "resolve correct imports via tag nodes" ignore {
      val List(foo: ResolvedTypeDecl) =
        cpg.file(".*foo.rb").ast.isCall.where(_.referencedImports).tag._toEvaluatedImport.toList: @unchecked
      foo.fullName shouldBe "dbi::program.DBI"
      val List(bar: ResolvedTypeDecl) =
        cpg.file(".*bar.rb").ast.isCall.where(_.referencedImports).tag._toEvaluatedImport.toList: @unchecked
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

  "assignment from a call to a identifier inside an imported module using new" should {
    lazy val cpg = code("""
        |require 'logger'
        |
        |log = Logger.new(STDOUT)
        |log.error("foo")
        |
        |""".stripMargin).cpg

    "resolve correct imports via tag nodes" in {
      val List(logging: ResolvedMethod, _) =
        cpg.call.where(_.referencedImports).tag._toEvaluatedImport.toList: @unchecked
      logging.fullName shouldBe s"logger::program.Logger.${XDefines.ConstructorMethodName}"
    }

    "provide a dummy type" ignore {
      val List(error) = cpg.call("error").l: @unchecked
      error.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      val Some(log) = cpg.identifier("log").headOption: @unchecked
      log.typeFullName shouldBe "logger::program.Logger"
      val List(errorCall) = cpg.call("error").l
      errorCall.methodFullName shouldBe "logger::program.Logger.error"
    }
  }

  "assignment from a call to a identifier inside an imported module using methodCall" should {
    lazy val cpg = code("""
        |require 'stripe'
        |
        |customer = Stripe::Customer.create
        |
        |""".stripMargin).cpg

    "resolved the type of call" in {
      val Some(create) = cpg.call("create").headOption: @unchecked
      create.methodFullName shouldBe "stripe::program.Stripe.Customer.create"
    }

    "resolved the type of identifier" in {
      val Some(customer) = cpg.identifier("customer").headOption: @unchecked
      customer.typeFullName shouldBe "stripe::program.Stripe.Customer.create.<returnValue>"
    }
  }

  "recovery of type for call having a method with same name" should {
    lazy val cpg = code("""
        |require "dbi"
        |
        |def connect
        |  puts "I am here"
        |end
        |
        |d = DBI.connect("DBI:Mysql:TESTDB:localhost", "testuser", "test123")
        |""".stripMargin)

    "have a correct type for call `connect`" in {
      cpg.call("connect").methodFullName.l shouldBe List("dbi::program.DBI.connect")
    }

    "have a correct type for identifier `d`" in {
      cpg.identifier("d").typeFullName.l shouldBe List("dbi::program.DBI.connect.<returnValue>")
    }
  }
}
