package io.joern.rubysrc2cpg.passes

import io.joern.rubysrc2cpg.passes.Defines.Main
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.joern.x2cpg.Defines as XDefines
import io.joern.rubysrc2cpg.passes.GlobalTypes.kernelPrefix
import io.shiftleft.codepropertygraph.generated.nodes.Identifier
import io.shiftleft.semanticcpg.language.importresolver.*
import io.shiftleft.semanticcpg.language.*

import scala.collection.immutable.List

object RubyExternalTypeRecoveryTests {
  private val SENDGRID_GEMFILE =
    """
      |# frozen_string_literal: true
      |source "https://rubygems.org"
      |
      |ruby "2.6.5"
      |
      |gem "sendgrid-ruby"
      |""".stripMargin

  private val STRIPE_GEMFILE =
    """
      |# frozen_string_literal: true
      |source "https://rubygems.org"
      |
      |ruby "2.6.5"
      |
      |gem "stripe"
      |""".stripMargin

  private val LOGGER_GEMFILE =
    """
      |# frozen_string_literal: true
      |source "https://rubygems.org"
      |
      |ruby "2.6.5"
      |
      |gem "logger"
      |gem "dummy_logger"
      |""".stripMargin
}

class RubyInternalTypeRecoveryTests extends RubyCode2CpgFixture(withPostProcessing = true) {
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

    "resolve 'print' and 'puts' StubbedRubyType calls" in {
      val List(printCall) = cpg.call("print").l
      printCall.methodFullName shouldBe s"$kernelPrefix.print"
      val List(maxCall) = cpg.call("puts").l
      maxCall.methodFullName shouldBe s"$kernelPrefix.puts"
    }

    "present the declared method name when a built-in with the same name is used in the same compilation unit" in {
      val List(absCall) = cpg.call("sleep").l
      absCall.methodFullName shouldBe s"main.rb:$Main.sleep"
    }
  }

  "Type information for literals" should {
    val cpg = code("""
                     |def func
                     | a = 2
                     | b = "abc"
                     | b
                     |end
                     |
                     |def func2
                     | func
                     |end
                     |
                     |c = func2()
                     |""".stripMargin)

    "propagate function return types" in {
      inside(cpg.method.name("func2?").l) {
        case func :: func2 :: Nil =>
          func.methodReturn.typeFullName shouldBe s"$kernelPrefix.String"
          func2.methodReturn.typeFullName shouldBe s"$kernelPrefix.String"
        case xs => fail(s"Expected 2 functions, got [${xs.name.mkString(",")}]")
      }
    }

    "propagate return type to identifier c" in {
      inside(cpg.identifier.name("c").l) {
        case cIdent :: Nil =>
          cIdent.typeFullName shouldBe s"$kernelPrefix.String"
        case xs => fail(s"Expected one identifier for c, got [${xs.name.mkString(",")}]")
      }
    }
  }

  "Type information for imported function" should {
    val cpg = code(
      """
        |class Test2A
        |end
        |
        |module Test2B
        |end
        |
        |def func
        |  "abc"
        |end
        |
        |""".stripMargin,
      "test2.rb"
    )
      .moreCode(
        """
          |require 'test2'
          |a = func
          |
          |b = Test2A.new
          |""".stripMargin,
        "test1.rb"
      )

    // TODO: Revisit
    "propagate to assigned variable" ignore {
      inside(cpg.file("test1.rb").method.name(":program").call.nameExact("<operator>.assignment").l) {
        case funcAssignment :: constructAssignment :: tmpAssignment :: Nil =>
          inside(funcAssignment.argument.l) {
            case (lhs: Identifier) :: rhs :: Nil =>
              lhs.typeFullName shouldBe s"$kernelPrefix.String"
            case xs => fail(s"Expected lhs and rhs, got [${xs.code.mkString(",")}] ")
          }

          inside(constructAssignment.argument.l) {
            case (lhs: Identifier) :: rhs :: Nil =>
              lhs.typeFullName shouldBe s"test2.rb:$Main.Test2A"
            case xs => fail(s"Expected lhs and rhs, got [${xs.code.mkString(",")}]")
          }
        case xs => fail(s"Expected lhs and rhs, got [${xs.code.mkString(",")}]")
      }
    }
  }

  "Type information for constructors" should {
    val cpg = code("""
                     |class A
                     |end
                     |
                     |def func
                     | d = A.new
                     | d
                     |end
                     |
                     |a = A.new
                     |b = func
                     |""".stripMargin)

    // TODO: Revisit
    "propagate to identifier" ignore {
      inside(cpg.identifier.name("(a|b)").l) {
        case aIdent :: bIdent :: Nil =>
          aIdent.typeFullName shouldBe s"Test0.rb:$Main.A"
          bIdent.typeFullName shouldBe s"Test0.rb:$Main.A"
        case xs => fail(s"Expected one identifier, got [${xs.name.mkString(",")}]")
      }
    }
  }
}

class RubyExternalTypeRecoveryTests
    extends RubyCode2CpgFixture(withPostProcessing = true, downloadDependencies = true) {

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
      .moreCode(RubyExternalTypeRecoveryTests.SENDGRID_GEMFILE, "Gemfile")

    // TODO: Revisit
    "be present in (Case 1)" ignore {
      cpg.identifier("sg").lineNumber(5).typeFullName.l shouldBe List("sendgrid-ruby.SendGrid.API")
      cpg.call("client").methodFullName.headOption shouldBe Option("sendgrid-ruby.SendGrid.API.client")
    }

    "resolve correct imports via tag nodes" in {
      inside(cpg.call.where(_.referencedImports).l) {
        case sendgridImport :: Nil =>
          inside(
            sendgridImport.tag._toEvaluatedImport
              .filter(_.label == EvaluatedImport.RESOLVED_METHOD)
              .map(_.asInstanceOf[ResolvedMethod])
              .filter(_.fullName.startsWith("sendgrid-ruby.SendGrid.API"))
              .l
          ) {
            case apiClassInit :: apiInit :: Nil =>
              apiInit.fullName shouldBe s"sendgrid-ruby.SendGrid.API.${Defines.Initialize}"
              apiClassInit.fullName shouldBe s"sendgrid-ruby.SendGrid.API<class>.${Defines.Initialize}"
            case xs => fail(s"Two ResolvedMethods expected, got [${xs.mkString(",")}]")
          }
        case xs => fail(s"Only sendgrid-ruby should be referenced, got [${xs.name.mkString}]")
      }
    }

    "be present in (Case 2)" ignore {
      cpg.call("post").methodFullName.l shouldBe List(
        s"sendgrid-ruby.$Main.SendGrid.API.client<returnValue>.mail<returnValue>.anonymous<returnValue>.post"
      )
    }
  }

  // TODO: Handle . versus :: Syntax needs to be resolved first
  "Type information for module members across modules" ignore {
    val cpg = code(
      """
        |# require "dbi"
        |
        |module FooModule
        | x = 1
        | y = "test"
        | # db = DBI.connect("DBI:Mysql:TESTDB:localhost", "testuser", "test123")
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
        |# d = FooModule::db
        |
        |# row = d.select_one("SELECT VERSION()")
        |
        |""".stripMargin,
      "bar.rb"
    ).cpg

    "resolve 'x' and 'y' locally under foo.rb" in {
      val Some(x) = cpg.identifier("x").where(_.file.name(".*foo.*")).headOption: @unchecked
      x.typeFullName shouldBe s"$kernelPrefix.Integer"
      val Some(y) = cpg.identifier("y").where(_.file.name(".*foo.*")).headOption: @unchecked
      y.typeFullName shouldBe s"$kernelPrefix.String"
    }

    "resolve 'FooModule.x' and 'FooModule.y' field access primitive types correctly" in {
      val List(z1, z2) = cpg.file
        .name(".*bar.*")
        .ast
        .isIdentifier
        .name("z")
        .l
      z1.typeFullName shouldBe "ANY"
      z1.dynamicTypeHintFullName shouldBe Seq(s"$kernelPrefix.Integer", s"$kernelPrefix.String")
      z2.typeFullName shouldBe "ANY"
      z2.dynamicTypeHintFullName shouldBe Seq(s"$kernelPrefix.Integer", s"$kernelPrefix.String")
    }

    "resolve 'FooModule.d' field access object types correctly" ignore {
      val Some(d) = cpg.file
        .name(".*bar.*")
        .ast
        .isIdentifier
        .name("d")
        .headOption: @unchecked
      d.typeFullName shouldBe "dbi.$Main.DBI.connect.<returnValue>"
      d.dynamicTypeHintFullName shouldBe Seq()
    }

    "resolve a 'select_one' call indirectly from 'FooModule.d' field access correctly" ignore {
      val List(d) = cpg.file
        .name(".*bar.*")
        .ast
        .isCall
        .name("select_one")
        .l
      d.methodFullName shouldBe "dbi.$Main.DBI.connect.<returnValue>.select_one"
      d.dynamicTypeHintFullName shouldBe Seq()
      d.callee(NoResolve).isExternal.headOption shouldBe Some(true)
    }

    // TODO Waiting for Module modelling to be done
    "resolve correct imports via tag nodes" ignore {
      val List(foo: ResolvedTypeDecl) =
        cpg.file(".*foo.rb").ast.isCall.where(_.referencedImports).tag._toEvaluatedImport.toList: @unchecked
      foo.fullName shouldBe s"dbi.$Main.DBI"
      val List(bar: ResolvedTypeDecl) =
        cpg.file(".*bar.rb").ast.isCall.where(_.referencedImports).tag._toEvaluatedImport.toList: @unchecked
      bar.fullName shouldBe s"foo.rb.$Main.FooModule"
    }

  }

  "assignment from a call to a identifier inside an imported module using new" should {
    lazy val cpg = code("""
                          |require 'logger'
                          |
                          |log = Logger.new(STDOUT)
                          |log.error("foo")
                          |
                          |""".stripMargin)
      .moreCode(RubyExternalTypeRecoveryTests.LOGGER_GEMFILE, "Gemfile")

    "resolve correct imports via tag nodes" in {
      inside(cpg.call.where(_.referencedImports).l) {
        case loggerImport :: Nil =>
          inside(
            loggerImport.tag._toEvaluatedImport
              .filter(_.label == EvaluatedImport.RESOLVED_METHOD)
              .map(_.asInstanceOf[ResolvedMethod])
              .filter(_.fullName == s"logger.Logger.${Defines.Initialize}")
              .l
          ) {
            case loggerInit :: Nil =>
              loggerInit.fullName shouldBe s"logger.Logger.${Defines.Initialize}"
            case xs => fail(s"Two ResolvedMethods expected, got [${xs.mkString(",")}]")
          }
        case xs => fail(s"Only logger library should be referenced, got [${xs.name.mkString}]")
      }
    }

    // TODO: Revisit
    "provide a dummy type" ignore {
      val Some(log) = cpg.identifier("log").headOption: @unchecked
      log.typeFullName shouldBe "logger.Logger"
      val List(errorCall) = cpg.call("error").l
      errorCall.methodFullName shouldBe "logger.Logger.error"
    }
  }

  // TODO: Fix parsing errors on Stripe library
  "assignment from a call to a identifier inside an imported module using methodCall" ignore {
    lazy val cpg = code(
      """
                          |require 'stripe'
                          |
                          |customer = Stripe::Customer.create
                          |
                          |""".stripMargin,
      "main.rb"
    )
      .moreCode(RubyExternalTypeRecoveryTests.STRIPE_GEMFILE, "Gemfile")

    "resolved the type of call" in {
      val Some(create) = cpg.call("create").headOption: @unchecked
      create.methodFullName shouldBe s"stripe.rb:$Main.Stripe.Customer.create"
    }

    "resolved the type of identifier" in {
      val Some(customer) = cpg.identifier("customer").headOption: @unchecked
      customer.typeFullName shouldBe s"stripe.$Main.Stripe.Customer.create.<returnValue>"
    }
  }

  // TODO: revisit
  "recovery of type for call having a method with same name" ignore {
    lazy val cpg = code("""
                          |require "logger"
                          |
                          |def error(a)
                          |  puts "I am here"
                          |end
                          |
                          |d = Logger.new(STDOUT)
                          |e = d.error("abc")
                          |""".stripMargin)
      .moreCode(RubyExternalTypeRecoveryTests.LOGGER_GEMFILE, "Gemfile")

    "have a correct type for call `connect`" in {
      cpg.call("error").methodFullName.l shouldBe List("logger.Logger.error")
    }

    "have a correct type for identifier `d`" in {
      cpg.identifier("e").typeFullName.l shouldBe List("logger.Logger.error.<returnValue>")
    }
  }
}
