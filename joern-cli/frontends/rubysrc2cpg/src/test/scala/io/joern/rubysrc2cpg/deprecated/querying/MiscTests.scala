package io.joern.rubysrc2cpg.deprecated.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.joern.x2cpg.Defines
import io.shiftleft.semanticcpg.language.*

class MiscTests extends RubyCode2CpgFixture(useDeprecatedFrontend = true) {

  "CPG for code with BEGIN and END blocks" should {
    val cpg = code("""
          |#!/usr/bin/env ruby
          |
          |# This code block will be executed before the program begins
          |BEGIN {
          |  beginvar = 5
          |  beginbool = beginvar > 21
          |}
          |
          |# This is the main logic of the program
          |puts "Hello, world!"
          |
          |# This code block will be executed after the program finishes
          |END {
          |  endvar = 67
          |  endbool = endvar > 23
          |}
          |""".stripMargin)

    "recognise all identifier and call nodes" in {
      cpg.identifier.name("beginvar").size shouldBe 2
      cpg.identifier.name("endvar").size shouldBe 2
      cpg.identifier.name("beginbool").size shouldBe 1
      cpg.identifier.name("endbool").size shouldBe 1
      cpg.call.name("puts").size shouldBe 1
      cpg.identifier.size shouldBe 7 // 1 identifier node is for `puts = typeDef(__builtin.puts)`
    }
  }

  "CPG for code with namespace resolution being used" should {
    val cpg = code("""
          |Rails.application.configure do
          |  config.log_formatter = ::Logger::Formatter.new
          |end
          |
          |""".stripMargin)

    "recognise all identifier and call nodes" in {
      cpg.call.name("application").size shouldBe 1
      cpg.call.name("configure").size shouldBe 1
      cpg.call.name(Defines.ConstructorMethodName).size shouldBe 1
      cpg.call.name("<operator>.scopeResolution").size shouldBe 2
      cpg.identifier.name("Rails").size shouldBe 1
      cpg.identifier.name("config").size shouldBe 1
      cpg.identifier.name("Formatter").size shouldBe 1
      cpg.identifier.name("Logger").size shouldBe 1
      cpg.identifier.name("log_formatter").size shouldBe 1
      cpg.identifier.size shouldBe 5
    }
  }

  "CPG for code with defined? keyword" should {
    val cpg = code("""
        |radius = 2
        |
        |area = 3.14 * radius * radius
        |
        |# Checking if the variable is defined or not
        |# Using defined? keyword
        |res1 = defined? radius
        |res2 = defined? height
        |res3 = defined? area
        |res4 = defined? Math::PI
        |
        |# Displaying results
        |puts "Result 1: #{res1}"
        |puts "Result 2: #{res2}"
        |puts "Result 3: #{res3}"
        |puts "Result 4: #{res4}"
        |""".stripMargin)

    "recognise all identifier nodes" in {
      cpg.identifier.name("radius").size shouldBe 4
      cpg.identifier.name("area").size shouldBe 2
      cpg.identifier.name("height").size shouldBe 1
      cpg.identifier.name("res1").size shouldBe 2
      cpg.identifier.name("res2").size shouldBe 2
      cpg.identifier.name("res3").size shouldBe 2
      cpg.identifier.name("res4").size shouldBe 2
      cpg.identifier.name("Math").size shouldBe 1
      cpg.identifier.name("PI").size shouldBe 1
    }

    "recognise all literal nodes" in {
      cpg.literal.code("3.14").size shouldBe 1
      cpg.literal.code("2").size shouldBe 1
      cpg.literal.code("Result 1: ").size shouldBe 1
      cpg.literal.code("Result 2: ").size shouldBe 1
      cpg.literal.code("Result 3: ").size shouldBe 1
      cpg.literal.code("Result 4: ").size shouldBe 1
    }
  }

  "CPG for code with association statements" should {
    val cpg = code("""
        |class Employee < EmployeeBase
        |    has_many :teams, foreign_key: "team_id", class_name: "Team"
        |    has_many :worklocations, foreign_key: "location_id", class_name: "WorkLocation"
        |end
        |""".stripMargin)

    "recognise all literal nodes" in {
      cpg.literal
        .code("\"team_id\"")
        .size shouldBe 1
      cpg.literal
        .code("\"location_id\"")
        .size shouldBe 1
    }

    "recognise all activeRecordAssociation operator calls" in {
      cpg.call
        .name("<operator>.activeRecordAssociation")
        .size shouldBe 4
    }
  }

  "CPG for code with class having a scoped constant reference" should {
    val cpg = code("""
        |class ModuleName::ClassName
        |  def some_method
        |    puts "Inside the method"
        |  end
        |end
        |""".stripMargin)

    "recognise all literal nodes" in {
      cpg.literal
        .code("\"Inside the method\"")
        .size shouldBe 1
    }

    "recognise all method nodes" in {
      cpg.method
        .name("some_method")
        .size shouldBe 1
    }
  }

  "CPG for code with alias" should {
    val cpg = code("""
        |def some_method(arg)
        |puts arg
        |end
        |alias :alias_name :some_method
        |alias_name("some param")
        |""".stripMargin)

    "recognise all call nodes" in {
      cpg.call
        .name("puts")
        .size shouldBe 1
      cpg.call
        .name("some_method")
        .size shouldBe 1
    }
  }

  "CPG for code with rescue clause" should {
    val cpg = code("""
        |begin
        |  puts "In begin"
        |rescue SomeException
        |  puts "SomeException occurred"
        |rescue => exceptionVar
        |  puts "Caught exception in variable #{exceptionVar}"
        |rescue
        |  puts "Catch-all block"
        |end
        |
        |""".stripMargin)

    "recognise all literal nodes" in {
      cpg.literal
        .code("\"In begin\"")
        .size shouldBe 1
      cpg.literal
        .code("\"SomeException occurred\"")
        .size shouldBe 1
      cpg.literal
        .code("\"Catch-all block\"")
        .size shouldBe 1
    }

    "recognise all call nodes" in {
      cpg.call
        .name("puts")
        .size shouldBe 4
    }

    "recognise all identifier nodes" in {
      cpg.identifier
        .name("exceptionVar")
        .size shouldBe 2
    }
  }

  "CPG for code with addition of method returns" should {
    val cpg = code("""
        |def num1; 1; end
        |def num2; 2; end
        |def num3; 3; end
        |x = num1 + num2 + num3
        |puts x
        |""".stripMargin)

    "recognise all identifier nodes" in {
      cpg.identifier
        .name("x")
        .size shouldBe 2
    }

    "recognise all call nodes" in {
      cpg.call
        .name("num1")
        .size shouldBe 1

      cpg.call
        .name("num2")
        .size shouldBe 1

      cpg.call
        .name("num3")
        .size shouldBe 1
    }
  }

  "CPG for code with chained constants as argument" should {
    val cpg = code("""
        |SomeFramework.someMethod SomeModule::SomeSubModule::submoduleMethod do
        |puts "nothing important"
        |end
        |""".stripMargin)

    "recognise all method nodes" ignore {
      cpg.method.name("submoduleMethod2").size shouldBe 1
    }

    "recognise all call nodes" in {
      cpg.call
        .name("submoduleMethod")
        .size shouldBe 1

      cpg.call
        .name("puts")
        .size shouldBe 1

      cpg.call
        .name("<operator>.scopeResolution")
        .size shouldBe 1
    }
  }

  "CPG for code with singleton object of some class" ignore {
    val cpg = code("""
        |class << "some_class"
        |end
        |""".stripMargin)

    "recognise all typedecl nodes" in {
      cpg.typeDecl.name("some_class").size shouldBe 1
    }
  }

  // TODO obj.foo="arg" should be interpreted as obj.foo("arg"). code change pending
  "CPG for code with method ending with =" should {
    val cpg = code("""
        |class MyClass
        | def foo=(value)
        | puts value
        | end
        |end
        |
        |obj = MyClass.new
        |obj.foo="arg"
        |""".stripMargin)

    "recognise all call nodes" in {
      cpg.call.name("puts").size shouldBe 1
      cpg.call.name("<operator>.fieldAccess").size shouldBe 1
    }

    "recognise all method nodes" in {
      cpg.method.name("foo=").size shouldBe 1
    }
  }

  // expectation is that this should not cause a crash
  "CPG for code with method having a singleton class" should {
    val cpg = code("""
        |module SomeModule
        |    def self.someMethod(arg)
        |      class << arg
        |      end
        |    end
        |end
        |""".stripMargin)

    "recognise all namespace nodes" in {
      cpg.namespace.name("SomeModule").size shouldBe 1
    }
  }

  "CPG for code with super without arguments" should {
    val cpg = code("""
        |class Parent
        |  def foo(arg)
        |  end
        |end
        |
        |class Child < Parent
        |  def foo(arg)
        |    super
        |  end
        |end
        |""".stripMargin)

    "recognise all call nodes" in {
      cpg.call.name("<operator>.super").size shouldBe 1
      cpg.method.name("foo").size shouldBe 2
    }
  }
}
