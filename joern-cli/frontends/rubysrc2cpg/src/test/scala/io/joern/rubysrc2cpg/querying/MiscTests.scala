package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language._

class MiscTests extends RubyCode2CpgFixture {

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
      cpg.identifier.name("beginvar").l.size shouldBe 2
      cpg.identifier.name("endvar").l.size shouldBe 2
      cpg.identifier.name("beginbool").l.size shouldBe 1
      cpg.identifier.name("endbool").l.size shouldBe 1
      cpg.call.name("puts").size shouldBe 1
      cpg.identifier.size shouldBe 6
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
      cpg.call.name("new").size shouldBe 1
      cpg.call.name("<operator>.scopeResolution").size shouldBe 2
      cpg.identifier.name("Rails").l.size shouldBe 1
      cpg.identifier.name("config").l.size shouldBe 1
      cpg.identifier.name("Formatter").l.size shouldBe 1
      cpg.identifier.name("Logger").l.size shouldBe 1
      cpg.identifier.name("log_formatter").l.size shouldBe 1
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
      cpg.identifier.name("radius").l.size shouldBe 4
      cpg.identifier.name("area").l.size shouldBe 2
      cpg.identifier.name("height").l.size shouldBe 1
      cpg.identifier.name("res1").l.size shouldBe 2
      cpg.identifier.name("res2").l.size shouldBe 2
      cpg.identifier.name("res3").l.size shouldBe 2
      cpg.identifier.name("res4").l.size shouldBe 2
      cpg.identifier.name("Math").l.size shouldBe 1
      cpg.identifier.name("PI").l.size shouldBe 1
    }

    "recognise all literal nodes" in {
      cpg.literal.code("3.14").l.size shouldBe 1
      cpg.literal.code("2").l.size shouldBe 1
      cpg.literal.code("Result 1: ").l.size shouldBe 1
      cpg.literal.code("Result 2: ").l.size shouldBe 1
      cpg.literal.code("Result 3: ").l.size shouldBe 1
      cpg.literal.code("Result 4: ").l.size shouldBe 1
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
        .l
        .size shouldBe 1
      cpg.literal
        .code("\"location_id\"")
        .l
        .size shouldBe 1
    }

    "recognise all activeRecordAssociation operator calls" in {
      cpg.call
        .name("<operator>.activeRecordAssociation")
        .l
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
        .l
        .size shouldBe 1
    }

    "recognise all method nodes" in {
      cpg.method
        .name("some_method")
        .l
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
        .l
        .size shouldBe 1
      cpg.call
        .name("some_method")
        .l
        .size shouldBe 1
    }
  }

  "CPG for code with rescue clause" should {
    val cpg = code("""
        |begin
        |  puts "In begin"
        |rescue SomeException
        |  puts "SomeException occurred"
        |rescue => SomeOtherException
        |  puts "SomeOtherException occurred"
        |rescue
        |  puts "Catch-all block"
        |end
        |
        |""".stripMargin)

    "recognise all literal nodes" in {
      cpg.literal
        .code("\"In begin\"")
        .l
        .size shouldBe 1
      cpg.literal
        .code("\"SomeException occurred\"")
        .l
        .size shouldBe 1
      cpg.literal
        .code("\"SomeOtherException occurred\"")
        .l
        .size shouldBe 1
      cpg.literal
        .code("\"Catch-all block\"")
        .l
        .size shouldBe 1
    }

    "recognise all call nodes" in {
      cpg.call
        .name("puts")
        .l
        .size shouldBe 4
    }
  }

  "CPG for code with addition of method returns" should {
    val cpg = code(
      """
        |def num1; 1; end
        |def num2; 2; end
        |def num3; 3; end
        |x = num1 + num2 + num3
        |puts x
        |""".stripMargin)

    "recognise all identifier nodes" in {
      cpg.identifier
        .name("x")
        .l
        .size shouldBe 2
    }

    "recognise all call nodes" in {
      cpg.call
        .name("num1")
        .l
        .size shouldBe 1

      cpg.call
        .name("num2")
        .l
        .size shouldBe 1

      cpg.call
        .name("num3")
        .l
        .size shouldBe 1
    }
  }
}
