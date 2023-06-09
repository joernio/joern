package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language._

class MiscTests extends RubyCode2CpgFixture {

  "CPG for code with method identifiers and literals in simple assignments" should {
    val cpg = code("""
        |# call instance methods
        |a = 1
        |b = 2
        |a = 3
        |b = 4
        |c = a*b
        |puts "Multiplication is : #{c}"
        |""".stripMargin)

    "recognise all identifier nodes" in {
      cpg.identifier.name("a").l.size shouldBe 3
      cpg.identifier.name("b").l.size shouldBe 3
      cpg.identifier.name("c").l.size shouldBe 2
    }

    "recognise all literal nodes" in {
      cpg.literal.code("1").l.size shouldBe 1
      cpg.literal.code("2").l.size shouldBe 1
      cpg.literal.code("3").l.size shouldBe 1
      cpg.literal.code("4").l.size shouldBe 1
    }
  }

  "CPG for code with class methods, members and locals in methods" should {
    val cpg = code("""
        |class Person
        |  attr_accessor :name, :age
        |
        |  def initialize(name, age)
        |    @name = name
        |    @age = age
        |  end
        |
        |  def greet
        |    puts "Hello, my name is #{@name} and I am #{@age} years old."
        |  end
        |
        |  def have_birthday
        |    @age += 1
        |    puts "Happy birthday! You are now #{@age} years old."
        |  end
        |end
        |
        |p = Person. new
        |p.greet
        |""".stripMargin)

    "recognise all identifier and call nodes" in {
      cpg.identifier.name("name").l.size shouldBe 1
      cpg.identifier.name("age").l.size shouldBe 1
      cpg.identifier.name("@name").l.size shouldBe 2
      cpg.identifier.name("@age").l.size shouldBe 4
      cpg.call.name("greet").size shouldBe 1
      cpg.method.name("initialize").size shouldBe 1
      cpg.method
        .name("greet")
        .size shouldBe 1
      cpg.call.name("puts").size shouldBe 2
      cpg.method.name("have_birthday").size shouldBe 1
      cpg.identifier.size shouldBe 11
    }
  }

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

  "CPG for code with square brackets as methods" should {
    val cpg = code("""
          |class MyClass < MyBaseClass
          |  def initialize
          |    @my_hash = {}
          |  end
          |
          |  def [](key)
          |    @my_hash[key.to_s]
          |  end
          |
          |  def []=(key, value)
          |    @my_hash[key.to_s] = value
          |  end
          |end
          |
          |my_object = MyClass.new
          |
          |""".stripMargin)

    "recognise all identifier and call nodes" in {
      cpg.method.name("\\[]").size shouldBe 1
      cpg.method.name("\\[]=").size shouldBe 1
      cpg.call.name(Operators.assignment).size shouldBe 3
      cpg.method.name("initialize").size shouldBe 1
      cpg.call.name("to_s").size shouldBe 2
      cpg.call.name("new").size shouldBe 1
      cpg.call.size shouldBe 9
      cpg.identifier.name("@my_hash").size shouldBe 3
      cpg.identifier.name("key").size shouldBe 2
      cpg.identifier.name("value").size shouldBe 1
      cpg.identifier.name("my_object").size shouldBe 1
      /*
       * FIXME
       *  def []=(key, value) gets parsed incorrectly with parser error "no viable alternative at input 'def []=(key, value)'"
       *  This needs a fix in the parser and update to this UT after the fix
       * FIXME
       *  MyClass is identified as a variableIdentifier and so an identifier. This needs to be fixed
       */
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

  "CPG for code with multiple assignments" should {
    val cpg = code("""
        |a, b, c = [1, 2, 3]
        |a, b, c = b, c, a
        |str1, str2 = ["hello", "world"]
        |""".stripMargin)

    "recognise all identifier nodes" in {
      cpg.identifier.name("a").l.size shouldBe 3
      cpg.identifier.name("b").l.size shouldBe 3
      cpg.identifier.name("c").l.size shouldBe 3
      cpg.identifier.name("str1").l.size shouldBe 1
      cpg.identifier.name("str2").l.size shouldBe 1
    }

    "recognise all literal nodes" in {
      cpg.literal.code("1").l.size shouldBe 1
      cpg.literal.code("2").l.size shouldBe 1
      cpg.literal.code("3").l.size shouldBe 1
      cpg.literal.code("\"hello\"").l.size shouldBe 1
      cpg.literal.code("\"world\"").l.size shouldBe 1
    }

    "recognise all call nodes" in {
      cpg.call.name(Operators.assignment).l.size shouldBe 3
    }
  }

  "CPG for code with modules" should {
    val cpg = code("""
        |module Module1
        |   def method1_1
        |   end
        |   def method1_2
        |   end
        |end
        |
        |module Module2
        |   def method2_1
        |   end
        |   def method2_2
        |   end
        |end
        |""".stripMargin)

    "recognise all method nodes" in {
      cpg.method.name("method1_1").l.size shouldBe 1
      cpg.method.name("method1_2").l.size shouldBe 1
      cpg.method.name("method2_1").l.size shouldBe 1
      cpg.method.name("method2_2").l.size shouldBe 1
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

  "CPG for code with private/protected/public" should {
    val cpg = code("""
        |class SomeClass
        |  private
        |  def method1
        |  end
        |
        |  protected
        |  def method2
        |  end
        |
        |  public
        |  def method3
        |  end
        |end
        |
        |""".stripMargin)

    "recognise all method nodes" in {
      cpg.method
        .name("method1")
        .l
        .size shouldBe 1
      cpg.method
        .name("method1")
        .l
        .size shouldBe 1
      cpg.method
        .name("method3")
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
