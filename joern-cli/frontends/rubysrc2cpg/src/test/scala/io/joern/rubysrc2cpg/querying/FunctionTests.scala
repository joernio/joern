package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language._

class FunctionTests extends RubyCode2CpgFixture {

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

    "recognise all identifier nodes" in {
      cpg.identifier.name("name").l.size shouldBe 1
      cpg.identifier.name("age").l.size shouldBe 1
      cpg.identifier.name("@name").l.size shouldBe 2
      cpg.identifier.name("@age").l.size shouldBe 4
      cpg.identifier.size shouldBe 11
    }

    "recognize all call nodes" in {
      cpg.call.name("greet").size shouldBe 1
      cpg.call.name("puts").size shouldBe 2
    }

    "recognize all method nodes" in {
      cpg.method.name("initialize").size shouldBe 1
      cpg.method.name("greet").size shouldBe 1
      cpg.method.name("have_birthday").size shouldBe 1
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

    "recognise all method nodes" in {
      cpg.method.name("\\[]").size shouldBe 1
      cpg.method.name("\\[]=").size shouldBe 1
      cpg.method.name("initialize").size shouldBe 1
    }

    "recognize all call nodes" in {
      cpg.call.name(Operators.assignment).size shouldBe 3
      cpg.call.name("to_s").size shouldBe 2
      cpg.call.name("new").size shouldBe 1
      cpg.call.size shouldBe 8
    }

    "recognize all identifier nodes" in {
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

    "recognise all method nodes defined in modules" in {
      cpg.method.name("method1_1").l.size shouldBe 1
      cpg.method.name("method1_2").l.size shouldBe 1
      cpg.method.name("method2_1").l.size shouldBe 1
      cpg.method.name("method2_2").l.size shouldBe 1
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
        .size shouldBe 1
      cpg.method
        .name("method1")
        .size shouldBe 1
      cpg.method
        .name("method3")
        .size shouldBe 1

    }
  }
}
