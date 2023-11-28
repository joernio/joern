package io.joern.rubysrc2cpg.deprecated.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*

class FunctionTests extends RubyCode2CpgFixture(useDeprecatedFrontend = true) {

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
        |p = Person.new
        |p.greet
        |""".stripMargin)

    "recognise all identifier nodes" in {
      cpg.identifier.name("name").size shouldBe 1
      cpg.identifier.name("age").size shouldBe 1
      cpg.fieldAccess.fieldIdentifier.canonicalName("name").size shouldBe 2
      cpg.fieldAccess.fieldIdentifier.canonicalName("age").size shouldBe 4
      cpg.identifier.size shouldBe 13 // 4 identifier node is for `puts = typeDef(__builtin.puts)` 1 node for class Person = typeDef
    }

    "recognize all call nodes" in {
      cpg.call.name("greet").size shouldBe 1
      cpg.call.name("puts").size shouldBe 2
    }

    "recognize all method nodes" in {
      // Initialize => <init>
      cpg.method.name("initialize").size shouldBe 0
      cpg.method.name(Defines.ConstructorMethodName).size shouldBe 1
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
      cpg.method.name("initialize").size shouldBe 0
      cpg.method.name(Defines.ConstructorMethodName).size shouldBe 1
    }

    "recognize all call nodes" in {
      cpg.call
        .name(Operators.assignment)
        .size shouldBe 4 //  +1 identifier node for TypeRef's assignment
      cpg.call.name("to_s").size shouldBe 2
      cpg.call.name(Defines.ConstructorMethodName).size shouldBe 1
      cpg.call.size shouldBe 12 // 1 identifier node for TypeRef's assignment
    }

    "recognize all identifier nodes" in {
      cpg.fieldAccess.fieldIdentifier.canonicalName("my_hash").size shouldBe 3
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

  "CPG for code with multiple yields" should {
    val cpg = code("""
        |def yield_with_arguments
        |  x = "something"
        |  y = "something_else"
        |  yield(x,y)
        |end
        |
        |yield_with_arguments { |arg1, arg2| puts "Yield block 1 #{arg1} and #{arg2}" }
        |yield_with_arguments { |arg1, arg2| puts "Yield block 2 #{arg2} and #{arg1}" }
        |
        |""".stripMargin)

    "recognise all method nodes" in {
      cpg.method
        .name("yield_with_arguments")
        .size shouldBe 1
      cpg.method
        .name("yield_with_arguments_yield")
        .size shouldBe 2
    }

    "recognise all call nodes" in {
      cpg.call
        .name("yield_with_arguments_yield")
        .size shouldBe 1

      cpg.call
        .name("puts")
        .size shouldBe 2
    }

    "recognise all identifier nodes" in {
      cpg.identifier
        .name("arg1")
        .size shouldBe 2

      cpg.identifier
        .name("arg2")
        .size shouldBe 2

      cpg.identifier
        .name("x")
        .size shouldBe 2

      cpg.identifier
        .name("y")
        .size shouldBe 2
    }
  }
}
