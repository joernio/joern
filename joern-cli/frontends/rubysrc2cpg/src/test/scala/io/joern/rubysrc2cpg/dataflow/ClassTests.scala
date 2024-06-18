package io.joern.rubysrc2cpg.dataflow

import io.joern.dataflowengineoss.language.*
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class ClassTests extends RubyCode2CpgFixture(withPostProcessing = true, withDataFlow = true) {
  // Works in deprecated
  "Data flow through class member" ignore {
    val cpg = code("""
        |class MyClass
        | @instanceVariable
        |
        | def initialize(value)
        |        @instanceVariable = value
        | end
        |
        | def getValue()
        |        @instanceVariable
        | end
        |end
        |
        |x = 12345
        |inst = MyClass.new(x)
        |y = inst.getValue
        |puts y
        |""".stripMargin)

    val src  = cpg.identifier.name("x").l
    val sink = cpg.call.name("puts").l
    sink.reachableByFlows(src).l.size shouldBe 2
  }

  "flow through module method" in {
    val cpg = code("""
                     |module MyModule
                     |  def MyModule.print(text)
                     |    puts text
                     |  end
                     |end
                     |
                     |x = "some text"
                     |
                     |MyModule::print(x)
                     |""".stripMargin)

    val src  = cpg.identifier.name("x").l
    val sink = cpg.call.name("puts").l
    sink.reachableByFlows(src).l.size shouldBe 2
  }

  "Data flow through chained scoped constant reference" in {
    val cpg = code("""
                     |module SomeModule
                     |SomeConstant = 1
                     |end
                     |
                     |x = 1
                     |y = SomeModule::SomeConstant * x
                     |puts y
                     |
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }

  // Works in deprecated
  "Data flow through scopedConstantAccessSingleLeftHandSide" ignore {
    val cpg = code("""
                     |SomeConstant = 1
                     |
                     |x = 1
                     |::SomeConstant = x
                     |y = ::SomeConstant + 10
                     |puts y
                     |
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }

  // TODO: This test is invalid as constants cannot be re-assigned
  "Data flow through xdotySingleLeftHandSide through a constant on left of the ::" ignore {
    val cpg = code("""
                     |module SomeModule
                     |  SomeConstant = 100
                     |end
                     |
                     |x = 2
                     |SomeModule::SomeConstant = x
                     |y = SomeModule::SomeConstant
                     |puts y
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }

  // TODO:
  "Data flow through xdotySingleLeftHandSide through a local on left of the ::" ignore {
    val cpg = code("""
                     |module SomeModule
                     |SomeConstant = 100
                     |end
                     |
                     |x = 2
                     |local = SomeModule
                     |local::SomeConstant = x
                     |y = SomeModule::SomeConstant
                     |puts y
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }

  // Works in deprecated
  "Data flow with super usage" ignore {
    val cpg = code("""
                     |class BaseClass
                     |  def doSomething(arg)
                     |    return arg + 10
                     |  end
                     |end
                     |
                     |class DerivedClass < BaseClass
                     |  def doSomething(arg)
                     |    super(arg)
                     |  end
                     |end
                     |
                     |x = 1
                     |object = DerivedClass.new
                     |y = object.doSomething(x)
                     |puts y
                     |
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }

  "Data flow using a keyword" in {
    val cpg = code("""
                     |class MyClass
                     |end
                     |
                     |x = MyClass.new
                     |y = x.class
                     |puts y
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }

  // TODO:
  "Flow for a global variable" ignore {
    val cpg = code("""
                     |$person_height = 6
                     |class Person
                     |    def height_in_cm
                     |        puts $person_height * 30
                     |    end
                     |end
                     |""".stripMargin)

    val source = cpg.identifier.name("$person_height").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }

  "dataflow in method defined under class << self block" in {
    val cpg = code("""
       class MyClass
                     |
                     |  class << self
                     |    def printPII
                     |      firstName="somename"
                     |      puts "log PII #{firstName}"
                     |    end
                     |  end
                     |end
                     |
                     |MyClass.printPII""".stripMargin)

    val source = cpg.identifier.name("firstName").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }

  "flow through special prefix methods" ignore {
    /* We only check private_class_method here. The mechanism is similar to others:
     *     attr_reader
     *     attr_writer
     *     attr_accessor
     *     remove_method
     *     public_class_method
     *     private
     *     protected
     */
    val cpg = code("""
                     |class Foo
                     | z = 1
                     | private_class_method def self.bar(x)
                     |   x
                     | end
                     |
                     | y = self.bar(z)
                     | puts y
                     |end
                     |""".stripMargin)

    val source = cpg.identifier.name("z").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }
}
