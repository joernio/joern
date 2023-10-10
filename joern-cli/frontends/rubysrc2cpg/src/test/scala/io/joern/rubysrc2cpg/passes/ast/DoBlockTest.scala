package io.joern.rubysrc2cpg.passes.ast

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Call, ControlStructure, Identifier, MethodRef}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, NodeTypes}

class DoBlockTest extends RubyCode2CpgFixture {

  "defining a method using metaprogramming and a do-block function" should {
    val cpg = code(s"""
        |define_method foo do |name, age|
        | value = public_send("#{name}_value")
        | unit = public_send("#{name}_unit")
        |
        | puts "My name is #{name} and age is #{age}"
        |
        | next unless value.present? && unit.present?
        | value.public_send(unit)
        |end
        |""".stripMargin)

    "create a do-block method called `foo`" in {
      val nameMethod :: _ = cpg.method.nameExact("foo").l: @unchecked

      val List(name, age) = nameMethod.parameter.l
      name.name shouldBe "name"
      age.name shouldBe "age"

      val List(value, unit) = nameMethod.local.l
      value.name shouldBe "value"
      unit.name shouldBe "unit"
    }
  }

  "a do-block function used as a higher-order function" should {
    val cpg = code("""class TransactionsController < ApplicationController
        |  def permitted_column_name(column_name)
        |    %w[trx_date description amount].find { |permitted| column_name == permitted } || 'trx_date'
        |  end
        |end
        |
        |""".stripMargin)

    "create a do-block method named from the surrounding function" in {
      val findMethod :: _ = cpg.method.name("find.*").l: @unchecked
      findMethod.name should startWith("find")
      findMethod.parameter.size shouldBe 1
      val permitParam :: _ = findMethod.parameter.l: @unchecked
      permitParam.name shouldBe "permitted"
    }

  }

  "a do-block function wrapped within an active record association" should {
    val cpg = code("""
        |refunds = []
        |attrs = {
        | refunds: refunds.sort_by! { |r| r["created"] }
        |}
        |""".stripMargin)

    "create a do-block method named from the surrounding function" in {
      val findMethod :: _ = cpg.method.name("sort_by.*").l: @unchecked
      findMethod.name should startWith("sort_by")
      findMethod.parameter.size shouldBe 1
      val permitParam :: _ = findMethod.parameter.l: @unchecked
      permitParam.name shouldBe "r"
    }
  }

  "a do-block function wrapped within a chained invocation inside of a call argument" should {
    val cpg = code("OpenStruct.new(obj.map { |key, val| [key, to_recursive_ostruct(val)] }.to_h)")

    "create a do-block method named from the surrounding function" in {
      val mapMethod :: _ = cpg.method.name("map.*").l: @unchecked
      mapMethod.name should startWith("map")
      mapMethod.parameter.size shouldBe 2
      val k :: v :: _ = mapMethod.parameter.l: @unchecked
      k.name shouldBe "key"
      v.name shouldBe "val"
    }
  }

  "chained higher-order functions as do-block functions" should {
    val cpg = code("""
        |xs
        | .select { |x| x.foo }
        | .each { |y| puts(y) }
        |""".stripMargin)

    "chain the two methods calls where one is the argument of the other" in {
      val selectCall = cpg.call.nameExact("select").head
      val eachCall   = cpg.call.nameExact("each").head
      selectCall.astParent shouldBe eachCall

      selectCall.astChildren.l match
        case ::(xs: Identifier, ::(selectRef: MethodRef, Nil)) =>
          xs.name shouldBe "xs"
          selectRef.referencedMethod.name should startWith("select")
        case _ => fail("'select' call children are not what is expected.")

      eachCall.astChildren.l match
        case ::(sCall: Call, ::(eachRef: MethodRef, Nil)) =>
          sCall shouldBe selectCall
          eachRef.referencedMethod.name should startWith("each")
        case _ => fail("'each' call children are not what is expected.")
    }
  }

  "a boolean do-block function as a conditional argument" should {
    val cpg = code("""
        |if @items.any? { |x| x > 1 }
        | puts "foo"
        |else
        | puts "bar"
        |end
        |""".stripMargin)

    "be defined outside of the control structure" in {
      val anyMethod = cpg.method.name("any.*").head
      anyMethod.astParent.label shouldBe NodeTypes.BLOCK
    }

    "have the call to the method ref as the conditional argument" in {
      val ifStmt               = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).head
      val ::(anyCall: Call, _) = ifStmt.condition.l: @unchecked
      anyCall.name should startWith("any")
    }
  }

}
