package io.joern.rubysrc2cpg.passes.ast

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

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

}
