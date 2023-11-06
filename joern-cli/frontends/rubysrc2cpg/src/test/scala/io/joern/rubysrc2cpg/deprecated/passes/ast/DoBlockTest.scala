package io.joern.rubysrc2cpg.deprecated.passes.ast

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class DoBlockTest extends RubyCode2CpgFixture(useDeprecatedFrontend = true) {

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

}
