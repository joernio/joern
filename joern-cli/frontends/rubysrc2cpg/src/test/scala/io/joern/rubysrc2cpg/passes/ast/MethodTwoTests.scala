package io.joern.rubysrc2cpg.passes.ast

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class MethodTwoTests extends RubyCode2CpgFixture {

  "Method test with define_method" should {
    val cpg = code("""
        |define_method(:foo) do |a, b|
        |  return ""
        |end
        |""".stripMargin)

    "should contain exactly one method node with correct fields" in {
      inside(cpg.method.name("foo").l) { case List(x) =>
        x.name shouldBe "foo"
        x.isExternal shouldBe false
        x.fullName shouldBe "Test0.rb::program.foo"
        x.code should startWith("return \"\"")
        x.isExternal shouldBe false
        x.order shouldBe 2
        x.filename endsWith "Test0.rb"
        x.lineNumber shouldBe Option(2)
        x.lineNumberEnd shouldBe Option(4)
      }
    }

    "should return correct number of lines" in {
      cpg.method.name("foo").numberOfLines.l shouldBe List(3)
    }

    "should allow traversing to parameters" in {
      cpg.method.name("foo").parameter.name.toSetMutable shouldBe Set("a", "b")
    }

    "should allow traversing to methodReturn" in {
      cpg.method.name("foo").methodReturn.l.size shouldBe 1
      cpg.method.name("foo").methodReturn.typeFullName.head shouldBe "ANY"
    }

    "should allow traversing to method" in {
      cpg.methodReturn.method.isExternal(false).name.l shouldBe List("foo", ":program")
    }

    "should allow traversing to file" in {
      cpg.method.name("foo").file.name.l should not be empty
    }

    "test function method ref" in {
      cpg.methodRef("foo").referencedMethod.fullName.l should not be empty
      cpg.methodRef("foo").referencedMethod.fullName.head shouldBe "Test0.rb::program.foo"
    }

    "test existence of local variable in module function" in {
      cpg.method.fullName("Test0.rb::program").local.name.l should contain("foo")
    }

    "test corresponding type, typeDecl and binding" in {
      cpg.method.fullName("Test0.rb::program.foo").referencingBinding.bindingTypeDecl.l should not be empty
      val bindingTypeDecl =
        cpg.method.fullName("Test0.rb::program.foo").referencingBinding.bindingTypeDecl.head

      bindingTypeDecl.name shouldBe "foo"
      bindingTypeDecl.fullName shouldBe "Test0.rb::program.foo"
      bindingTypeDecl.referencingType.name.head shouldBe "foo"
      bindingTypeDecl.referencingType.fullName.head shouldBe "Test0.rb::program.foo"
    }

    "test method parameter nodes" in {

      cpg.method.name("foo").parameter.name.l.size shouldBe 2
      val parameter1 = cpg.method.fullName("Test0.rb::program.foo").parameter.order(1).head
      parameter1.name shouldBe "a"
      parameter1.index shouldBe 1
      parameter1.typeFullName shouldBe "ANY"

      val parameter2 = cpg.method.fullName("Test0.rb::program.foo").parameter.order(2).head
      parameter2.name shouldBe "b"
      parameter2.index shouldBe 2
      parameter2.typeFullName shouldBe "ANY"
    }

    "should allow traversing from parameter to method" in {
      cpg.parameter.name("a").method.name.l shouldBe List("foo")
      cpg.parameter.name("b").method.name.l shouldBe List("foo")
    }
  }
}
