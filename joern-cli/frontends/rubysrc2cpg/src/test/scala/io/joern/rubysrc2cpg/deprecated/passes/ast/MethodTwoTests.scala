package io.joern.rubysrc2cpg.deprecated.passes.ast

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.{EvaluationStrategies, NodeTypes}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

class MethodTwoTests extends RubyCode2CpgFixture(useDeprecatedFrontend = true) {

  "Method test with define_method" should {
    val cpg = code("""
        |define_method(:foo) do |a, b|
        |  return ""
        |end
        |""".stripMargin)

    // TODO: This test cases needs to be fixed.
    "should contain exactly one method node with correct fields" ignore {
      inside(cpg.method.name("foo").l) { case List(x) =>
        x.name shouldBe "foo"
        x.isExternal shouldBe false
        x.fullName shouldBe "Test0.rb::program:foo"
        x.code should startWith("def foo(a, b)")
        x.isExternal shouldBe false
        x.order shouldBe 1
        x.filename.endsWith("Test0.rb")
        x.lineNumber shouldBe Option(2)
        x.lineNumberEnd shouldBe Option(4)
      }
    }

    // TODO: This test cases needs to be fixed.
    "should return correct number of lines" ignore {
      cpg.method.name("foo").numberOfLines.l shouldBe List(3)
    }

    // TODO: This test cases needs to be fixed.
    "should allow traversing to parameters" ignore {
      cpg.method.name("foo").parameter.name.toSetMutable shouldBe Set("a", "b")
    }

    // TODO: This test cases needs to be fixed.
    "should allow traversing to methodReturn" ignore {
      cpg.method.name("foo").methodReturn.l.size shouldBe 1
      cpg.method.name("foo").methodReturn.typeFullName.head shouldBe "ANY"
    }

    // TODO: This test cases needs to be fixed.
    "should allow traversing to method" ignore {
      cpg.methodReturn.method.name.l shouldBe List("foo", ":program")
    }

    // TODO: This test cases needs to be fixed.
    "should allow traversing to file" ignore {
      cpg.method.name("foo").file.name.l should not be empty
    }

    // TODO: Need to be fixed
    "test function method ref" ignore {
      cpg.methodRefWithName("foo").referencedMethod.fullName.l should not be empty
      cpg.methodRefWithName("foo").referencedMethod.fullName.head shouldBe
        "Test0.rb::program:foo"
    }

    // TODO: Need to be fixed.
    "test existence of local variable in module function" ignore {
      cpg.method.fullName("Test0.rb::program").local.name.l should contain("foo")
    }

    // TODO: need to be fixed.
    "test corresponding type, typeDecl and binding" ignore {
      cpg.method.fullName("Test0.rb::program:foo").referencingBinding.bindingTypeDecl.l should not be empty
      val bindingTypeDecl =
        cpg.method.fullName("Test0.rb::program:foo").referencingBinding.bindingTypeDecl.head

      bindingTypeDecl.name shouldBe "foo"
      bindingTypeDecl.fullName shouldBe "Test0.rb::program:foo"
      bindingTypeDecl.referencingType.name.head shouldBe "foo"
      bindingTypeDecl.referencingType.fullName.head shouldBe "Test0.rb::program:foo"
    }

    // TODO: Need to be fixed
    "test method parameter nodes" ignore {

      cpg.method.name("foo").parameter.name.l.size shouldBe 2
      val parameter1 = cpg.method.fullName("Test0.rb::program:foo").parameter.order(1).head
      parameter1.name shouldBe "a"
      parameter1.index shouldBe 1
      parameter1.typeFullName shouldBe "ANY"

      val parameter2 = cpg.method.fullName("Test0.rb::program:foo").parameter.order(2).head
      parameter2.name shouldBe "b"
      parameter2.index shouldBe 2
      parameter2.typeFullName shouldBe "ANY"
    }

    // TODO: Need to be fixed
    "should allow traversing from parameter to method" ignore {
      cpg.parameter.name("a").method.name.l shouldBe List("foo")
      cpg.parameter.name("b").method.name.l shouldBe List("foo")
    }
  }
}
