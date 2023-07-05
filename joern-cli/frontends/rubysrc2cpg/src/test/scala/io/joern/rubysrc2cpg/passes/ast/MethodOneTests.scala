package io.joern.rubysrc2cpg.passes.ast

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import io.shiftleft.codepropertygraph.generated.NodeTypes
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

class MethodOneTests extends RubyCode2CpgFixture {

  "Method test with regular keyword def and end " should {
    val cpg = code("""
        |def foo(a, b)
        |  return ""
        |end
        |""".stripMargin)

    "should contain exactly one method node with correct fields" in {
      inside(cpg.method.name("foo").l) { case List(x) =>
        x.name shouldBe "foo"
        x.isExternal shouldBe false
        x.fullName shouldBe "Test0.rb::program:foo"
        x.code should startWith("def foo(a, b)")
        x.isExternal shouldBe false
        x.order shouldBe 1
        x.filename endsWith "Test0.rb"
        x.lineNumber shouldBe Option(2)
        x.lineNumberEnd shouldBe Option(4)
      }
    }

    "should return correct number of lines" in {
      cpg.method.name("foo").numberOfLines.l shouldBe List(3)
    }

    // TODO: This test cases needs to be fixed.
    "should allow traversing to parameters" ignore {
      cpg.method.name("foo").parameter.name.toSetMutable shouldBe Set("a", "b")
    }

    "should allow traversing to methodReturn" in {
      cpg.method.name("foo").methodReturn.l.size shouldBe 1
      cpg.method.name("foo").methodReturn.typeFullName.head shouldBe "ANY"
    }

    "should allow traversing to method" in {
      cpg.methodReturn.method.name.l shouldBe List("foo", ":program")
    }

    "should allow traversing to file" in {
      cpg.method.name("foo").file.name.l should not be empty
    }

    // TODO: Need to be fixed
    "test function method ref" ignore {
      cpg.methodRef("foo").referencedMethod.fullName.l should not be empty
      cpg.methodRef("foo").referencedMethod.fullName.head shouldBe
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
      // TODO: its not working with "b"
      cpg.parameter.name("b").method.name.l shouldBe List("foo")
    }
  }

  "Method with variable arguments" should {
    val cpg = code("""
        |def foo(*names)
        |  return ""
        |end
        |""".stripMargin)

    // TODO: Need to be fixed
    "Variable argument properties should be rightly set" ignore {
      cpg.parameter.name("names").l.size shouldBe 1
      val param = cpg.parameter.name("names").l.head
      param.isVariadic shouldBe true
    }
  }

  "Multiple Return tests" should {
    val cpg = code("""
        |def foo(names)
        |  if names == "Alice"
        |    return 1
        |  else
        |    return 2
        |end
        |""".stripMargin)

    // TODO: Need to be fixed.
    "be correct for multiple returns" ignore {
      cpg.method("foo").methodReturn.l.size shouldBe 2
      inside(cpg.method("foo").methodReturn.l) { case List(mainMethodReturn) =>
        mainMethodReturn.typeFullName shouldBe "ANY"
      }
      val astReturns  = cpg.method("foo").ast.isReturn.l
      val cfgReturns  = cpg.method("foo").methodReturn.cfgPrev.l
      val travReturns = cpg.method("foo").methodReturn.toReturn.l
      inside(astReturns) { case List(ret1, ret2) =>
        ret1.code shouldBe "return 1"
        ret1.lineNumber shouldBe Option(4)
        ret2.code shouldBe "return 2"
        ret2.lineNumber shouldBe Option(6)
      }
      astReturns shouldBe cfgReturns
      astReturns shouldBe travReturns
    }
  }
}
