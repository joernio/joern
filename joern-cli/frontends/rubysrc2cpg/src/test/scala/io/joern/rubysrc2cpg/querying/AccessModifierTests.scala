package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.passes.Defines
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.semanticcpg.language.*

class AccessModifierTests extends RubyCode2CpgFixture {

  "methods defined on the <main> level are private" in {
    val cpg = code("""
        |def foo
        |end
        |""".stripMargin)

    cpg.method("foo").head.isPrivate.size shouldBe 1
  }

  "a method should be public by default, with the `initialize` default constructor private" in {
    val cpg = code("""
        |class Foo
        | def bar
        | end
        |end
        |""".stripMargin)

    cpg.method("bar").head.isPublic.size shouldBe 1
    cpg.method(Defines.Initialize).head.isPrivate.size shouldBe 1
    cpg.method(Defines.TypeDeclBody).head.isPrivate.size shouldBe 1
  }

  "an access modifier should affect the visibility of subsequent method definitions" in {
    val cpg = code("""
        |class Foo
        | def bar
        | end
        |
        | private
        |
        | def baz
        | end
        |
        | def faz
        | end
        |
        |end
        |
        |class Baz
        | def test1
        | end
        |
        | protected
        |
        | def test2
        | end
        |end
        |""".stripMargin)

    cpg.method("bar").head.isPublic.size shouldBe 1

    cpg.method("baz").head.isPrivate.size shouldBe 1
    cpg.method("faz").head.isPrivate.size shouldBe 1

    cpg.method("test1").head.isPublic.size shouldBe 1
    cpg.method("test2").head.isProtected.size shouldBe 1
  }

  "nested types should 'remember' their access modifier mode according to scope" in {
    val cpg = code("""
        |class Foo
        | private
        |
        | class Bar
        |
        |   public
        |   def baz
        |   end
        |
        | end
        |
        | def test
        | end
        |end
        |""".stripMargin)

    cpg.method("baz").isPublic.size shouldBe 1
    cpg.method("test").isPrivate.size shouldBe 1
  }

  "an identifier sharing the same name as an access modifier in an unambiguous spot should not be confused" in {
    val cpg = code("""
        |  def message_params
        |    {
        |      private: @private
        |    }
        |  end
        |""".stripMargin)

    val privateKey  = cpg.literal(":private").head
    val indexAccess = privateKey.astParent.asInstanceOf[Call]
    indexAccess.name shouldBe Operators.indexAccess
    indexAccess.methodFullName shouldBe Operators.indexAccess
    indexAccess.code shouldBe "<tmp-0>[:private]"
  }

}
