package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.passes.Defines
import io.joern.rubysrc2cpg.passes.Defines.RubyOperators
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.{ModifierTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Literal}
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

  "a bare access modifier at type level should create an operator call" in {
    val cpg = code("""
        |class Foo
        | private
        |
        | def bar
        | end
        |end
        |""".stripMargin)

    inside(cpg.call.nameExact(RubyOperators.privateModifier).l) { case modifierCall :: Nil =>
      modifierCall.methodFullName shouldBe RubyOperators.privateModifier
      modifierCall.code shouldBe "private"
    }

    cpg.method("bar").head.isPrivate.size shouldBe 1
  }

  "a bare access modifier at type level and on the same line as a method decl should create an operator call" in {
    val cpg = code("""
        |class Foo
        | private; def bar
        | end
        |end
        |""".stripMargin)

    inside(cpg.call.nameExact(RubyOperators.privateModifier).l) { case modifierCall :: Nil =>
      modifierCall.methodFullName shouldBe RubyOperators.privateModifier
      modifierCall.code shouldBe "private"
    }

    cpg.method("bar").head.isPrivate.size shouldBe 1
  }

  "protected modifier should create its own distinct operator call" ignore {
    val cpg = code("""
        |class Foo
        | protected
        |
        | def bar
        | end
        |end
        |""".stripMargin)

    inside(cpg.call.nameExact(RubyOperators.protectedModifier).l) { case modifierCall :: Nil =>
      modifierCall.methodFullName shouldBe RubyOperators.protectedModifier
      modifierCall.code shouldBe "protected"
    }

    cpg.method("bar").head.isProtected.size shouldBe 1
  }

  "private_class_method with a method declaration should create an operator call" ignore {
    val cpg = code("""
        |class Foo
        | private_class_method def self.bar(x)
        |   x
        | end
        |end
        |""".stripMargin)

    inside(cpg.call.nameExact(RubyOperators.privateClassMethod).l) { case modifierCall :: Nil =>
      modifierCall.methodFullName shouldBe RubyOperators.privateClassMethod
      modifierCall.argument.size shouldBe 1
      modifierCall.argument.head.code shouldBe "bar"

      inside(modifierCall.argument.l) { case (sym: Literal) :: Nil =>
        sym.code shouldBe ":bar"
      }
    }
  }

  "public with a symbol argument should create an operator call with the symbol as argument" in {
    val cpg = code("""
        |class Foo
        |  def bar
        |  end
        |
        |  public :bar
        |end
        |""".stripMargin)

    inside(cpg.call.nameExact(RubyOperators.publicModifier).l) { case modifierCall :: Nil =>
      modifierCall.methodFullName shouldBe RubyOperators.publicModifier
      modifierCall.code shouldBe "public :bar"

      inside(modifierCall.argument.l) { case (sym: Literal) :: Nil =>
        sym.code shouldBe ":bar"
      }
    }

    cpg.method("bar").head.isPublic.size shouldBe 1
  }

  "private with a symbol argument should create an operator call with the symbol as argument" in {
    val cpg = code("""
        |class Foo
        |  def bar
        |  end
        |
        |  private :bar
        |end
        |""".stripMargin)

    inside(cpg.call.nameExact(RubyOperators.privateModifier).l) { case modifierCall :: Nil =>
      modifierCall.methodFullName shouldBe RubyOperators.privateModifier
      modifierCall.code shouldBe "private :bar"

      inside(modifierCall.argument.l) { case (sym: Literal) :: Nil =>
        sym.code shouldBe ":bar"
      }
    }
  }

  "protected with a symbol argument should create an operator call with the symbol as argument" in {
    val cpg = code("""
        |class Foo
        |  def bar
        |  end
        |
        |  protected :bar
        |end
        |""".stripMargin)

    inside(cpg.call.nameExact(RubyOperators.protectedModifier).l) { case modifierCall :: Nil =>
      modifierCall.methodFullName shouldBe RubyOperators.protectedModifier
      modifierCall.code shouldBe "protected :bar"

      inside(modifierCall.argument.l) { case (sym: Literal) :: Nil =>
        sym.code shouldBe ":bar"
      }
    }
  }

  "private_class_method with a symbol argument should create an operator call" in {
    val cpg = code("""
        |class Foo
        |  def self.bar
        |  end
        |
        |  private_class_method :bar
        |end
        |""".stripMargin)

    inside(cpg.call.nameExact(RubyOperators.privateClassMethod).l) { case modifierCall :: Nil =>
      modifierCall.methodFullName shouldBe RubyOperators.privateClassMethod
      modifierCall.code shouldBe "private_class_method :bar"

      inside(modifierCall.argument.l) { case (sym: Literal) :: Nil =>
        sym.code shouldBe ":bar"
      }
    }
  }

  "public_class_method with a symbol argument should create an operator call" in {
    val cpg = code("""
        |class Foo
        |  private_class_method def self.bar
        |  end
        |
        |  public_class_method :bar
        |end
        |""".stripMargin)

    inside(cpg.call.nameExact(RubyOperators.publicClassMethod).l) { case modifierCall :: Nil =>
      modifierCall.methodFullName shouldBe RubyOperators.publicClassMethod
      modifierCall.code shouldBe "public_class_method :bar"

      inside(modifierCall.argument.l) { case (sym: Literal) :: Nil =>
        sym.code shouldBe ":bar"
      }
    }
  }

  "private with multiple symbol arguments should pass all arguments to the operator call" in {
    val cpg = code("""
        |class Foo
        |  def bar; end
        |  def baz; end
        |  private :bar, :baz
        |end
        |""".stripMargin)

    inside(cpg.call.nameExact(RubyOperators.privateModifier).l) { case modifierCall :: Nil =>
      modifierCall.code shouldBe "private :bar, :baz"

      inside(modifierCall.argument.l) { case (sym1: Literal) :: (sym2: Literal) :: Nil =>
        sym1.code shouldBe ":bar"
        sym2.code shouldBe ":baz"
      }
    }
  }

  // TODO: MethodDeclaration as argument to AccessModifier needs handling in astForAccessModifier
  "private def bar should create an operator call with the method definition as argument" ignore {
    val cpg = code("""
        |class Foo
        |  private def bar
        |  end
        |end
        |""".stripMargin)

    inside(cpg.call.nameExact(RubyOperators.privateModifier).l) { case modifierCall :: Nil =>
      modifierCall.methodFullName shouldBe RubyOperators.privateModifier
      modifierCall.argument.size shouldBe 1
    }

    cpg.method("bar").head.isPrivate.size shouldBe 1
  }

  "private inside a method body should create an operator call without changing scope state" in {
    val cpg = code("""
        |class Foo
        |  def foo
        |    private
        |  end
        |end
        |""".stripMargin)

    cpg.method("foo").head.isPublic.size shouldBe 1

    inside(cpg.method("foo").body.ast.isCall.nameExact(RubyOperators.privateModifier).l) { case modifierCall :: Nil =>
      modifierCall.name shouldBe RubyOperators.privateModifier
    }
  }

  "targeted private :bar" should {
    val cpg = code("""
        |class Foo
        |  def bar; end
        |  private :bar
        |  def baz; end
        |end
        |""".stripMargin)

    "correctly represent the operator call" in {
      inside(cpg.call.nameExact(RubyOperators.privateModifier).l) { case modifierCall :: Nil =>
        modifierCall.code shouldBe "private :bar"

        inside(modifierCall.argument.l) { case (sym: Literal) :: Nil =>
          sym.code shouldBe ":bar"
        }
      }
    }

    "make bar private" in {
      cpg.method("bar").head.isPrivate.size shouldBe 1
    }

    "not change default visibility for subsequent methods" in {
      cpg.method("baz").head.isPublic.size shouldBe 1
    }
  }

  "retroactive protected :bar" should {
    val cpg = code("""
        |class Foo
        |  def bar; end
        |  protected :bar
        |end
        |""".stripMargin)

    "correctly represent the operator call" in {
      inside(cpg.call.nameExact(RubyOperators.protectedModifier).l) { case modifierCall :: Nil =>
        modifierCall.code shouldBe "protected :bar"

        inside(modifierCall.argument.l) { case (sym: Literal) :: Nil =>
          sym.code shouldBe ":bar"
        }
      }
    }

    "make bar protected" in {
      cpg.method("bar").head.isProtected.size shouldBe 1
    }
  }

  "private_class_method :bar" should {
    val cpg = code("""
        |class Foo
        |  def self.bar; end
        |  private_class_method :bar
        |end
        |""".stripMargin)

    "correctly represent the operator call" in {
      inside(cpg.call.nameExact(RubyOperators.privateClassMethod).l) { case modifierCall :: Nil =>
        modifierCall.code shouldBe "private_class_method :bar"

        inside(modifierCall.argument.l) { case (sym: Literal) :: Nil =>
          sym.code shouldBe ":bar"
        }
      }
    }

    "make singleton method bar private" in {
      cpg.method("bar").head.isPrivate.size shouldBe 1
    }
  }

  "public_class_method :bar after private_class_method" should {
    val cpg = code("""
        |class Foo
        |  private_class_method def self.bar; end
        |  public_class_method :bar
        |end
        |""".stripMargin)

    "correctly represent the operator call" in {
      inside(cpg.call.nameExact(RubyOperators.publicClassMethod).l) { case modifierCall :: Nil =>
        modifierCall.code shouldBe "public_class_method :bar"

        inside(modifierCall.argument.l) { case (sym: Literal) :: Nil =>
          sym.code shouldBe ":bar"
        }
      }
    }

    "restore singleton method to public" in {
      cpg.method("bar").head.isPublic.size shouldBe 1
    }
  }

  "private with multiple symbol arguments" should {
    val cpg = code("""
        |class Foo
        |  def bar; end
        |  def baz; end
        |  private :bar, :baz
        |end
        |""".stripMargin)

    "correctly represent the operator call" in {
      inside(cpg.call.nameExact(RubyOperators.privateModifier).l) { case modifierCall :: Nil =>
        modifierCall.code shouldBe "private :bar, :baz"

        inside(modifierCall.argument.l) { case (sym1: Literal) :: (sym2: Literal) :: Nil =>
          sym1.code shouldBe ":bar"
          sym2.code shouldBe ":baz"
        }
      }
    }

    "make both methods private" in {
      cpg.method("bar").head.isPrivate.size shouldBe 1
      cpg.method("baz").head.isPrivate.size shouldBe 1
    }
  }

  "private_class_method with multiple symbol arguments" should {
    val cpg = code("""
        |class Foo
        |  def self.bar; end
        |  def self.baz; end
        |  private_class_method :bar, :baz
        |end
        |""".stripMargin)

    "correctly represent the operator call" in {
      inside(cpg.call.nameExact(RubyOperators.privateClassMethod).l) { case modifierCall :: Nil =>
        modifierCall.code shouldBe "private_class_method :bar, :baz"

        inside(modifierCall.argument.l) { case (sym1: Literal) :: (sym2: Literal) :: Nil =>
          sym1.code shouldBe ":bar"
          sym2.code shouldBe ":baz"
        }
      }
    }

    "make both singleton methods private" in {
      cpg.method("bar").head.isPrivate.size shouldBe 1
      cpg.method("baz").head.isPrivate.size shouldBe 1
    }
  }

  "private_class_method with an array literal" should {
    val cpg = code("""
        |class Foo
        |  def self.bar; end
        |  def self.baz; end
        |  private_class_method %i[
        |    bar
        |    baz
        |  ]
        |end
        |""".stripMargin)

    "correctly represent the access modifier call" in {
      inside(cpg.call.nameExact(RubyOperators.privateClassMethod).l) { case modifierCall :: Nil =>
        modifierCall.methodFullName shouldBe RubyOperators.privateClassMethod
      }
    }

    "make both singleton methods private" in {
      cpg.method("bar").head.isPrivate.size shouldBe 1
      cpg.method("baz").head.isPrivate.size shouldBe 1
    }

    "not produce orphaned tmp identifiers" in {
      cpg.identifier.nameExact("<tmp-0>").method.size should not be 0
    }
  }

  "visibility can be re-opened: private then public" in {
    val cpg = code("""
        |class Foo
        |  private
        |  def bar; end
        |  public
        |  def baz; end
        |end
        |""".stripMargin)

    cpg.method("bar").head.isPrivate.size shouldBe 1
    cpg.method("baz").head.isPublic.size shouldBe 1
  }

  "access modifier operator calls" should {
    val cpg = code("""
        |class Foo
        |  def bar; end
        |  private :bar
        |  private_class_method :bar
        |end
        |""".stripMargin)

    "be inside the <body> method, not direct TYPE_DECL children" in {
      val bodyMethod = cpg.typeDecl.name("Foo").method.nameExact(Defines.TypeDeclBody).head

      inside(bodyMethod.body.ast.isCall.nameExact(RubyOperators.privateModifier).l) { case modifierCall :: Nil =>
        modifierCall.code shouldBe "private :bar"
      }

      inside(bodyMethod.body.ast.isCall.nameExact(RubyOperators.privateClassMethod).l) { case modifierCall :: Nil =>
        modifierCall.code shouldBe "private_class_method :bar"
      }
    }
  }

}
