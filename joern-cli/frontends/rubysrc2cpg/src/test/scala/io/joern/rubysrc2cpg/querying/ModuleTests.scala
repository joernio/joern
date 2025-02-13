package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.passes.Defines.Main
import io.joern.rubysrc2cpg.passes.{Defines, GlobalTypes}
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{File, Literal, NamespaceBlock}
import io.shiftleft.codepropertygraph.generated.{ModifierTypes, NodeTypes}
import io.shiftleft.semanticcpg.language.*

class ModuleTests extends RubyCode2CpgFixture {

  "`module M ... end` is represented by a TYPE_DECL node" in {
    val cpg = code("""
                     |module M
                     |end
                     |""".stripMargin)

    val List(m) = cpg.typeDecl.name("M").l

    m.fullName shouldBe s"Test0.rb:$Main.M"
    m.lineNumber shouldBe Some(2)
    m.baseType.l shouldBe List()
    m.member.name.l shouldBe List(Defines.TypeDeclBody)
    m.method.name.l shouldBe List(Defines.TypeDeclBody)
  }

  "nested modules are represented by nested TYPE_DECL nodes" in {
    val cpg = code("""
        |module M1
        | module M2
        | end
        |end
        |""".stripMargin)

    val List(m) = cpg.typeDecl.name("M1").l

    m.fullName shouldBe s"Test0.rb:$Main.M1"
    m.lineNumber shouldBe Some(2)
    m.baseType.l shouldBe List()
    m.member.name.l shouldBe List(Defines.TypeDeclBody)
    m.method.name.l shouldBe List(Defines.TypeDeclBody)
  }

  "Module defined in Namespace" in {
    val cpg = code("""
        |module Api::V1::MobileController
        |end
        |""".stripMargin)

    inside(cpg.namespaceBlock.fullNameExact("Api.V1").typeDecl.l) {
      case mobileNamespace :: mobileClassNamespace :: Nil =>
        mobileNamespace.name shouldBe "MobileController"
        mobileNamespace.fullName shouldBe "Test0.rb:<main>.Api.V1.MobileController"

        mobileClassNamespace.name shouldBe "MobileController<class>"
        mobileClassNamespace.fullName shouldBe "Test0.rb:<main>.Api.V1.MobileController<class>"
      case xs => fail(s"Expected two namespace blocks, got ${xs.code.mkString(",")}")
    }

    inside(cpg.typeDecl.name("MobileController").l) {
      case mobileTypeDecl :: Nil =>
        mobileTypeDecl.name shouldBe "MobileController"
        mobileTypeDecl.fullName shouldBe "Test0.rb:<main>.Api.V1.MobileController"
        mobileTypeDecl.astParentFullName shouldBe "Api.V1"
        mobileTypeDecl.astParentType shouldBe NodeTypes.NAMESPACE_BLOCK

        mobileTypeDecl.astParent.isNamespaceBlock shouldBe true

        val namespaceDecl = mobileTypeDecl.astParent.asInstanceOf[NamespaceBlock]
        namespaceDecl.name shouldBe "Api.V1"
        namespaceDecl.filename shouldBe "Test0.rb"

        namespaceDecl.astParent.isFile shouldBe true
        val parentFileDecl = namespaceDecl.astParent.asInstanceOf[File]
        parentFileDecl.name shouldBe "Test0.rb"

      case xs => fail(s"Expected one class decl, got [${xs.code.mkString(",")}]")
    }
  }

  "Class Method Modifiers" should {
    val cpg = code("""
        |# Taken from Mastodon Repo
        |module LanguagesHelper
        |  ISO_639_1 = {}
        |  ISO_639_3 = {}
        |  SUPPORTED_LOCALES = {}
        |  REGIONAL_LOCALE_NAMES = {}
        |
        |  private_class_method def self.locale_name_for_sorting(locale)
        |    if (supported_locale = SUPPORTED_LOCALES[locale.to_sym])
        |      ASCIIFolding.new.fold(supported_locale[1]).downcase
        |    elsif (regional_locale = REGIONAL_LOCALE_NAMES[locale.to_sym])
        |      ASCIIFolding.new.fold(regional_locale).downcase
        |    else
        |      locale
        |    end
        |  end
        |
        |  def publicMethodAfterwards
        |  end
        |end
        |""".stripMargin)
    "Generate private modifier on method" in {
      inside(cpg.method.name("locale_name_for_sorting")._modifierViaAstOut.l) {
        case virtualModifier :: privateModifier :: Nil =>
          virtualModifier.modifierType shouldBe ModifierTypes.VIRTUAL
          privateModifier.modifierType shouldBe ModifierTypes.PRIVATE
        case xs => fail(s"Expected two modifiers, got [${xs.modifierType.mkString(",")}]")
      }
    }

    "Revert to original access modifier after previous method def" in {
      inside(cpg.method.name("publicMethodAfterwards")._modifierViaAstOut.l) {
        case virtualModifier :: publicModifier :: Nil =>
          virtualModifier.modifierType shouldBe ModifierTypes.VIRTUAL
          publicModifier.modifierType shouldBe ModifierTypes.PUBLIC
        case xs => fail(s"Expected got [${xs.modifierType.mkString(",")}]")
      }
    }
  }

  "Protected call with block" should {
    val cpg = code("""
        |module QA
        |  trait :protected do
        |    protected { true }
        |  end
        |end
        |""".stripMargin)

    "Have the correct proc arg in call" in {
      inside(cpg.call.name("protected").argument.l) {
        case _ :: proc :: Nil =>
          proc.code shouldBe "<lambda>1&Proc"
        case xs => fail(s"Expected one call for protected, got [${xs.code.mkString(",")}]")
      }
    }

    "Generate a lambda with true body" in {
      inside(cpg.method.isLambda.l) {
        case protectedLambda :: _ :: Nil =>
          val List(lambdaReturn) = protectedLambda.body.astChildren.isReturn.l
          lambdaReturn.code shouldBe "true"
        case xs => fail(s"Expected two lambdas, got [${xs.code.mkString(",")}]")
      }
    }
  }
  "Argument `(...)` in call should not be lifted" in {
    val cpg = code("""
        |module ArticlesHelper
        |  def foo
        |  end
        |
        |  def active_threads(...)
        |    Articles::ActiveThreadsQuery.call(...)
        |  end
        |end
        |
        |""".stripMargin)

    inside(cpg.typeDecl.name("ArticlesHelper").method.l) {
      case bodyMethod :: _ :: _ :: Nil =>
        inside(bodyMethod.block.astChildren.l) {
          case Nil => // bodyMethod should be empty
          case xs  => fail(s"Expected empty body, got [${xs.code.mkString(",")}]")
        }
      case xs => fail(s"Expected three methods got [${xs.name.mkString(",")}]")
    }
  }
}
