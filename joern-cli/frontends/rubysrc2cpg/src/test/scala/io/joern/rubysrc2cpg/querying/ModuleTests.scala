package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.passes.Defines
import io.joern.rubysrc2cpg.passes.Defines.Main
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.NodeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{File, NamespaceBlock}
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
}
