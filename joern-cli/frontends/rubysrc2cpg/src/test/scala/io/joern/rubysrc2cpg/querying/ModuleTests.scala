package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.passes.Defines
import io.joern.rubysrc2cpg.passes.Defines.Main
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.NodeTypes
import io.shiftleft.codepropertygraph.generated.nodes.NamespaceBlock
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

    inside(cpg.typeDecl.name("MobileController").l) {
      case mobileTypeDecl :: Nil =>
        mobileTypeDecl.name shouldBe "MobileController"
        mobileTypeDecl.fullName shouldBe "Api.V1.MobileController"
        mobileTypeDecl.astParentFullName shouldBe "Api.V1"
        mobileTypeDecl.astParentType shouldBe NodeTypes.NAMESPACE_BLOCK

        mobileTypeDecl.astParent.isNamespaceBlock shouldBe true

        val namespaceDecl = mobileTypeDecl.astParent.asInstanceOf[NamespaceBlock]
        namespaceDecl.name shouldBe "Api.V1"
        namespaceDecl.filename shouldBe "Test0.rb"

      case xs => fail(s"Expected one class decl, got [${xs.code.mkString(",")}]")
    }
  }
}
