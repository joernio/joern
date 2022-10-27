package io.joern.jssrc2cpg.passes

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.language._

class BuiltinTypesPassTest extends AbstractPassTest {

  "BuiltinTypesPass" should {
    val cpg = Cpg.emptyCpg
    new BuiltinTypesPass(cpg).createAndApply()

    "create a '<global>' NamespaceBlock" in {
      cpg.namespaceBlock.name.l shouldBe List(Defines.GLOBAL_NAMESPACE)
    }

    "create types and type decls correctly" in {
      Defines.JSTYPES.foreach { typeName: String =>
        val List(typeDeclNode) = cpg.typeDecl(typeName).l
        typeDeclNode.fullName shouldBe typeName
        typeDeclNode.isExternal shouldBe false
        typeDeclNode.filename shouldBe "builtintypes"

        cpg.namespaceBlock.astChildren.l should contain(typeDeclNode)

        val List(typeNode) = cpg.typ(typeName).l
        typeNode.fullName shouldBe typeName
      }
    }

  }

}
