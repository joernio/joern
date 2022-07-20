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
      Defines.values.foreach { case typeName: Defines.Tpe =>
        val typeNameLabel      = typeName.label
        val List(typeDeclNode) = cpg.typeDecl(typeNameLabel).l
        typeDeclNode.fullName shouldBe typeNameLabel
        typeDeclNode.isExternal shouldBe false
        typeDeclNode.filename shouldBe "builtintypes"

        cpg.namespaceBlock.astChildren.l should contain(typeDeclNode)

        val List(typeNode) = cpg.typ(typeNameLabel).l
        typeNode.fullName shouldBe typeNameLabel
      }
    }

  }

}
