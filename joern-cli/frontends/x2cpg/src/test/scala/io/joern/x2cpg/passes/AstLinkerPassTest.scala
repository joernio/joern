package io.joern.x2cpg.passes

import io.joern.x2cpg.layers.Base
import io.joern.x2cpg.passes.base.AstLinkerPass
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.{NewCall, NewMember, NewTypeDecl}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import io.shiftleft.semanticcpg.testing.MockCpg
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class AstLinkerPassTest extends AnyWordSpec with Matchers {

  "Check member node is child of TypeDecl node" in {
    val cpg = MockCpg().withCustom { (graph, _) =>
      implicit val withSchemaValidation: ValidationMode = ValidationMode.Disabled
      val typeDecl                                      = NewTypeDecl().name("Sample").fullName("namespace.Sample")
      Ast.storeInDiffGraph(Ast(typeDecl), graph)
      val memberNode = NewMember()
        .name("test")
        .typeFullName("String")
        .astParentType(NodeTypes.TYPE_DECL)
        .astParentFullName("namespace.Sample")
      Ast.storeInDiffGraph(Ast(memberNode), graph)
    }.cpg

    val context = new LayerCreatorContext(cpg)
    new Base().run(context)
    val List(typeDecl) = cpg.typeDecl("Sample").l
    val List(member)   = typeDecl.member.l
    member.typeFullName shouldBe "String"
    member.name shouldBe "test"
  }
}
