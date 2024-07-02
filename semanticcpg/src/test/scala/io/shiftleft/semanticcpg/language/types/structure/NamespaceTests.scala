package io.shiftleft.semanticcpg.language.types.structure

import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.testing.MockCpg
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class NamespaceTests extends AnyWordSpec with Matchers {

  val cpg = MockCpg()
    .withFile("filename.c")
    .withNamespace("io.shiftleft.testcode.namespace", inFile = Some("filename.c"))
    .withTypeDecl("NamespaceTest", inNamespace = Some("io.shiftleft.testcode.namespace"))
    .cpg

  "generic cpg" should {
    "find package io.shiftleft.testcode.namespace" in {
      val queryResult: List[Namespace] = cpg.namespace.toList

      queryResult.map(_.name) should contain("io.shiftleft.testcode.namespace")
    }

    "find a namespace block for io.shiftleft.testcode.namespace" in {
      val queryResult: List[NamespaceBlock] = cpg.namespaceBlock.toList
      queryResult.map(_.name) should contain("io.shiftleft.testcode.namespace")
    }

    "be able to traverse from namespace block to type decl" in {
      val queryResult: List[TypeDecl] = cpg.namespaceBlock
        .nameExact("io.shiftleft.testcode.namespace")
        .typeDecl
        .toList
      queryResult.map(_.name) should contain("NamespaceTest")
    }

    "be able to expand to class NamespaceTest from via its namespace." in {
      val queryResult: List[TypeDecl] =
        cpg.namespace.name("io.shiftleft.testcode.namespace").typeDecl.toList

      queryResult.size shouldBe 1
      queryResult.head.name shouldBe "NamespaceTest"
    }

    "be able to expand to file which defines namespace." in {
      val queryResult: List[File] =
        cpg.namespace.name("io.shiftleft.testcode.namespace").file.toList

      queryResult.size shouldBe 1
      queryResult.head.name shouldBe "filename.c"
    }
  }

}
