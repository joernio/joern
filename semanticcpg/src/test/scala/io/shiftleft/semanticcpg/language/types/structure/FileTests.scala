package io.shiftleft.semanticcpg.language.types.structure

import io.shiftleft.codepropertygraph.generated.nodes.{File, Namespace, TypeDecl}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.testing.MockCpg
import org.scalatest.LoneElement
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class FileTests extends AnyWordSpec with Matchers with LoneElement {
  val fileName = "io/shiftleft/testcode/file/FileTest.java"
  val cpg = MockCpg()
    .withFile(fileName)
    .withNamespace("io.shiftleft.testcode.file", inFile = Some(fileName))
    .withTypeDecl("FileTest", inFile = Some(fileName), inNamespace = Some("io.shiftleft.testcode.file"))
    .withMethod("method", inTypeDecl = Some("FileTest"))
    .cpg

  "generic cpg" should {
    s"find file $fileName" in {
      val queryResult: List[File] = cpg.file.l

      queryResult.map(_.name) should contain(fileName)
    }

    "be able to expand to class FileTest" in {
      val queryResult: List[TypeDecl] =
        cpg.file.name(fileName).typeDecl.l

      queryResult.loneElement.name shouldBe "FileTest"
    }

    "be able to expand to namespace" in {
      val queryResult: List[Namespace] =
        cpg.file.name(fileName).namespace.l
      queryResult.loneElement.name shouldBe "io.shiftleft.testcode.file"
    }

    "be able to get file in which a formal method return is defined" in {
      val queryResult: List[File] =
        cpg.method.name("method").methodReturn.file.l

      queryResult.loneElement.name shouldBe fileName
    }
  }

}
