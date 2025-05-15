package io.joern.joerncli

import flatgraph.formats.dot.DotExporter
import io.joern.joerncli.JoernExport.Representation
import io.shiftleft.codepropertygraph.generated.Cpg
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import collection.mutable

import java.nio.file.Paths

class JoernExportTests extends AnyWordSpec with Matchers with AbstractJoernCliTest {

  "export to graphson, for example code 'testcode/free'" should withTestCpg(
    Paths.get(getClass.getClassLoader.getResource("testcode/free").toURI)
  ) { case (cpg: Cpg, _) =>
    "split output by method" in {
      val tempDir = os.temp.dir(prefix = "joern-export-test")
      JoernExport.exportCpg(cpg, Representation.Cpg, JoernExport.Format.Graphson, tempDir.toNIO)
      val exportedFiles = os.walk(tempDir).filter(_.toIO.isFile)
      exportedFiles.size shouldBe 7
    }
  }

  "file name sanitization" should {
    if (!scala.util.Properties.isWin) {
      "remove leading `/` to ensure we're not writing outside of the output directory" in {
        val windowsFilenameDeduplicationHelper = mutable.Set.empty[String]
        val fileExtension                      = DotExporter.defaultFileExtension

        JoernExport.sanitizedFileName(
          methodName = "methodName1",
          methodFilename = "file1.c",
          fileExtension,
          windowsFilenameDeduplicationHelper
        ) shouldBe "file1.c/methodName1.dot"

        JoernExport.sanitizedFileName(
          methodName = "methodName2",
          methodFilename = "",
          fileExtension,
          windowsFilenameDeduplicationHelper
        ) shouldBe "_root_/methodName2.dot"

      }
    }
  }

}
