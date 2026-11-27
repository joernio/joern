package io.joern.joerncli

import flatgraph.formats.dot.DotExporter
import flatgraph.formats.neo4jcsv.Neo4jCsvExporter
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

  "file name sanitization with windows semantics" should {
    "append the file extension exactly once, even for many methods with the same name" in {
      val deduplicationHelper = mutable.Set.empty[String]
      val fileExtension       = Neo4jCsvExporter.defaultFileExtension

      val fileNames = (1 to 60).map { _ =>
        JoernExport.sanitizedFileName(
          methodName = "<init>",
          methodFilename = "file1.java",
          fileExtension,
          deduplicationHelper,
          isWindows = true
        )
      }

      fileNames.distinct.size shouldBe 60
      fileNames.foreach { fileName =>
        fileName should endWith(s".$fileExtension")
        fileName.stripSuffix(s".$fileExtension") should not include "."
      }
      fileNames.head shouldBe s"_init_.$fileExtension"
      fileNames(1) shouldBe s"_init__.$fileExtension"
    }

    "not clash for method names that only differ in characters that are sanitized" in {
      val deduplicationHelper = mutable.Set.empty[String]
      val fileExtension       = Neo4jCsvExporter.defaultFileExtension

      val fileNames = Seq("<init>", "_init_", "<init>").map { methodName =>
        JoernExport.sanitizedFileName(methodName, "file1.java", fileExtension, deduplicationHelper, isWindows = true)
      }

      fileNames.distinct.size shouldBe 3
    }
  }

}
