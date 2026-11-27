package io.joern.joerncli

import flatgraph.formats.dot.DotExporter
import flatgraph.formats.neo4jcsv.Neo4jCsvExporter
import io.joern.joerncli.JoernExport.Representation
import io.shiftleft.codepropertygraph.generated.Cpg
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import collection.mutable

import java.nio.file.{Path, Paths}

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
    val fileExtension = Neo4jCsvExporter.defaultFileExtension

    def fileNames(isWindows: Boolean, methods: (String, String)*): Seq[Path] = {
      val usedPaths = mutable.Set.empty[Path]
      methods.map { case (methodName, methodFilename) =>
        JoernExport.sanitizedFileName(methodName, methodFilename, fileExtension, usedPaths, isWindows)
      }
    }

    "use the file name of the method as directory outside of Windows" in {
      fileNames(isWindows = false, "methodName1" -> "file1.c", "methodName2" -> "dir/file2.c") shouldBe
        Seq(
          Paths.get("file1.c", s"methodName1.$fileExtension"),
          Paths.get("dir", "file2.c", s"methodName2.$fileExtension")
        )
    }

    "use only the method name on Windows to keep paths short" in {
      fileNames(isWindows = true, "methodName1" -> "file1.c", "methodName2" -> "dir/file2.c") shouldBe
        Seq(Paths.get(s"methodName1.$fileExtension"), Paths.get(s"methodName2.$fileExtension"))
    }

    "remove leading `/` to ensure we're not writing outside of the output directory" in {
      fileNames(
        isWindows = false,
        "methodName1" -> "",
        "methodName2" -> "/abs/file.c",
        "methodName3" -> "../../x.c"
      ) shouldBe
        Seq(
          Paths.get("_root_", s"methodName1.$fileExtension"),
          Paths.get("_root_", "abs", "file.c", s"methodName2.$fileExtension"),
          Paths.get("x.c", s"methodName3.$fileExtension")
        )
    }

    "normalize paths of CPGs that were created on Windows" in {
      fileNames(
        isWindows = false,
        "m1" -> "C:\\src\\file.c",
        "m2" -> "src\\sub\\file.c",
        "m3" -> "\\abs\\file.c"
      ) shouldBe
        Seq(
          Paths.get("C_", "src", "file.c", s"m1.$fileExtension"),
          Paths.get("src", "sub", "file.c", s"m2.$fileExtension"),
          Paths.get("_root_", "abs", "file.c", s"m3.$fileExtension")
        )
    }

    for (isWindows <- Seq(true, false)) {
      s"append the file extension exactly once, even for many methods with the same name (isWindows=$isWindows)" in {
        val result = fileNames(isWindows, Seq.fill(60)("<init>" -> "file1.java")*)

        result.distinct.size shouldBe 60
        result.foreach { path =>
          val fileName = path.getFileName.toString
          fileName should endWith(s".$fileExtension")
          fileName.stripSuffix(s".$fileExtension") should not include "."
        }
        result.head.getFileName.toString shouldBe s"_init_.$fileExtension"
        result(1).getFileName.toString shouldBe s"_init__.$fileExtension"
      }

      s"not clash for names that only differ in sanitized characters (isWindows=$isWindows)" in {
        fileNames(isWindows, "<init>" -> "f.java", "_init_" -> "f.java", "<init>" -> "f.java").distinct.size shouldBe 3
      }

      s"not clash for names that only differ in case (isWindows=$isWindows)" in {
        fileNames(isWindows, "foo" -> "f.java", "Foo" -> "f.java", "FOO" -> "f.java")
          .map(_.toString.toLowerCase)
          .distinct
          .size shouldBe 3
      }
    }

    "not clash for files that only differ in case outside of Windows" in {
      val result = fileNames(isWindows = false, "foo" -> "File.java", "foo" -> "file.java")
      result.map(_.toString.toLowerCase).distinct.size shouldBe 2
    }
  }

}
