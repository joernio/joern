package io.joern.joerncli

import better.files.File
import io.joern.joerncli.JoernExport.Representation
import io.shiftleft.codepropertygraph.generated.Cpg
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class JoernExportTests extends AnyWordSpec with Matchers with AbstractJoernCliTest {

  "export to graphson, for example code 'testcode/free'" should withTestCpg(
    File(getClass.getClassLoader.getResource("testcode/free"))
  ) { case (cpg: Cpg, _) =>
    "split output by method" in {
      val tempDir = os.temp.dir(prefix = "joern-export-test")
      JoernExport.exportCpg(cpg, Representation.Cpg, JoernExport.Format.Graphson, tempDir.toNIO)
      val exportedFiles = os.walk(tempDir).filter(_.toIO.isFile)
      exportedFiles.size shouldBe 7
    }
  }

}
