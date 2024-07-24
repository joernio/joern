package io.joern.c2cpg.io

import better.files.File
import io.joern.c2cpg.parser.FileDefaults
import io.joern.c2cpg.testfixtures.CDefaultTestCpg
import io.joern.x2cpg.testfixtures.Code2CpgFixture
import io.shiftleft.semanticcpg.language.*

import java.nio.file.Path

class FileHandlingTests
    extends Code2CpgFixture(() =>
      new CDefaultTestCpg(FileDefaults.C_EXT) {
        override def codeFilePreProcessing(codeFile: Path): Unit = {
          if (codeFile.toString.endsWith("broken.c")) {
            File(codeFile).delete().symbolicLinkTo(File("does/not/exist.c"))
          }
        }
      }
        .withOssDataflow(false)
        .withExtraFlows(List.empty)
        .withPostProcessingPasses(false)
    ) {

  "File handling" should {
    val cpg = code(
      """
      |int a() {}
      |""".stripMargin,
      "a.c"
    ).moreCode("", "broken.c")

    "not crash on broken symlinks" in {
      val fileNames = cpg.file.name.l
      fileNames should contain("a.c").and(not contain "broken.c")
    }

  }

}
