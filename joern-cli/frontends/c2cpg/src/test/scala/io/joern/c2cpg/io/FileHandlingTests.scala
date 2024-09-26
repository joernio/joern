package io.joern.c2cpg.io

import better.files.File
import io.joern.c2cpg.parser.FileDefaults
import io.joern.c2cpg.testfixtures.CDefaultTestCpg
import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.semanticsloader.NoSemantics
import io.joern.x2cpg.testfixtures.Code2CpgFixture
import io.shiftleft.semanticcpg.language.*

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

object FileHandlingTests {
  private val brokenLinkedFile: String = "broken.c"
  private val cyclicLinkedFile: String = "loop.c"
}

class FileHandlingTests
    extends Code2CpgFixture(() =>
      new CDefaultTestCpg(FileDefaults.C_EXT) {
        override def codeFilePreProcessing(codeFile: Path): Unit = {
          if (codeFile.toString.endsWith(FileHandlingTests.brokenLinkedFile)) {
            File(codeFile).delete().symbolicLinkTo(File("does/not/exist.c"))
          }
          if (codeFile.toString.endsWith(FileHandlingTests.cyclicLinkedFile)) {
            val dir         = File(codeFile).delete().parent
            val folderA     = Paths.get(dir.toString(), "FolderA")
            val folderB     = Paths.get(dir.toString(), "FolderB")
            val symlinkAtoB = folderA.resolve("LinkToB")
            val symlinkBtoA = folderB.resolve("LinkToA")
            Files.createDirectory(folderA)
            Files.createDirectory(folderB)
            Files.createSymbolicLink(symlinkAtoB, folderB)
            Files.createSymbolicLink(symlinkBtoA, folderA)
          }
        }
      }
        .withOssDataflow(false)
        .withSemantics(DefaultSemantics())
        .withPostProcessingPasses(false)
    ) {

  "File handling 1" should {
    val cpg = code(
      """
      |int a() {}
      |""".stripMargin,
      "a.c"
    ).moreCode("", FileHandlingTests.brokenLinkedFile)

    "not crash on broken symlinks" in {
      val fileNames = cpg.file.name.l
      fileNames should contain("a.c").and(not contain FileHandlingTests.brokenLinkedFile)
    }

  }

  "File handling 2" should {
    val cpg = code(
      """
        |int a() {}
        |""".stripMargin,
      "a.c"
    ).moreCode("", FileHandlingTests.cyclicLinkedFile)

    "not crash on cyclic symlinks" in {
      val fileNames = cpg.file.name.l
      fileNames should contain("a.c")
    }

  }

}
