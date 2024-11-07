package io.joern.jssrc2cpg.preprocessing

import better.files.File
import io.joern.jssrc2cpg.Config
import io.joern.jssrc2cpg.testfixtures.AstJsSrc2CpgSuite
import io.joern.jssrc2cpg.utils.AstGenRunner

class AstGenTests extends AstJsSrc2CpgSuite {

  "Ast gen" should {
    val tmpDir = File.newTemporaryDirectory("src")
    ((tmpDir / "folder1").createDirectoryIfNotExists() / "1.js").write("console.log('folder1');")
    ((tmpDir / "folder2").createDirectoryIfNotExists() / "1.js").write("console.log('folder2');")
    ((tmpDir / "folder3").createDirectoryIfNotExists() / "1.js").write("console.log('folder3');")
    "ignore files mentioned in exclusion regex" in {
      val newInputDir = new AstGenRunner(Config().withInputPath(tmpDir.toString).withIgnoredFilesRegex(".*folder3.*"))
        .filterAndCopyFiles()

      val fileSet = newInputDir.listRecursively.filter(_.isRegularFile).map(_.pathAsString).toSet
      fileSet.size shouldBe 2
      fileSet.count(_.matches(".*folder1.*")) shouldBe 1
      fileSet.count(_.matches(".*folder2.*")) shouldBe 1
      fileSet.count(_.matches(".*folder3.*")) shouldBe 0
    }

    "don't ignore files, if exclusion regex not passed" in {
      val newInputDir = new AstGenRunner(Config().withInputPath(tmpDir.toString)).filterAndCopyFiles()

      val fileSet = newInputDir.listRecursively.filter(_.isRegularFile).map(_.pathAsString).toSet
      fileSet.size shouldBe 3
      fileSet.count(_.matches(".*folder1.*")) shouldBe 1
      fileSet.count(_.matches(".*folder2.*")) shouldBe 1
      fileSet.count(_.matches(".*folder3.*")) shouldBe 1

    }
  }

}
