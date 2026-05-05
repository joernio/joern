package io.joern.rust2cpg.testfixtures

import io.joern.x2cpg.testfixtures.DefaultTestCpg

import java.nio.file.{Files, Path}

class RustDefaultTestCpg extends DefaultTestCpg with Rust2CpgFrontend {

  override val fileSuffix: String = ".rs"

  override protected def codeDirPreProcessing(rootFile: Path, codeFiles: List[Path]): Unit = {
    val cargoTomlPath = rootFile.resolve("Cargo.toml")
    val cargoTomlContents =
      """
        |[package]
        |name = "rust2cpgtest"
        |version = "0.1.0"
        |edition = "2021"
        |""".stripMargin

    Files.writeString(cargoTomlPath, cargoTomlContents)
  }

}
