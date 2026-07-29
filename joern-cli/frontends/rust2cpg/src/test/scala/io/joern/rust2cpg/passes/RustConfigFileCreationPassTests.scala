package io.joern.rust2cpg.passes

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.utils.FileUtil.*

import java.nio.file.Paths

class RustConfigFileCreationPassTests extends Rust2CpgSuite {

  "Cargo.toml" should {
    val cargoToml =
      """[package]
        |name = "rust2cpgtest"
        |version = "0.1.0"
        |edition = "2021"
        |""".stripMargin
    val cpg = code("fn main() {}").moreCode(cargoToml, "Cargo.toml")

    "be included" in {
      cpg.configFile.nameExact("Cargo.toml").content.l shouldBe List(cargoToml)
    }
  }

  "Cargo.lock" should {
    val cargoLock =
      """version = 3
        |
        |[[package]]
        |name = "rust2cpgtest"
        |version = "0.1.0"
        |""".stripMargin
    val cpg = code("fn main() {}").moreCode(cargoLock, "Cargo.lock")

    "be included" in {
      cpg.configFile.nameExact("Cargo.lock").content.l shouldBe List(cargoLock)
    }
  }

  "rust-toolchain.toml" should {
    val toolchainToml =
      """[toolchain]
        |channel = "stable"
        |""".stripMargin
    val cpg = code("fn main() {}").moreCode(toolchainToml, "rust-toolchain.toml")

    "be included" in {
      cpg.configFile.nameExact("rust-toolchain.toml").content.l shouldBe List(toolchainToml)
    }
  }

  "Cargo.toml in a subdirectory" should {
    val cargoTomlPath = (Paths.get("nested") / "Cargo.toml").toString
    val cargoToml =
      """[package]
        |name = "nested"
        |version = "0.1.0"
        |edition = "2021"
        |""".stripMargin
    val cpg = code("fn main() {}").moreCode(cargoToml, cargoTomlPath)

    "be included" in {
      cpg.configFile.nameExact(cargoTomlPath).content.l shouldBe List(cargoToml)
    }
  }

}
