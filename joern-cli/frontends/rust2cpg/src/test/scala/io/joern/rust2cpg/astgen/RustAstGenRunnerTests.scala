package io.joern.rust2cpg.astgen

import io.joern.rust2cpg.Config
import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.semanticcpg.utils.FileUtil.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Path}

class RustAstGenRunnerTests extends AnyWordSpec with Matchers {

  private def writeFile(file: Path, content: String): Unit = {
    file.createWithParentsIfNotExists(createParents = true)
    Files.writeString(file, content)
  }

  "RustAstGenRunner" should {

    "parse a single-crate project" in {
      FileUtil.usingTemporaryDirectory("rust2cpgTestInput") { inputDir =>
        writeFile(
          inputDir / "Cargo.toml",
          """[package]
            |name = "simple"
            |version = "0.1.0"
            |edition = "2021"
            |""".stripMargin
        )
        writeFile(inputDir / "src" / "main.rs", "fn main() { println!(\"hello\"); }")
        writeFile(inputDir / "src" / "lib.rs", "pub mod utils; pub fn add(a: i32, b: i32) -> i32 { a + b }")
        writeFile(inputDir / "src" / "utils" / "mod.rs", "pub fn greet() {}")

        FileUtil.usingTemporaryDirectory("rust2cpgTestOut") { outputDir =>
          val config = Config().withInputPath(inputDir.toString).withOutputPath(outputDir.toString)
          val result = new RustAstGenRunner(config).execute(outputDir)
          result.parsedFiles.sorted should contain theSameElementsAs Seq(
            outputDir / "src" / "lib.rs.json",
            outputDir / "src" / "main.rs.json",
            outputDir / "src" / "utils" / "mod.rs.json"
          ).map(_.toString)
        }
      }
    }

    "parse a workspace project with crates/" in {
      FileUtil.usingTemporaryDirectory("rust2cpgTestInput") { inputDir =>
        writeFile(
          inputDir / "Cargo.toml",
          """[workspace]
            |members = ["crates/core", "crates/cli"]
            |""".stripMargin
        )
        writeFile(
          inputDir / "crates" / "core" / "Cargo.toml",
          """[package]
            |name = "core"
            |version = "0.1.0"
            |edition = "2021"
            |""".stripMargin
        )
        writeFile(inputDir / "crates" / "core" / "src" / "lib.rs", "pub fn core_fn() -> u32 { 42 }")
        writeFile(
          inputDir / "crates" / "cli" / "Cargo.toml",
          """[package]
            |name = "cli"
            |version = "0.1.0"
            |edition = "2021"
            |
            |[dependencies]
            |core = { path = "../core" }
            |""".stripMargin
        )
        writeFile(inputDir / "crates" / "cli" / "src" / "main.rs", "fn main() { println!(\"{}\", core::core_fn()); }")

        FileUtil.usingTemporaryDirectory("rust2cpgTestOut") { outputDir =>
          val config = Config().withInputPath(inputDir.toString).withOutputPath(outputDir.toString)
          val result = new RustAstGenRunner(config).execute(outputDir)
          result.parsedFiles.sorted should contain theSameElementsAs Seq(
            outputDir / "crates" / "cli" / "src" / "main.rs.json",
            outputDir / "crates" / "core" / "src" / "lib.rs.json"
          ).map(_.toString)
        }
      }

    }

    "parse a library project with tests, examples, and benches" in {
      FileUtil.usingTemporaryDirectory("rust2cpgTestInput") { inputDir =>
        writeFile(
          inputDir / "Cargo.toml",
          """[package]
            |name = "mylib"
            |version = "0.1.0"
            |edition = "2021"
            |""".stripMargin
        )
        writeFile(inputDir / "src" / "lib.rs", "pub fn lib_fn() -> bool { true }")
        writeFile(inputDir / "tests" / "integration.rs", "#[test] fn it_works() { assert!(mylib::lib_fn()); }")
        writeFile(inputDir / "examples" / "demo.rs", "fn main() { println!(\"demo\"); }")
        writeFile(inputDir / "benches" / "bench.rs", "fn main() {}")

        FileUtil.usingTemporaryDirectory("rust2cpgTestOut") { outputDir =>
          val config = Config().withInputPath(inputDir.toString).withOutputPath(outputDir.toString)
          val result = new RustAstGenRunner(config).execute(outputDir)
          result.parsedFiles.sorted should contain theSameElementsAs Seq(
            outputDir / "benches" / "bench.rs.json",
            outputDir / "examples" / "demo.rs.json",
            outputDir / "src" / "lib.rs.json",
            outputDir / "tests" / "integration.rs.json"
          ).map(_.toString)
        }
      }

    }

    "respect ignored files configuration" in {
      FileUtil.usingTemporaryDirectory("rust2cpgTestInput") { inputDir =>
        writeFile(
          inputDir / "Cargo.toml",
          """[package]
            |name = "simple"
            |version = "0.1.0"
            |edition = "2021"
            |""".stripMargin
        )
        writeFile(inputDir / "src" / "main.rs", "fn main() {}")
        writeFile(inputDir / "src" / "lib.rs", "pub mod utils; pub fn add(a: i32, b: i32) -> i32 { a + b }")
        writeFile(inputDir / "src" / "utils" / "mod.rs", "pub fn greet() {}")

        FileUtil.usingTemporaryDirectory("rust2cpgTestOut") { outputDir =>
          val ignoredFile = (inputDir / "src" / "lib.rs").toString
          val config      = Config()
            .withInputPath(inputDir.toString)
            .withOutputPath(outputDir.toString)
            .withIgnoredFiles(Seq(ignoredFile))
          val result = new RustAstGenRunner(config).execute(outputDir)
          result.parsedFiles.sorted should contain theSameElementsAs Seq(
            outputDir / "src" / "main.rs.json",
            outputDir / "src" / "utils" / "mod.rs.json"
          ).map(_.toString)
        }
      }
    }

    "respect ignored files regex configuration" in {
      FileUtil.usingTemporaryDirectory("rust2cpgTestInput") { inputDir =>
        writeFile(
          inputDir / "Cargo.toml",
          """[package]
            |name = "mylib"
            |version = "0.1.0"
            |edition = "2021"
            |""".stripMargin
        )
        writeFile(inputDir / "src" / "lib.rs", "pub fn lib_fn() -> bool { true }")
        writeFile(inputDir / "tests" / "integration.rs", "#[test] fn it_works() { assert!(mylib::lib_fn()); }")
        writeFile(inputDir / "examples" / "demo.rs", "fn main() { println!(\"demo\"); }")
        writeFile(inputDir / "benches" / "bench.rs", "fn main() {}")

        FileUtil.usingTemporaryDirectory("rust2cpgTestOut") { outputDir =>
          val config = Config()
            .withInputPath(inputDir.toString)
            .withOutputPath(outputDir.toString)
            .withIgnoredFilesRegex(".*bench.*")
          val result = new RustAstGenRunner(config).execute(outputDir)
          result.parsedFiles.sorted should contain theSameElementsAs Seq(
            outputDir / "examples" / "demo.rs.json",
            outputDir / "src" / "lib.rs.json",
            outputDir / "tests" / "integration.rs.json"
          ).map(_.toString)
        }
      }
    }

    "match ignored files regex against the whole path relative to the input directory" in {

      /** Relative paths use the separator of the operating system, so regexes must accept both `/` and `\`. */
      val sep = "[/\\\\]"
      FileUtil.usingTemporaryDirectory("rust2cpgTestInput") { inputDir =>
        writeFile(
          inputDir / "Cargo.toml",
          """[package]
            |name = "mylib"
            |version = "0.1.0"
            |edition = "2021"
            |""".stripMargin
        )
        writeFile(inputDir / "src" / "lib.rs", "pub fn lib_fn() -> bool { true }")
        writeFile(inputDir / "tests" / "integration.rs", "#[test] fn it_works() { assert!(mylib::lib_fn()); }")
        writeFile(inputDir / "benches" / "bench.rs", "fn main() {}")

        def parsed(regex: String): Seq[String] = FileUtil.usingTemporaryDirectory("rust2cpgTestOut") { outputDir =>
          val config = Config().withInputPath(inputDir.toString).withOutputPath(outputDir.toString)
          new RustAstGenRunner(config.withIgnoredFilesRegex(regex))
            .execute(outputDir)
            .parsedFiles
            .map(file =>
              outputDir.relativize(java.nio.file.Paths.get(file)).toString.replace(java.io.File.separator, "/")
            )
            .sorted
        }

        parsed(s"benches$sep.*") shouldBe Seq("src/lib.rs.json", "tests/integration.rs.json")
        parsed("benches") shouldBe Seq("src/lib.rs.json", "tests/integration.rs.json")
        parsed(s"(?!src$sep.*$$).*\\.rs") shouldBe Seq("src/lib.rs.json")
        parsed("ench") shouldBe Seq("benches/bench.rs.json", "src/lib.rs.json", "tests/integration.rs.json")
        // a regex that only matches the absolute path does not exclude anything
        val absolute = inputDir.toString.replaceAll("([^A-Za-z0-9_/])", "\\\\$1")
        parsed(s"$absolute${sep}benches$sep.*") shouldBe
          Seq("benches/bench.rs.json", "src/lib.rs.json", "tests/integration.rs.json")
      }
    }

    "skip out-of-crate file" in {
      FileUtil.usingTemporaryDirectory("rust2cpgTestInput") { inputDir =>
        writeFile(
          inputDir / "Cargo.toml",
          """[package]
            |name = "mylib"
            |version = "0.1.0"
            |edition = "2021"
            |""".stripMargin
        )
        writeFile(inputDir / "src" / "lib.rs", "pub fn lib_fn() {}")
        writeFile(inputDir / "src" / "orphan.rs", "pub fn orphan_fn() {}")

        FileUtil.usingTemporaryDirectory("rust2cpgTestOut") { outputDir =>
          val config = Config().withInputPath(inputDir.toString).withOutputPath(outputDir.toString)
          val result = new RustAstGenRunner(config).execute(outputDir)
          result.parsedFiles shouldBe List((outputDir / "src" / "lib.rs.json").toString)
          result.skippedFiles shouldBe List((inputDir / "src" / "orphan.rs").toRealPath().toString)
        }
      }
    }

    "collect skipped files" in {
      FileUtil.usingTemporaryDirectory("rust2cpgTestInput") { inputDir =>
        val config    = Config().withInputPath(inputDir.toString)
        val astGenOut =
          List(
            "garbage",
            s"Skipped: ${inputDir / "src" / "broken.rs"}",
            "  garbage ",
            s"Skipped: ${inputDir / "src" / "utils" / "mod.rs"}",
            "garbage "
          )
        new RustAstGenRunner(config).skippedFiles(inputDir, astGenOut) shouldBe List(
          (inputDir / "src" / "broken.rs").toString,
          (inputDir / "src" / "utils" / "mod.rs").toString
        )
      }
    }

    "parse a project with a non-standard layout" in {
      FileUtil.usingTemporaryDirectory("rust2cpgTestInput") { inputDir =>
        writeFile(
          inputDir / "Cargo.toml",
          """[package]
            |name = "custom_layout"
            |version = "0.1.0"
            |edition = "2021"
            |
            |[[bin]]
            |name = "app"
            |path = "bin/app.rs"
            |""".stripMargin
        )
        writeFile(inputDir / "bin" / "app.rs", "fn main() { println!(\"app\"); }")
        writeFile(inputDir / "src" / "lib.rs", "pub fn lib_fn() {}")

        FileUtil.usingTemporaryDirectory("rust2cpgTestOut") { outputDir =>
          val config = Config().withInputPath(inputDir.toString).withOutputPath(outputDir.toString)
          val result = new RustAstGenRunner(config).execute(outputDir)
          result.parsedFiles.sorted should contain theSameElementsAs Seq(
            outputDir / "bin" / "app.rs.json",
            outputDir / "src" / "lib.rs.json"
          ).map(_.toString)
        }
      }
    }
  }
}
