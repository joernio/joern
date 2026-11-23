package io.joern.rust2cpg.astgen

import io.joern.rust2cpg.{Config, Rust2Cpg}
import io.joern.x2cpg.astgen.AstGenRunner.DefaultAstGenRunnerResult
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Path}
import scala.util.{Failure, Success, Try}

/** A failed AST generator must not look like an empty project.
  *
  * Hermetic on purpose: both cases drive the flag rather than the generator, so neither depends on
  * a toolchain being present or absent on the machine running the suite.
  */
class AstGenFailureTests extends AnyWordSpec with Matchers {

  "DefaultAstGenRunnerResult" should {

    "report no failure by default, so existing callers are unaffected" in {
      DefaultAstGenRunnerResult().generatorFailed shouldBe false
      DefaultAstGenRunnerResult(parsedFiles = List("a.json")).generatorFailed shouldBe false
    }

    "distinguish a failed generator from a project with no parsable files" in {
      val noFiles = DefaultAstGenRunnerResult()
      val failed  = DefaultAstGenRunnerResult(generatorFailed = true)
      noFiles.parsedFiles shouldBe empty
      failed.parsedFiles shouldBe empty
      // Equal in every other respect; only the flag separates them.
      noFiles.generatorFailed shouldBe false
      failed.generatorFailed shouldBe true
    }
  }

  "Rust2Cpg" should {

    "fail rather than produce an empty CPG when the AST generator could not run" in {
      val tmp: Path = Files.createTempDirectory("rust2cpg-astgen-failure")
      try {
        Files.writeString(tmp.resolve("Cargo.toml"), "[package]\nname = \"x\"\nversion = \"0.1.0\"\n")
        val src = Files.createDirectories(tmp.resolve("src"))
        Files.writeString(src.resolve("lib.rs"), "pub fn f(a: i32) -> i32 { a + 1 }\n")
        // A channel that cannot be present. rust_ast_gen invokes cargo with
        // RUSTUP_AUTO_INSTALL=0, so it cannot install one, and `cargo metadata` fails.
        Files.writeString(tmp.resolve("rust-toolchain.toml"), "[toolchain]\nchannel = \"1.0.0\"\n")

        val out    = tmp.resolve("out.cpg.bin")
        val config = Config().withInputPath(tmp.toString).withOutputPath(out.toString)
        val result = new Rust2Cpg().createCpg(config)

        result match {
          case Failure(_) => succeed
          case Success(_) =>
            fail(
              "createCpg reported success after the AST generator failed. Before this change it " +
                "wrote a CPG with zero methods and exited 0, so the caller could not tell a failed " +
                "parse from a codebase containing nothing."
            )
        }
      } finally {
        Try(Files.walk(tmp).sorted(java.util.Comparator.reverseOrder[Path]()).forEach(Files.delete))
      }
    }
  }
}
