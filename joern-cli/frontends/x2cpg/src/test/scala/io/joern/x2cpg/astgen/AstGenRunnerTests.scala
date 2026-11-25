package io.joern.x2cpg.astgen

import io.joern.x2cpg.X2CpgConfig
import io.joern.x2cpg.astgen.AstGenRunner.{AstGenFailedException, AstGenProgramMetaData}
import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.semanticcpg.utils.FileUtil.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Path}
import scala.util.{Failure, Success, Try}

object AstGenRunnerTests {

  final case class StubConfig(override val genericConfig: X2CpgConfig.GenericConfig = X2CpgConfig.GenericConfig())
      extends X2CpgConfig[StubConfig] {
    override def withGenericConfig(value: X2CpgConfig.GenericConfig): StubConfig = copy(genericConfig = value)
  }

  private object StubMetaData extends AstGenProgramMetaData(name = "stubastgen", configPrefix = "stub")

  /** An AST generator that does not run any process. It writes the given JSON files to the output directory and then
    * returns the given outcome.
    */
  final class StubAstGenRunner(config: StubConfig, jsonFiles: Seq[String], outcome: Try[Seq[String]])
      extends AstGenRunner(StubMetaData, config) {

    override protected def skippedFiles(in: Path, astGenOut: List[String]): List[String] =
      astGenOut.collect { case line if line.startsWith("Skipped: ") => line.stripPrefix("Skipped: ") }

    override protected def runAstGenNative(
      in: String,
      out: Path,
      exclude: String,
      include: String
    ): Try[Seq[String]] = {
      jsonFiles.foreach(file => Files.writeString(out / file, "{}"))
      outcome
    }
  }
}

class AstGenRunnerTests extends AnyWordSpec with Matchers {
  import AstGenRunnerTests.*

  private def withRunner[T](jsonFiles: Seq[String], outcome: Try[Seq[String]])(f: (StubAstGenRunner, Path) => T): T = {
    FileUtil.usingTemporaryDirectory("astgenRunnerIn") { inputDir =>
      FileUtil.usingTemporaryDirectory("astgenRunnerOut") { outputDir =>
        val config = StubConfig().withInputPath(inputDir.toString)
        f(new StubAstGenRunner(config, jsonFiles, outcome), outputDir)
      }
    }
  }

  "AstGenRunner.execute" should {

    "report the parsed and skipped files of a successful run" in {
      withRunner(Seq("a.json"), Success(Seq("Skipped: b.js"))) { (runner, out) =>
        val result = runner.execute(out)
        result.parsedFiles shouldBe List((out / "a.json").toString)
        result.skippedFiles shouldBe List("b.js")
      }
    }

    "accept a successful run without any files, e.g., an input without parsable sources" in {
      withRunner(Seq.empty, Success(Seq.empty)) { (runner, out) =>
        val result = runner.execute(out)
        result.parsedFiles shouldBe empty
        result.skippedFiles shouldBe empty
      }
    }

    "accept a run that only skipped files, e.g., an input with just a syntax error" in {
      withRunner(Seq.empty, Success(Seq("Skipped: broken.js"))) { (runner, out) =>
        val result = runner.execute(out)
        result.parsedFiles shouldBe empty
        result.skippedFiles shouldBe List("broken.js")
      }
    }

    "continue with the files of a partially failed run" in {
      withRunner(Seq("a.json"), Failure(new RuntimeException("exit code 1"))) { (runner, out) =>
        val result = runner.execute(out)
        result.parsedFiles shouldBe List((out / "a.json").toString)
        result.skippedFiles shouldBe empty
      }
    }

    "throw if the run failed without producing any output" in {
      val cause = new RuntimeException("exit code 1")
      withRunner(Seq.empty, Failure(cause)) { (runner, out) =>
        val exception = intercept[AstGenFailedException](runner.execute(out))
        exception.getCause shouldBe cause
        exception.getMessage should include("stubastgen")
      }
    }
  }

}
