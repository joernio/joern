package io.joern.swiftsrc2cpg.utils

import io.joern.swiftsrc2cpg.Config
import io.joern.x2cpg.astgen.AstGenRunner.{AstGenProgramMetaData, SkippedFile}
import io.joern.x2cpg.utils.Environment
import io.joern.x2cpg.utils.Environment.{ArchitectureType, OperatingSystemType}
import io.shiftleft.semanticcpg.utils.{ExternalCommand, ExternalCommandResult}
import org.slf4j.LoggerFactory

import java.nio.file.{Path, Paths}
import java.util.regex.Pattern
import scala.util.matching.Regex

object AstGenRunner {

  val AstGenDefaultIgnoreRegex: Seq[Regex] =
    List(
      s"\\..*${Pattern.quote(java.io.File.separator)}.*".r,
      s"__.*${Pattern.quote(java.io.File.separator)}.*".r,
      s"tests${Pattern.quote(java.io.File.separator)}.*".r,
      s"specs${Pattern.quote(java.io.File.separator)}.*".r,
      s"test${Pattern.quote(java.io.File.separator)}.*".r,
      s"spec${Pattern.quote(java.io.File.separator)}.*".r
    )

  private object astGenMetaData
      extends AstGenProgramMetaData(
        name = "SwiftAstGen",
        configPrefix = "swiftsrc2cpg",
        binEnvVar = Some("SWIFTASTGEN_BIN"),
        versionFlag = "--version",
        versionConfigKey = Some("swiftsrc2cpg.astgen_version")
      )
}

class AstGenRunner(config: Config) extends io.joern.x2cpg.astgen.AstGenRunner(AstGenRunner.astGenMetaData, config) {

  private val logger = LoggerFactory.getLogger(getClass)

  // SwiftAstGen ships a single universal macOS binary, so x86 and ARM map to the same suffix.
  override val MacX86: String   = "mac"
  override val MacArm: String   = "mac"
  override val LinuxArm: String = "linux-arm64"

  override val bazelRuleSuffixes = bazelRuleSuffixDefaults.concat(
    Map(
      (OperatingSystemType.Mac, ArchitectureType.X86)   -> "_macos",
      (OperatingSystemType.Mac, ArchitectureType.ARMv8) -> "_macos"
    )
  )

  // SwiftAstGen writes everything to stdout: `Generated AST for file: `<path>`` on success, and (inferred from the
  // previous substring-based parser, no reproducible failure fixture exists since SwiftSyntax tolerates most
  // malformed input) some `<prefix>: `<path>.swift` <reason>` shape for failures.
  private val FailureLine: Regex = """.*: `(.*?\.swift)`\s*(.*)""".r

  override protected def skippedFiles(in: Path, runResult: ExternalCommandResult): List[SkippedFile] = {
    runResult.stdOut
      .collect {
        case line if !line.startsWith("Generated") =>
          line match {
            case FailureLine(file, reason) => Some(SkippedFile(toRelativeInputPath(file, in), reason))
            case _                         => None
          }
      }
      .flatten
      .toList
  }

  // SwiftAstGen exits non-zero on Windows even on success; only treat empty output on Windows as an actual failure.
  override protected def isSuccess(runResult: ExternalCommandResult): Boolean = {
    runResult.exitCode == 0 || (scala.util.Properties.isWin && runResult.stdOut.nonEmpty)
  }

  override protected def logUnsuccessfulRun(runResult: ExternalCommandResult): Unit = {
    if (scala.util.Properties.isWin && runResult.stdOut.isEmpty && runResult.stdErr.isEmpty) {
      logger.error("""Unable to execute SwiftAstGen!
          |On Windows systems Swift needs to be installed.
          |Please see: https://www.swift.org/install/windows/
          |""".stripMargin)
    } else {
      super.logUnsuccessfulRun(runResult)
    }
  }

  override protected def runAstGenNative(
    in: String,
    out: Path,
    exclude: String,
    include: String
  ): ExternalCommandResult = {
    val excludeArgs = if (exclude.nonEmpty) Seq("--exclude-regex", exclude) else Seq.empty
    ExternalCommand.run(Seq(astGenCommand, "-o", out.toString) ++ excludeArgs, Option(Paths.get(in)))
  }

}
