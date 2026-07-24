package io.joern.swiftsrc2cpg.utils

import io.joern.swiftsrc2cpg.Config
import io.joern.x2cpg.astgen.AstGenRunner.AstGenProgramMetaData
import io.joern.x2cpg.utils.Environment
import io.joern.x2cpg.utils.Environment.{ArchitectureType, OperatingSystemType}
import org.slf4j.LoggerFactory

import java.nio.file.Path
import java.util.regex.Pattern
import scala.util.Try
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

  override protected def skippedFiles(in: Path, astGenOut: List[String]): List[String] = {
    val skipped = astGenOut.collect {
      case out if !out.startsWith("Generated") =>
        val filename = out.substring(out.indexOf(": `") + 3, out.indexOf("swift`") + 5)
        val reason   = out.substring(out.indexOf("` ") + 2)
        logger.warn(s"\t- failed to parse '$filename': '$reason'")
        Option(filename)
      case out =>
        logger.debug(s"\t+ $out")
        None
    }
    skipped.flatten
  }

  override protected def runAstGenNative(in: String, out: Path, exclude: String, include: String): Try[Seq[String]] = {
    val excludeArgs = if (exclude.nonEmpty) Seq("--exclude-regex", exclude) else Seq.empty
    ExternalCommand.run(Seq(astGenCommand, "-o", out.toString) ++ excludeArgs, in)
  }

}
