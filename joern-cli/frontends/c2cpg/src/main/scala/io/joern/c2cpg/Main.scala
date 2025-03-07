package io.joern.c2cpg

import io.joern.c2cpg.Frontend.*
import io.joern.x2cpg.{X2CpgConfig, X2CpgMain}
import io.joern.x2cpg.utils.server.FrontendHTTPServer
import io.joern.x2cpg.SourceFiles
import org.slf4j.LoggerFactory
import scopt.OParser

final case class Config(
  includePaths: Set[String] = Set.empty,
  defines: Set[String] = Set.empty,
  includeComments: Boolean = false,
  logProblems: Boolean = false,
  logPreprocessor: Boolean = false,
  printIfDefsOnly: Boolean = false,
  includePathsAutoDiscovery: Boolean = false,
  skipFunctionBodies: Boolean = false,
  withPreprocessedFiles: Boolean = false,
  compilationDatabase: Option[String] = None
) extends X2CpgConfig[Config] {
  def withIncludePaths(includePaths: Set[String]): Config = {
    this.copy(includePaths = includePaths).withInheritedFields(this)
  }

  def withDefines(defines: Set[String]): Config = {
    this.copy(defines = defines).withInheritedFields(this)
  }

  def withIncludeComments(value: Boolean): Config = {
    this.copy(includeComments = value).withInheritedFields(this)
  }

  def withLogProblems(value: Boolean): Config = {
    this.copy(logProblems = value).withInheritedFields(this)
  }

  def withLogPreprocessor(value: Boolean): Config = {
    this.copy(logPreprocessor = value).withInheritedFields(this)
  }

  def withPrintIfDefsOnly(value: Boolean): Config = {
    this.copy(printIfDefsOnly = value).withInheritedFields(this)
  }

  def withIncludePathsAutoDiscovery(value: Boolean): Config = {
    this.copy(includePathsAutoDiscovery = value).withInheritedFields(this)
  }

  def withSkipFunctionBodies(value: Boolean): Config = {
    this.copy(skipFunctionBodies = value).withInheritedFields(this)
  }

  def withPreprocessedFiles(value: Boolean): Config = {
    this.copy(withPreprocessedFiles = value).withInheritedFields(this)
  }

  def withCompilationDatabase(value: String): Config = {
    this.copy(compilationDatabase = Some(value)).withInheritedFields(this)
  }
}

private object Frontend {
  implicit val defaultConfig: Config = Config()

  val cmdLineParser: OParser[Unit, Config] = {
    val builder = OParser.builder[Config]
    import builder._
    OParser.sequence(
      programName(classOf[C2Cpg].getSimpleName),
      opt[Unit]("include-comments")
        .text(s"includes all comments into the CPG")
        .action((_, c) => c.withIncludeComments(true)),
      opt[Unit]("log-problems")
        .text(s"enables logging of all parse problems while generating the CPG")
        .action((_, c) => c.withLogProblems(true)),
      opt[Unit]("log-preprocessor")
        .text(s"enables logging of all preprocessor statements while generating the CPG")
        .action((_, c) => c.withLogPreprocessor(true)),
      opt[Unit]("print-ifdef-only")
        .text(s"prints a comma-separated list of all preprocessor ifdef and if statements; does not create a CPG")
        .action((_, c) => c.withPrintIfDefsOnly(true)),
      opt[String]("include")
        .unbounded()
        .text("header include paths")
        .action((incl, c) => c.withIncludePaths(c.includePaths + incl)),
      opt[Unit]("no-include-auto-discovery")
        .text("disables auto discovery of system header include paths")
        .hidden(),
      opt[Unit]("with-include-auto-discovery")
        .text("enables auto discovery of system header include paths")
        .action((_, c) => c.withIncludePathsAutoDiscovery(true)),
      opt[Unit]("skip-function-bodies")
        .text("instructs the parser to skip function and method bodies.")
        .action((_, c) => c.withSkipFunctionBodies(true)),
      opt[Unit]("no-image-locations")
        // deprecated, won't be removed for now to avoid breaking existing scripts
        .hidden(),
      opt[Unit]("with-preprocessed-files")
        .text("includes *.i files and gives them priority over their unprocessed origin source files.")
        .action((_, c) => c.withPreprocessedFiles(true)),
      opt[String]("define")
        .unbounded()
        .text("define a name")
        .action((d, c) => c.withDefines(c.defines + d)),
      opt[String]("compilation-database")
        .text("""enables the processing of compilation database files (e.g., compile_commands.json).
            | This allows to automatically extract compiler options, source files, and other build information from the specified database
            | and ensuring consistency with the build configuration.
            | For a cmake based build such a file is generated with the environment variable CMAKE_EXPORT_COMPILE_COMMANDS being present.
            | Clang based build are supported e.g., with https://github.com/rizsotto/Bear
            | """.stripMargin)
        .action((d, c) => c.withCompilationDatabase(SourceFiles.toAbsolutePath(d, c.inputPath)))
    )
  }

}

object Main extends X2CpgMain(cmdLineParser, new C2Cpg()) with FrontendHTTPServer[Config, C2Cpg] {

  override def run(config: Config, c2cpg: C2Cpg): Unit = {
    config match {
      case c if c.serverMode      => startup()
      case c if c.printIfDefsOnly => c2cpg.printIfDefsOnly(config)
      case _                      => c2cpg.run(config)
    }
  }

  override protected def newDefaultConfig(): Config = Config()

}
