package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.Config
import io.joern.rubysrc2cpg.parser.RubyAstGenRunner.{ExecutionEnvironment, JRubyEnvironment, astGenMetaData}
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.astgen.AstGenRunner.{
  AstGenProgramMetaData,
  AstGenRunnerResult,
  DefaultAstGenRunnerResult,
  SkippedFile
}
import io.joern.x2cpg.astgen.AstGenRunner
import io.joern.x2cpg.utils.{Environment, JoernRunfilesLocator}
import io.shiftleft.semanticcpg.utils.ExternalCommandResult
import org.jruby.RubyInstanceConfig
import org.jruby.embed.{LocalContextScope, LocalVariableBehavior, PathType, ScriptingContainer}
import org.slf4j.LoggerFactory

import java.io.File.separator
import java.io.{ByteArrayOutputStream, InputStream, PrintStream}
import java.nio.file.*
import java.util
import java.util.jar.JarFile
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try, Using}

/** Creates a JRuby scripting environment using `ruby_ast_gen` within a temporary directory allowing for re-usable
  * execution.
  */
class RubyAstGenRunner(config: Config, sharedJRubyEnv: Option[JRubyEnvironment] = None)
    extends AstGenRunner(RubyAstGenRunner.astGenMetaData, config)
    with AutoCloseable {

  private val logger = LoggerFactory.getLogger(getClass)

  private val ownsEnvironment            = sharedJRubyEnv.isEmpty
  private val jrubyEnv: JRubyEnvironment = sharedJRubyEnv.getOrElse(JRubyEnvironment())
  private val env                        = jrubyEnv.env
  private val container                  = jrubyEnv.container

  override def close(): Unit = {
    if (ownsEnvironment) {
      jrubyEnv.close()
    }
  }

  override def fileFilter(file: String, out: Path): Boolean = {
    file.stripSuffix(".json").replace(out.toString, config.inputPath) match {
      case filePath if isIgnoredByUserConfig(filePath)   => false
      case filePath if isIgnoredByDefaultRegex(filePath) => false
      case filePath if filePath.endsWith(".csproj")      => false
      case _                                             => true
    }
  }

  private def isIgnoredByDefaultRegex(filePath: String): Boolean = {
    config.defaultIgnoredFilesRegex.exists(_.matches(filePath))
  }

  // ruby_ast_gen's JRuby driver logs `[INFO]/[WARN]/[ERR]` lines across both streams; diagnostics for a file may
  // arrive without the filename repeated, so we associate them with the most recently seen file.
  override def skippedFiles(in: Path, runResult: ExternalCommandResult): List[SkippedFile] = {
    val diagnosticMap = mutable.LinkedHashMap.empty[String, Seq[String]]

    def addReason(reason: String, lastFile: Option[String] = None): Unit = {
      val key = lastFile.orElse(diagnosticMap.lastOption.map(_._1))
      key.foreach { resolvedKey =>
        diagnosticMap.updateWith(resolvedKey) {
          case Some(existing) => Some(existing :+ reason)
          case None           => Some(List(reason))
        }
      }
    }

    (runResult.stdOut ++ runResult.stdErr).map(_.strip()).foreach {
      case s"[WARN] $reason - $fileName"  => addReason(reason, Option(fileName))
      case s"[ERR] '$fileName' - $reason" => addReason(reason, Option(fileName))
      case s"[ERR] Failed to parse $fileName: $reason" =>
        addReason(s"Failed to parse: $reason", Option(fileName))
      case s"[INFO] Processed: $fileName -> $_" => diagnosticMap.put(fileName, Nil)
      case s"[INFO] Excluding: $fileName"       => addReason("excluded by file filter", Option(fileName))
      case _                                    => // ignore
    }

    diagnosticMap.collect {
      case (filename, diagnostics) if diagnostics.nonEmpty =>
        SkippedFile(filename, diagnostics.mkString("; "))
    }.toList
  }

  override def runAstGenNative(in: String, out: Path, exclude: String, include: String): ExternalCommandResult = {
    val scriptTarget = Files.createTempFile("ruby_driver", ".rb")
    try {
      // We use the URI format as this is the best in terms of language agnostic importing
      val requireFile = env.path.resolve("lib").resolve("ruby_ast_gen.rb").toUri.toString
      val mainScript =
        s"""
          |options = {
          |  input: nil,
          |  output: '.ast',
          |  exclude: '^(tests?|vendor|spec)',
          |  debug: false
          |}
          |
          |options[:input] = "${in.replace("\\", "\\\\")}"
          |options[:output] = "${out.toString.replace("\\", "\\\\")}"
          |${
            if (exclude.isEmpty) { "" }
            else { s"options[:exclude] = /${exclude.replace("/", "\\/")}/" }
          }
          |
          |if (defined?(RubyAstGen) != 'constant' || defined?(RubyAstGen::parse) != 'method')
          |  require "$requireFile"
          |end
          |RubyAstGen::parse(options)
          |""".stripMargin

      // We write this file to disk as Windows fails to resolve imports (`require` calls) when we execute the string/
      // as an argument to `container.runScriptlet` directly. Additionally, this is written to temporary files
      // as we expect that this may be called by multiple threads.
      Files.writeString(scriptTarget, mainScript, StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.CREATE)
      executeWithJRuby(scriptTarget)
    } finally {
      scriptTarget.toFile.delete()
    }
  }

  override def execute(out: Path): AstGenRunnerResult = {
    execute(out, config)
  }

  /** Extends the interfaces' `execute` function to account for possibly varying configurations when running this runner
    * for multiple executions.
    */
  def execute(out: Path, specifiedConfig: Config): AstGenRunnerResult = {
    val in = Paths.get(config.inputPath)
    logger.info(s"Running ${astGenMetaData.name} on '${specifiedConfig.inputPath}'")

    val combineIgnoreRegex =
      if (
        specifiedConfig.ignoredFilesRegex
          .toString()
          .isEmpty && specifiedConfig.defaultIgnoredFilesRegex.toString.nonEmpty
      ) {
        specifiedConfig.defaultIgnoredFilesRegex.mkString("|")
      } else if (
        config.ignoredFilesRegex.toString().nonEmpty && specifiedConfig.defaultIgnoredFilesRegex.toString.isEmpty
      ) {
        specifiedConfig.ignoredFilesRegex.toString()
      } else if (
        specifiedConfig.ignoredFilesRegex.toString().nonEmpty && specifiedConfig.defaultIgnoredFilesRegex
          .toString()
          .nonEmpty
      ) {
        s"((${specifiedConfig.ignoredFilesRegex.toString()})|(${specifiedConfig.defaultIgnoredFilesRegex.mkString("|")}))"
      } else {
        ""
      }

    val runResult = Try(runAstGenNative(specifiedConfig.inputPath, out, combineIgnoreRegex, ""))
    val srcFiles = SourceFiles.determine(
      out.toString(),
      Set(".json"),
      ignoredDefaultRegex = Option(specifiedConfig.defaultIgnoredFilesRegex),
      ignoredFilesRegex = Option(specifiedConfig.ignoredFilesRegex),
      ignoredFilesPath = Option(specifiedConfig.ignoredFiles)
    )
    val parsed = filterFiles(srcFiles, out)
    runResult match {
      case Success(result) =>
        if (!isSuccess(result)) logUnsuccessfulRun(result)
        DefaultAstGenRunnerResult(parsed, collectSkippedFiles(in, result))
      case Failure(exception) =>
        logger.error(s"\t- running ${astGenMetaData.name} failed!", exception)
        DefaultAstGenRunnerResult(parsed, List.empty)
    }
  }

  private def executeWithJRuby(script: Path): ExternalCommandResult = {
    Using.resources(new ByteArrayOutputStream(), new ByteArrayOutputStream()) { (outStream, errStream) =>
      container.setOutput(new PrintStream(outStream))
      container.setError(new PrintStream(errStream))
      val runResult   = Try(container.runScriptlet(PathType.ABSOLUTE, script.toString))
      val stdOutLines = outStream.toString.split("\n").toIndexedSeq.filterNot(_.isBlank)
      val stdErrLines = errStream.toString.split("\n").toIndexedSeq.filterNot(_.isBlank)
      runResult match {
        case Success(_) => ExternalCommandResult(0, stdOutLines, stdErrLines, script.toString, None)
        case Failure(exception) =>
          ExternalCommandResult(1, stdOutLines, stdErrLines :+ exception.getMessage, script.toString, None)
      }
    }
  }

}

object RubyAstGenRunner {

  private object astGenMetaData
      extends AstGenProgramMetaData(
        name = "ruby_ast_gen",
        configPrefix = "rubysrc2cpg",
        multiArchitectureBuilds = false
      )

  /** Encapsulates the expensive JRuby runtime setup (execution environment and scripting container). Can be shared
    * across multiple RubyAstGenRunner instances to avoid repeated JRuby initialization.
    */
  class JRubyEnvironment(val env: ExecutionEnvironment, val container: ScriptingContainer) extends AutoCloseable {
    private val logger = LoggerFactory.getLogger(getClass)

    override def close(): Unit = {
      val closeContainer = Try(container.terminate())
      if (closeContainer.isFailure) {
        logger.error("Error terminating JRuby scripting container!", closeContainer.failed.get)
      }
      val closeEnv = Try(env.close())
      if (closeEnv.isFailure) {
        logger.error("Error cleaning up JRuby execution directory!", closeEnv.failed.get)
      }
    }
  }

  object JRubyEnvironment {
    private val logger = LoggerFactory.getLogger(getClass)

    private def platformSuffix: String = {
      (Environment.operatingSystem, Environment.architecture) match {
        case (Environment.OperatingSystemType.Linux, Environment.ArchitectureType.X86)     => "linux_x86"
        case (Environment.OperatingSystemType.Linux, Environment.ArchitectureType.ARMv8)   => "linux_arm"
        case (Environment.OperatingSystemType.Mac, Environment.ArchitectureType.X86)       => "macos_x86"
        case (Environment.OperatingSystemType.Mac, Environment.ArchitectureType.ARMv8)     => "macos_arm"
        case (Environment.OperatingSystemType.Windows, Environment.ArchitectureType.X86)   => "win_x86"
        case (Environment.OperatingSystemType.Windows, Environment.ArchitectureType.ARMv8) => "win_arm"
        case _ =>
          logger.warn("Could not detect OS version! Defaulting to 'Linux'.")
          "linux_x86"
      }
    }

    def apply(): JRubyEnvironment = {
      val env = JoernRunfilesLocator
        .resolve(s"rubysrc2cpg_astgen_$platformSuffix/")
        .map(path => LocalDir(Path.of(path)))
        .getOrElse(prepareExecutionEnvironment("ruby_ast_gen"))
      val cwd        = env.path.toAbsolutePath.toString
      val bundleBase = env.path.resolve("vendor").resolve("bundle").resolve("jruby")
      val rubyAbi    = Files.list(bundleBase).iterator.asScala.next().getFileName.toString
      val gemPath    = bundleBase.resolve(rubyAbi).toString
      val container  = new ScriptingContainer(LocalContextScope.THREADSAFE, LocalVariableBehavior.TRANSIENT)
      val config     = container.getProvider.getRubyInstanceConfig
      container.setCompileMode(RubyInstanceConfig.CompileMode.OFF)
      container.setNativeEnabled(true)
      container.setObjectSpaceEnabled(true)
      container.setCurrentDirectory(cwd)
      config.setLoadGemfile(false)
      container.setEnvironment(Map("GEM_PATH" -> gemPath).asJava)
      config.setHasShebangLine(true)
      config.setHardExit(false)
      new JRubyEnvironment(env, container)
    }
  }

  sealed trait ExecutionEnvironment extends AutoCloseable {
    def path: Path

    def close(): Unit = {}
  }

  private case class TempDir(path: Path) extends ExecutionEnvironment {

    override def close(): Unit = {
      def cleanUpDir(f: Path): Unit = {
        if (Files.isDirectory(f)) {
          Files.list(f).iterator.asScala.foreach(cleanUpDir)
        }
        Files.deleteIfExists(f)
      }

      cleanUpDir(path)
    }

  }

  private case class LocalDir(path: Path) extends ExecutionEnvironment

  private def prepareExecutionEnvironment(resourceDir: String): ExecutionEnvironment = {
    val resourceUrl = getClass.getClassLoader.getResource(resourceDir)
    if (resourceUrl == null) {
      throw new IllegalArgumentException(s"Resource sub-directory '$resourceDir' not found.")
    }

    resourceUrl.getProtocol match {
      case "jar" =>
        val tempPath = Files.createTempDirectory("ruby_ast_gen-")
        val jarPath  = resourceUrl.getPath.split("!")(0).stripPrefix("file:")
        val jarFile  = new JarFile(jarPath)

        val entries = jarFile.entries().asScala.filter(_.getName.startsWith(resourceDir + "/"))
        entries.foreach { entry =>
          val entryPath = tempPath.resolve(entry.getName.stripPrefix(resourceDir + "/"))
          if (entry.isDirectory) {
            Files.createDirectories(entryPath)
          } else {
            Files.createDirectories(entryPath.getParent)
            val inputStream: InputStream = jarFile.getInputStream(entry)
            try {
              Files.copy(inputStream, entryPath, StandardCopyOption.REPLACE_EXISTING)
              if (entryPath.endsWith("ruby_ast_gen")) { entryPath.toFile.setExecutable(true, true) }
            } finally {
              inputStream.close()
            }
          }
        }
        TempDir(tempPath)
      case "file" =>
        val resourcePath = Paths.get(resourceUrl.toURI)
        val mainScript   = resourcePath.resolve("exe").resolve("ruby_ast_gen")
        mainScript.toFile.setExecutable(true, false)
        LocalDir(resourcePath)
      case x =>
        throw new IllegalArgumentException(s"Resources is within an unsupported environment '$x'.")
    }
  }

}
