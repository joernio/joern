package io.joern.rubysrc2cpg.parser

import better.files.File
import io.joern.rubysrc2cpg.Config
import io.joern.rubysrc2cpg.parser.RubyAstGenRunner.ExecutionEnvironment
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.astgen.AstGenRunner.{AstGenProgramMetaData, AstGenRunnerResult, DefaultAstGenRunnerResult}
import io.joern.x2cpg.astgen.AstGenRunnerBase
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
class RubyAstGenRunner(config: Config) extends AstGenRunnerBase(config) with AutoCloseable {

  private val logger = LoggerFactory.getLogger(getClass)

  private val env: ExecutionEnvironment = RubyAstGenRunner.prepareExecutionEnvironment("ruby_ast_gen")
  // The scripting container is re-used as it persists any previously imported definitions. The cost to import and
  // construct class and method definitions is quite expensive, so it makes sense to persist and create the container
  // on-demand.
  private val container: ScriptingContainer = {
    val cwd       = env.path.toAbsolutePath.toString
    val gemPath   = Seq(cwd, "vendor", "bundle", "jruby", "3.1.0").mkString(separator)
    val container = new ScriptingContainer(LocalContextScope.THREADSAFE, LocalVariableBehavior.TRANSIENT)
    val config    = container.getProvider.getRubyInstanceConfig
    container.setCompileMode(RubyInstanceConfig.CompileMode.OFF)
    container.setNativeEnabled(false)
    container.setObjectSpaceEnabled(true)
    container.setCurrentDirectory(cwd)
    config.setLoadGemfile(true)
    container.setEnvironment(Map("GEM_PATH" -> gemPath, "GEM_FILE" -> gemPath).asJava)
    config.setHasShebangLine(true)
    config.setHardExit(false)

    container
  }

  override def close(): Unit = {
    val closeContainer = Try(container.terminate())
    if (closeContainer.isFailure) {
      logger.error("Error occurred while terminating JRuby scripting container!", closeContainer.failed.get)
    }
    val closeEnv = Try(env.close())
    if (closeEnv.isFailure) {
      logger.error("Error occurred while cleaning up JRuby execution directory!", closeEnv.failed.get)
    }
  }

  override def fileFilter(file: String, out: File): Boolean = {
    file.stripSuffix(".json").replace(out.pathAsString, config.inputPath) match {
      case filePath if isIgnoredByUserConfig(filePath)   => false
      case filePath if isIgnoredByDefaultRegex(filePath) => false
      case filePath if filePath.endsWith(".csproj")      => false
      case _                                             => true
    }
  }

  private def isIgnoredByDefaultRegex(filePath: String): Boolean = {
    config.defaultIgnoredFilesRegex.exists(_.matches(filePath))
  }

  override def skippedFiles(in: File, astGenOut: List[String]): List[String] = {
    val diagnosticMap = mutable.LinkedHashMap.empty[String, Seq[String]]

    def addReason(reason: String, lastFile: Option[String] = None) = {
      val key = lastFile.getOrElse(diagnosticMap.last._1)
      diagnosticMap.updateWith(key) {
        case Some(x) => Option(x :+ reason)
        case None    => Option(reason :: Nil)
      }
    }

    astGenOut.map(_.strip()).foreach {
      case s"[WARN] $reason - $fileName"  => addReason(reason, Option(fileName))
      case s"[ERR] '$fileName' - $reason" => addReason(reason, Option(fileName))
      case s"[ERR] Failed to parse $fileName: $reason" =>
        addReason(s"Failed to parse: $reason", Option(fileName))
      case s"[INFO] Processed: $fileName -> $_" => diagnosticMap.put(fileName, Nil)
      case s"[INFO] Excluding: $fileName"       => addReason("Skipped", Option(fileName))
      case _                                    => // ignore
    }

    diagnosticMap.flatMap {
      case (filename, Nil) =>
        logger.debug(s"Successfully parsed '$filename'")
        None
      case (filename, "Skipped" :: Nil) =>
        logger.debug(s"Skipped '$filename' due to file filter")
        Option(filename)
      case (filename, diagnostics) =>
        logger.warn(
          s"Parsed '$filename' with the following diagnostics:\n${diagnostics.map(x => s" - $x").mkString("\n")}"
        )
        Option(filename)
    }.toList
  }

  override def runAstGenNative(in: String, out: File, exclude: String, include: String)(implicit
    metaData: AstGenProgramMetaData
  ): Try[Seq[String]] = {
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
          |${if exclude.isEmpty then "" else s"options[:exclude] = /${exclude.replace("/", "\\/")}/"}
          |
          |if defined?(RubyAstGen) != 'constant' || defined?(RubyAstGen::parse) != 'method' then
          |  require "$requireFile"
          |end
          |RubyAstGen::parse(options)
          |""".stripMargin

      // We write this file to disk as Windows fails to resolve imports (`require` calls) when we execute the string/
      // as an argument to `container.runScriptlet` directly. Additionally, this is written to temporary files
      // as we expect that this may be called by multiple threads.
      Files.writeString(scriptTarget, mainScript, StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.CREATE)
      executeWithJRuby(scriptTarget)
    } catch {
      case tempPathException: Exception => Failure(tempPathException)
    } finally {
      scriptTarget.toFile.delete()
    }
  }

  override def execute(out: File): AstGenRunnerResult = {
    execute(out, config)
  }

  /** Extends the interfaces' `execute` function to account for possibly varying configurations when running this runner
    * for multiple executions.
    */
  def execute(out: File, specifiedConfig: Config): AstGenRunnerResult = {
    implicit val metaData: AstGenProgramMetaData = specifiedConfig.astGenMetaData
    val in                                       = File(config.inputPath)
    logger.info(s"Running ${metaData.name} on '${specifiedConfig.inputPath}'")

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

    runAstGenNative(specifiedConfig.inputPath, out, combineIgnoreRegex, "") match {
      case Success(result) =>
        val srcFiles = SourceFiles.determine(
          out.toString(),
          Set(".json"),
          ignoredDefaultRegex = Option(specifiedConfig.defaultIgnoredFilesRegex),
          ignoredFilesRegex = Option(specifiedConfig.ignoredFilesRegex),
          ignoredFilesPath = Option(specifiedConfig.ignoredFiles)
        )
        val parsed  = filterFiles(srcFiles, out)
        val skipped = skippedFiles(in, result.toList)
        DefaultAstGenRunnerResult(parsed, skipped)
      case Failure(f) =>
        logger.error(s"\t- running ${metaData.name} failed!", f)
        DefaultAstGenRunnerResult()
    }
  }

  private def executeWithJRuby(script: Path): Try[Seq[String]] = {
    Using.resources(new ByteArrayOutputStream(), new ByteArrayOutputStream()) { (outStream, errStream) =>
      container.setOutput(new PrintStream(outStream))
      container.setError(new PrintStream(errStream))
      Try {
        container.runScriptlet(PathType.ABSOLUTE, script.toString)
        (outStream.toString.split("\n").toIndexedSeq ++ errStream.toString.split("\n")).filterNot(_.isBlank)
      }
    }
  }

}

object RubyAstGenRunner {

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
              if entryPath.endsWith("ruby_ast_gen") then entryPath.toFile.setExecutable(true, true)
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
