package io.joern.rubysrc2cpg.parser

import better.files.File
import io.joern.rubysrc2cpg.Config
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.astgen.AstGenRunner.{AstGenProgramMetaData, AstGenRunnerResult, DefaultAstGenRunnerResult}
import io.joern.x2cpg.astgen.AstGenRunnerBase
import org.jruby.RubyInstanceConfig
import org.jruby.embed.{LocalContextScope, LocalVariableBehavior, ScriptingContainer}
import org.slf4j.LoggerFactory

import java.io.File.separator
import java.io.{ByteArrayOutputStream, InputStream, PrintStream}
import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.util
import java.util.jar.JarFile
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try, Using}

class RubyAstGenRunner(config: Config) extends AstGenRunnerBase(config) {

  private val logger = LoggerFactory.getLogger(getClass)

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
    try {
      val mainScript =
        s"""
          |options = {
          |  input: nil,
          |  output: '.ast',
          |  exclude: '^(tests?|vendor|spec)',
          |  debug: false
          |}
          |
          |options[:input] = "$in"
          |options[:output] = "$out"
          |${if exclude.isEmpty then "" else s"options[:exclude] = /$exclude/"}
          |
          |RubyAstGen::parse(options)
          |""".stripMargin
      RubyAstGenRunner.executeWithJRuby(mainScript)
    } catch {
      case tempPathException: Exception => Failure(tempPathException)
    }
  }

  override def execute(out: File): AstGenRunnerResult = {
    implicit val metaData: AstGenProgramMetaData = config.astGenMetaData
    val in                                       = File(config.inputPath)
    logger.info(s"Running ${metaData.name} on '${config.inputPath}'")

    val combineIgnoreRegex =
      if (config.ignoredFilesRegex.toString().isEmpty && config.defaultIgnoredFilesRegex.toString.nonEmpty) {
        config.defaultIgnoredFilesRegex.mkString("|")
      } else if (config.ignoredFilesRegex.toString().nonEmpty && config.defaultIgnoredFilesRegex.toString.isEmpty) {
        config.ignoredFilesRegex.toString()
      } else if (config.ignoredFilesRegex.toString().nonEmpty && config.defaultIgnoredFilesRegex.toString().nonEmpty) {
        s"((${config.ignoredFilesRegex.toString()})|(${config.defaultIgnoredFilesRegex.mkString("|")}))"
      } else {
        ""
      }

    runAstGenNative(config.inputPath, out, combineIgnoreRegex, "") match {
      case Success(result) =>
        val srcFiles = SourceFiles.determine(
          out.toString(),
          Set(".json"),
          ignoredDefaultRegex = Option(config.defaultIgnoredFilesRegex),
          ignoredFilesRegex = Option(config.ignoredFilesRegex),
          ignoredFilesPath = Option(config.ignoredFiles)
        )
        val parsed  = filterFiles(srcFiles, out)
        val skipped = skippedFiles(in, result.toList)
        DefaultAstGenRunnerResult(parsed, skipped)
      case Failure(f) =>
        logger.error(s"\t- running ${metaData.name} failed!", f)
        DefaultAstGenRunnerResult()
    }
  }

}

object RubyAstGenRunner {

  private var isReady: Boolean = false
  lazy val container: ScriptingContainer = {
    val env = RubyAstGenRunner.env
    val cwd = env.path.toAbsolutePath.toString
    val gemPath = Seq(cwd, "vendor", "bundle", "jruby", "3.1.0").mkString(separator)
    val container = new ScriptingContainer(LocalContextScope.THREADSAFE, LocalVariableBehavior.TRANSIENT)
    val config = container.getProvider.getRubyInstanceConfig
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
  lazy val env: ExecutionEnvironment = prepareExecutionEnvironment("ruby_ast_gen")
  sys.addShutdownHook {
    container.terminate()
    env.close()
  }

  private def executeWithJRuby(mainScript: String): Try[Seq[String]] = {
    val container = RubyAstGenRunner.container
    val env = RubyAstGenRunner.env
    val cwd = env.path.toAbsolutePath.toString
    if (!isReady) {
      // initialize program in script container
      val script =
        s"""
           |libs = File.expand_path("$cwd/vendor/bundle/ruby/**/gems/**/lib", __FILE__)
           |$$LOAD_PATH.unshift *Dir.glob(libs)
           |require "$cwd/lib/ruby_ast_gen.rb"
           |""".stripMargin
      isReady = true
      container.runScriptlet(script)
    }
    Using.resources(new ByteArrayOutputStream(), new ByteArrayOutputStream()) { (outStream, errStream) =>
      container.setOutput(new PrintStream(outStream))
      container.setError(new PrintStream(errStream))
      Try {
        container.runScriptlet(mainScript)
        outStream.toString.split("\n").toIndexedSeq ++ errStream.toString.split("\n")
      }
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
        val jarPath = resourceUrl.getPath.split("!")(0).stripPrefix("file:")
        val jarFile = new JarFile(jarPath)

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
        val mainScript = resourcePath.resolve("exe").resolve("ruby_ast_gen")
        mainScript.toFile.setExecutable(true, false)
        LocalDir(resourcePath)
      case x =>
        throw new IllegalArgumentException(s"Resources is within an unsupported environment '$x'.")
    }
  }

}