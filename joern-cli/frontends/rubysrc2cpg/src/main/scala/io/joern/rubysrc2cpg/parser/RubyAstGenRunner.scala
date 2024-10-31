package io.joern.rubysrc2cpg.parser

import better.files.File
import io.joern.rubysrc2cpg.Config
import io.joern.x2cpg.astgen.AstGenRunner.{AstGenProgramMetaData, executableDir}
import io.joern.x2cpg.astgen.AstGenRunnerBase
import io.joern.x2cpg.utils.Environment.OperatingSystemType
import io.joern.x2cpg.utils.{Environment, ExternalCommand}
import org.jruby.RubyInstanceConfig
import org.jruby.embed.{LocalContextScope, LocalVariableBehavior, PathType, ScriptingContainer}
import org.slf4j.LoggerFactory

import java.io.File.separator
import java.io.{ByteArrayOutputStream, InputStream, PrintStream}
import java.nio.file.attribute.PosixFilePermission
import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.util.jar.JarFile
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try, Using}
import java.util

class RubyAstGenRunner(config: Config) extends AstGenRunnerBase(config) {

  private val logger = LoggerFactory.getLogger(getClass)

  override def fileFilter(file: String, out: File): Boolean = {
    file.stripSuffix(".json").replace(out.pathAsString, config.inputPath) match {
      case filePath if isIgnoredByUserConfig(filePath) => false
      case filePath if filePath.endsWith(".csproj")    => false
      case _                                           => true
    }
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
      case s"W, [$_]  WARN -- : $reason - $fileName"   => addReason(reason, Option(fileName))
      case s"E, [$_] ERROR -- : '$fileName' - $reason" => addReason(reason, Option(fileName))
      case s"E, [$_] ERROR -- : Failed to parse $fileName: $reason" =>
        addReason(s"Failed to parse: $reason", Option(fileName))
      case s"I, [$_]  INFO -- : Processed: $fileName -> $_" => diagnosticMap.put(fileName, Nil)
      case s"I, [$_]  INFO -- : Excluding: $fileName"       => addReason("Skipped", Option(fileName))
      case _                                                => // ignore
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
      Using.resource(prepareExecutionEnvironment("ruby_ast_gen")) { env =>
        val cwd            = env.path.toAbsolutePath.toString
        val excludeCommand = if (exclude.isEmpty) "" else s"-e \"$exclude\""
        val gemPath        = Seq(cwd, "vendor", "bundle", "jruby", "3.1.0").mkString(separator)
        val rubyArgs       = Array("--log", "info", "-o", out.toString(), "-i", in, excludeCommand)
        val mainScript     = Seq(cwd, "exe", "ruby_ast_gen").mkString(separator)
        if (config.tryLocalRuby) {
          executeWithNativeRuby(mainScript, cwd, rubyArgs, gemPath)
            .recoverWith(throwable =>
              logger.warn(
                s"Encountered exception while executing with native Ruby, trying with JRuby: '${throwable.getMessage}'"
              )
              executeWithJRuby(mainScript, cwd, rubyArgs, gemPath)
            )
        } else {
          executeWithJRuby(mainScript, cwd, rubyArgs, gemPath)
        }
      }
    } catch {
      case tempPathException: Exception => Failure(tempPathException)
    }
  }

  private def executeWithNativeRuby(
    mainScript: String,
    cwd: String,
    rubyArgs: Array[String],
    gemPath: String
  ): Try[Seq[String]] = {
    val isCompatible =
      ExternalCommand.run("ruby" :: "--version" :: Nil, ".").successOption.map(_.mkString("\n").strip()) match {
        case Some(s"ruby $major.$minor.$patch ($_) [$_]") =>
          major.toInt == 3 && ((minor.toInt == 0 && patch.toInt >= 4) || (minor.toInt >= 1))
        case Some(versionOutput) => throw new RuntimeException(s"Unable to parse Ruby version from $versionOutput")
        case None                => throw new RuntimeException(s"Unable to execute native Ruby binary")
      }
    if (isCompatible) {
      ExternalCommand
        .run(mainScript +: rubyArgs.toSeq, cwd, extraEnv = Map("GEM_PATH" -> gemPath, "GEM_FILE" -> gemPath))
        .toTry
    } else {
      throw new RuntimeException("Non compatible Ruby version detected. Ruby version of at least 3.0.4 is required.")
    }
  }

  private def executeWithJRuby(
    mainScript: String,
    cwd: String,
    rubyArgs: Array[String],
    gemPath: String
  ): Try[Seq[String]] = {
    val outStream = new ByteArrayOutputStream()
    val errStream = new ByteArrayOutputStream()
    val container = new ScriptingContainer(LocalContextScope.SINGLETHREAD, LocalVariableBehavior.TRANSIENT)
    val config    = container.getProvider.getRubyInstanceConfig
    container.setCompileMode(RubyInstanceConfig.CompileMode.OFF)
    container.setNativeEnabled(false)
    container.setObjectSpaceEnabled(true)
    container.setCurrentDirectory(cwd)
    container.setOutput(new PrintStream(outStream))
    container.setError(new PrintStream(errStream))
    config.setLoadGemfile(true)
    container.setArgv(rubyArgs)
    container.setEnvironment(Map("GEM_PATH" -> gemPath, "GEM_FILE" -> gemPath).asJava)
    config.setHasShebangLine(true)
    config.setHardExit(false)

    try {
      container.runScriptlet(PathType.ABSOLUTE, mainScript)
      val consoleOut = outStream.toString.split("\n").toIndexedSeq ++ errStream.toString.split("\n")
      Success(consoleOut)
    } catch {
      case e: Exception => Failure(e)
    }
  }

  private def prepareExecutionEnvironment(resourceDir: String): ExecutionEnvironment = {
    val resourceUrl = getClass.getClassLoader.getResource(resourceDir)
    if (resourceUrl == null) {
      throw new IllegalArgumentException(s"Resource directory '$resourceDir' not found in JAR.")
    }

    def setFilePerms(targetPath: Path): Unit = {
      val execPerms =
        util.Set.of(PosixFilePermission.OWNER_EXECUTE, PosixFilePermission.OWNER_READ, PosixFilePermission.OWNER_WRITE)

      if (Environment.operatingSystem != OperatingSystemType.Windows)
        Files.setPosixFilePermissions(targetPath, execPerms)
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
              if entryPath.endsWith("ruby_ast_gen") then setFilePerms(entryPath)
            } finally {
              inputStream.close()
            }
          }
        }
        TempDir(tempPath)
      case "file" =>
        val resourcePath = Paths.get(resourceUrl.toURI)
        val mainScript   = resourcePath.resolve("exe").resolve("ruby_ast_gen")
        setFilePerms(mainScript)
        LocalDir(resourcePath)
      case x =>
        throw new IllegalArgumentException(s"Resources is within an unsupported environment '$x'.")
    }
  }

  private sealed trait ExecutionEnvironment extends AutoCloseable {
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

}
