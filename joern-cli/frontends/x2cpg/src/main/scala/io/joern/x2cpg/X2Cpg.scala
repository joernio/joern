package io.joern.x2cpg

import better.files.File
import io.joern.x2cpg.X2Cpg.withErrorsToConsole
import io.joern.x2cpg.layers.{Base, CallGraph, ControlFlow, TypeRelations}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import org.slf4j.LoggerFactory
import overflowdb.Config
import scopt.OParser

import java.io.PrintWriter
import java.nio.file.Files
import scala.util.{Failure, Success, Try}

object X2CpgConfig {
  def defaultOutputPath: String = "cpg.bin"
}

trait X2CpgConfig[R] {
  def withInputPath(inputPath: String): R
  def withOutputPath(x: String): R
}

/** Base class for `Main` classes of CPG frontends.
  *
  * Main classes that inherit from this base class parse the command line, exiting with an error code if this does not
  * succeed. On success, the method `run` is called, which evaluates, given a frontend and a configuration, creates the
  * CPG and stores it on disk.
  *
  * @param cmdLineParser
  *   parser for command line arguments
  * @param frontend
  *   the frontend to use for CPG creation
  */
abstract class X2CpgMain[T <: X2CpgConfig[T], X <: X2CpgFrontend[_]](cmdLineParser: OParser[Unit, T], frontend: X)(
  implicit defaultConfig: T
) extends App {

  /** method that evaluates frontend with configuration
    */
  def run(config: T, frontend: X): Unit

  X2Cpg.parseCommandLine(args, cmdLineParser, defaultConfig) match {
    case Some(config) =>
      try {
        run(config, frontend)
      } catch {
        case ex: Throwable =>
          println(ex.getMessage)
          ex.printStackTrace()
          System.exit(1)
      }
    case None =>
      println("Error parsing the command line")
      System.exit(1)
  }
}

/** Trait that represents a CPG generator, where T is the frontend configuration class.
  */
trait X2CpgFrontend[T <: X2CpgConfig[_]] {

  /** Create a CPG according to given configuration. Returns CPG wrapped in a `Try`, making it possible to detect and
    * inspect exceptions in CPG generation. To be provided by the frontend.
    */
  def createCpg(config: T): Try[Cpg]

  /** Create CPG according to given configuration, printing errors to the console if they occur. The CPG closed not
    * returned.
    */
  def run(config: T): Unit = {
    withErrorsToConsole(config) { _ =>
      createCpg(config) match {
        case Success(cpg) =>
          cpg.close()
          Success(cpg)
        case Failure(exception) =>
          Failure(exception)
      }
    }
  }

  /** Create a CPG for code at `inputName` (a single location) with default frontend configuration. If `outputName`
    * exists, it is the file name of the resulting CPG. Otherwise, the CPG is held in memory.
    */
  def createCpg(inputName: String, outputName: Option[String])(implicit defaultConfig: T): Try[Cpg] = {
    val defaultWithInputPath = defaultConfig.withInputPath(inputName).asInstanceOf[T]
    val config = if (!outputName.contains(X2CpgConfig.defaultOutputPath)) {
      if (outputName.isEmpty) {
        defaultWithInputPath.withOutputPath("").asInstanceOf[T]
      } else {
        defaultWithInputPath.withOutputPath(outputName.get).asInstanceOf[T]
      }
    } else {
      defaultWithInputPath
    }
    createCpg(config)
  }

  /** Create a CPG in memory for file at `inputName` with default configuration.
    */
  def createCpg(inputName: String)(implicit defaultConfig: T): Try[Cpg] = createCpg(inputName, None)(defaultConfig)

}

object X2Cpg {

  private val logger = LoggerFactory.getLogger(X2Cpg.getClass)

  /** Parse commands line arguments in `args` using an X2Cpg command line parser, extended with the frontend specific
    * options in `frontendSpecific` with the initial configuration set to `initialConf`. On success, the configuration
    * is returned wrapped into an Option. On failure, error messages are printed and, `None` is returned.
    */
  def parseCommandLine[R <: X2CpgConfig[R]](
    args: Array[String],
    frontendSpecific: OParser[_, R],
    initialConf: R
  ): Option[R] = {
    val parser = commandLineParser(frontendSpecific)
    OParser.parse(parser, args, initialConf)
  }

  /** Create a command line parser that can be extended to add options specific for the frontend.
    */
  private def commandLineParser[R <: X2CpgConfig[R]](frontendSpecific: OParser[_, R]): OParser[_, R] = {
    val builder = OParser.builder[R]
    import builder._
    OParser.sequence(
      arg[String]("input-dir")
        .text("source directory")
        .action((x, c) => c.withInputPath(x)),
      opt[String]("output")
        .abbr("o")
        .text("output filename")
        .action { (x, c) =>
          c.withOutputPath(x)
        },
      help("help").text("display this help message"),
      frontendSpecific
    )
  }

  /** Create an empty CPG, backed by the file at `optionalOutputPath` or in-memory if `optionalOutputPath` is empty.
    */
  def newEmptyCpg(optionalOutputPath: Option[String] = None): Cpg = {
    val odbConfig = optionalOutputPath
      .map { outputPath =>
        val outFile = File(outputPath)
        if (outputPath != "" && outFile.exists) {
          logger.info("Output file exists, removing: " + outputPath)
          outFile.delete()
        }
        Config.withDefaults.withStorageLocation(outputPath)
      }
      .getOrElse {
        Config.withDefaults()
      }
    Cpg.withConfig(odbConfig)
  }

  /** Apply function `applyPasses` to a newly created CPG. The CPG is wrapped in a `Try` and returned. On failure, the
    * CPG is ensured to be closed.
    */
  def withNewEmptyCpg[T <: X2CpgConfig[_]](outPath: String, config: T)(applyPasses: (Cpg, T) => Unit): Try[Cpg] = {
    val outputPath = if (outPath != "") Some(outPath) else None
    Try {
      val cpg = newEmptyCpg(outputPath)
      Try {
        applyPasses(cpg, config)
      } match {
        case Success(_) => cpg
        case Failure(exception) =>
          cpg.close()
          throw exception
      }
    }
  }

  /** Given a function that receives a configuration and returns an arbitrary result type wrapped in a `Try`, evaluate
    * the function, printing errors to the console.
    */
  def withErrorsToConsole[T <: X2CpgConfig[_]](config: T)(f: T => Try[_]): Try[_] = {
    f(config) match {
      case Failure(exception) =>
        exception.printStackTrace()
        Failure(exception)
      case Success(v) =>
        Success(v)
    }
  }

  /** For a CPG generated by a frontend, run the default passes that turn a frontend-CPG into a complete CPG.
    */
  def applyDefaultOverlays(cpg: Cpg): Unit = {
    val context = new LayerCreatorContext(cpg)
    new Base().run(context)
    new ControlFlow().run(context)
    new TypeRelations().run(context)
    new CallGraph().run(context)
  }

  /** Write `sourceCode` to a temporary file inside a temporary directory. The prefix for the temporary directory is
    * given by `tmpDirPrefix`. The suffix for the temporary file is given by `suffix`. Both file and directory are
    * deleted on exit.
    */
  def writeCodeToFile(sourceCode: String, tmpDirPrefix: String, suffix: String): java.io.File = {
    val tmpDir = Files.createTempDirectory(tmpDirPrefix).toFile
    tmpDir.deleteOnExit()
    val codeFile = java.io.File.createTempFile("Test", suffix, tmpDir)
    codeFile.deleteOnExit()
    new PrintWriter(codeFile) { write(sourceCode); close() }
    tmpDir
  }

}
