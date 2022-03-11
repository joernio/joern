package io.joern.x2cpg

import better.files.File
import io.joern.x2cpg.X2Cpg.withErrorsToConsole
import io.shiftleft.codepropertygraph.Cpg
import org.slf4j.LoggerFactory
import overflowdb.Config
import scopt.OParser

import scala.util.{Failure, Success, Try}

object X2CpgConfig {
  def defaultOutputPath: String = "cpg.bin"
}

trait X2CpgConfig[R] {
  def withAdditionalInputPath(inputPath: String): R
  def withOutputPath(x: String): R
}

trait X2CpgFrontend[T <: X2CpgConfig[_]] {

  /** Create a CPG according to given configuration. Returns CPG wrapped in a `Try`, making it possible to detect and
    * inspect exceptions in CPG generation.
    */
  def createCpg(config: T): Try[Cpg]

  /** Create CPG according to given configuration, printing errors to the console if they occur. The CPG is not
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

  /** Create a CPG for code at `inputNames` with default frontend configuration. If `outputName` exists, it is the file
    * name of the resulting CPG. Otherwise, the CPG is held in memory.
    */
  def createCpg(inputNames: List[String], outputName: Option[String])(implicit defaultConfig: T): Try[Cpg] = {
    val defaultWithInputPaths = inputNames
      .foldLeft(defaultConfig) { (c, x) => c.withAdditionalInputPath(x).asInstanceOf[T] }
    val config = if (!outputName.contains(X2CpgConfig.defaultOutputPath)) {
      if (outputName.isEmpty) {
        defaultWithInputPaths.withOutputPath("").asInstanceOf[T]
      } else {
        defaultWithInputPaths.withOutputPath(outputName.get).asInstanceOf[T]
      }
    } else {
      defaultWithInputPaths
    }
    createCpg(config)
  }

  /** Create a CPG for code at `inputName` (a single location) with default frontend configuration. If `outputName`
    * exists, it is the file name of the resulting CPG. Otherwise, the CPG is held in memory.
    */
  def createCpg(inputName: String, outputName: Option[String])(implicit defaultConfig: T): Try[Cpg] = {
    createCpg(List(inputName), outputName)(defaultConfig)
  }

  def createCpg(inputName: String)(implicit defaultConfig: T): Try[Cpg] = createCpg(inputName, None)(defaultConfig)
  def createCpg(inputNames: List[String])(implicit defaultConfig: T): Try[Cpg] =
    createCpg(inputNames, None)(defaultConfig)
}

object X2Cpg {

  private val logger = LoggerFactory.getLogger(X2Cpg.getClass)

  /** Parse commands line arguments in `args` using an X2Cpg command line parser, extended with the frontend specific
    * options in `frontendSpecific` with the initial configuration set to `initialConf`.
    *
    * On success, the configuration is returned wrapped into an Option. On failure, error messages are printed and
    * `None` is returned.
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
      arg[String]("input-dirs")
        .unbounded()
        .text("list of source files and/or source directories")
        .action((x, c) => c.withAdditionalInputPath(x)),
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
    Try {
      f(config)
    } match {
      case Failure(exception) =>
        println(exception.getMessage)
        exception.printStackTrace()
        Failure(exception)
      case Success(v) => v
    }
  }

}
