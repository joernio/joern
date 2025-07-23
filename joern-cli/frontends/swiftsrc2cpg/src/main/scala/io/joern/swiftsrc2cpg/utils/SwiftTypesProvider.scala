package io.joern.swiftsrc2cpg.utils

import com.google.gson.JsonObject
import io.joern.swiftsrc2cpg.Config
import io.shiftleft.semanticcpg.utils.ExternalCommand
import io.shiftleft.utils.IOUtils
import org.slf4j.LoggerFactory
import versionsort.VersionHelper

import java.io.StringReader
import java.nio.file.Paths
import scala.collection.mutable
import scala.util.{Try, Using}

object SwiftTypesProvider {

  private val logger = LoggerFactory.getLogger(getClass)

  private val MinimumSwiftVersion = "6.1" // This brings in the -dump-ast-format json option

  private val SwiftVersionCommand  = Seq("swift", "--version")
  private val SwiftcVersionCommand = Seq("swiftc", "--version")
  private val SwiftVersionCommands = Seq(SwiftVersionCommand, SwiftcVersionCommand)
  private val SwiftBuildCommand    = Seq("swift", "build", "--verbose")
  private val SwiftcDumpOptions    = Seq("-dump-ast", "-dump-ast-format", "json")

  /** The regular expression pattern `(?<!\\\\)` is used to split a string of spaces, but only if the space is not
    * preceded by a backslash.
    *
    *   - `(?<!\\\\)` is a negative lookbehind that checks if there is no backslash before the current position
    *   - ` ` is the actual space character after which it is split
    *
    * The pattern in the `argsFromLine` function is used to split command line arguments, preserving backslash escaped
    * spaces (`\ `) within an argument. This is important for arguments that contain spaces, such as file paths.
    *
    * Example: The string {{{file1.swift file\ with\ spaces.swift -o output}}} would be split into
    * {{{["file1.swift", "file\ with\ spaces.swift", "-o", "output"]}}}.
    */
  private val SwiftcArgSplit = "(?<!\\\\) "

  private val SwiftcIgnoredArgs =
    Seq("-v", "-incremental", "-whole-module-optimization", "-parseable-output", "-serialize-diagnostics")

  def apply(config: Config): Option[SwiftTypesProvider] = {
    config.xcodeOutputPath.map(outputPath => build(config, IOUtils.readLinesInFile(outputPath))).orElse(build(config))
  }

  private def isValidEnvironment(config: Config): Boolean = {
    val commands = if (config.xcodeOutputPath.isDefined) {
      // we do not need 'swift' on the system if the commands are taken from Xcode
      Seq(SwiftcVersionCommand)
    } else {
      SwiftVersionCommands
    }
    commands.forall { command =>
      ExternalCommand.run(command, mergeStdErrInStdOut = true).successOption match {
        case Some(outLines) =>
          val swiftVersion = outLines.find(_.startsWith("Swift version ")).map { str =>
            str.substring("Swift version ".length, str.indexOf(" ("))
          }
          swiftVersion.exists { v =>
            val isCompatible = Try(VersionHelper.compare(v, MinimumSwiftVersion)).toOption.getOrElse(-1) >= 0
            if (!isCompatible) { logger.debug(s"Found Swift version '$v' but '$MinimumSwiftVersion' is required!") }
            isCompatible
          }
        case _ =>
          logger.debug(s"No Swift version on this system found!")
          false
      }
    }
  }

  private def build(config: Config): Option[SwiftTypesProvider] = {
    if (isValidEnvironment(config)) {
      ExternalCommand
        .run(SwiftBuildCommand, mergeStdErrInStdOut = true, workingDir = Some(Paths.get(config.inputPath)))
        .successOption
        .map(outLines => build(config, outLines))
    } else None
  }

  private def argShouldBeFiltered(arg: String): Boolean = {
    SwiftcIgnoredArgs.contains(arg) || arg.startsWith("-emit") || arg == "-output-file-map"
  }

  private def parseSwiftcArgs(args: List[String]): List[String] = {
    @scala.annotation.tailrec
    def loop(remaining: List[String], result: List[String]): List[String] = {
      remaining match {
        case Nil => result.reverse
        case head :: tail if argShouldBeFiltered(head) =>
          val remainingArgs = if (tail.headOption.exists(!_.startsWith("-"))) {
            val nextFlagIdx = tail.indexWhere(_.startsWith("-"))
            if (nextFlagIdx >= 0) tail.drop(nextFlagIdx) else Nil
          } else { tail }
          loop(remainingArgs, result)
        case head :: tail => loop(tail, head :: result)
      }
    }
    loop(args, Nil)
  }

  private def isSwiftcInvocation(line: String): Boolean = {
    (line.contains("\\swiftc.exe ") || line.contains("/swiftc "))
    && line.contains(" -module-name ")
    && !line.contains(" -emit-executable ")
  }

  private def argsFromLine(line: String): List[String] = {
    line.replace("\\ -", " -").replace("/ -", " -").split(SwiftcArgSplit).toList
  }

  def build(config: Config, compilerOutput: Seq[String]): SwiftTypesProvider = {
    val swiftcInvocations       = compilerOutput.filter(isSwiftcInvocation)
    val parsedSwiftcInvocations = swiftcInvocations.map(l => parseSwiftcArgs(argsFromLine(l)) ++ SwiftcDumpOptions)
    new SwiftTypesProvider(config, parsedSwiftcInvocations)
  }
}

case class SwiftTypesProvider(config: Config, parsedSwiftcInvocations: Seq[Seq[String]]) {

  private def calculateFullname(node: JsonObject): String = {
    // TODO: do the actual calculation
    "fullname"
  }

  def retrieveDeclFullnameMapping(): Map[String, String] = {
    val mapping = mutable.HashMap.empty[String, String]
    parsedSwiftcInvocations.foreach { invocationCommand =>
      ExternalCommand
        .run(invocationCommand, mergeStdErrInStdOut = true, workingDir = Some(Paths.get(config.inputPath)))
        .successOption
        .foreach { outlines =>
          val jsonString = outlines.mkString
          Using(new StringReader(jsonString)) { reader =>
            GsonUtils.collectJsonNodesWithProperty(reader, "usr").foreach { jsonObject =>
              mapping(jsonObject.get("usr").getAsString) = calculateFullname(jsonObject)
            }
          }
        }
    }
    mapping.toMap
  }

}
