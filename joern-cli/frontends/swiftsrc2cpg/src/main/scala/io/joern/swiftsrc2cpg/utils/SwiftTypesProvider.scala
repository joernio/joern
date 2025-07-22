package io.joern.swiftsrc2cpg.utils

import io.joern.swiftsrc2cpg.Config
import io.shiftleft.utils.IOUtils
import versionsort.VersionHelper

import java.nio.file.Paths
import scala.util.Try

object SwiftTypesProvider {

  private val SwiftVersionCommand  = Seq("swift", "--version")
  private val SwiftcVersionCommand = Seq("swiftc", "--version")
  private val SwiftVersionCommands = Seq(SwiftVersionCommand, SwiftcVersionCommand)
  private val SwiftBuildCommand    = Seq("swift", "build", "--verbose")
  private val SwiftcDumpOptions    = Seq("-dump-ast", "-dump-ast-format", "json")

  private val SwiftcIgnoredArgs =
    Seq("-v", "-incremental", "-whole-module-optimization", "-parseable-output", "-serialize-diagnostics")

  def apply(config: Config): Option[SwiftTypesProvider] = {
    config.xcodeOutputPath match {
      case Some(outputPath) => build(config, IOUtils.readLinesInFile(outputPath))
      case None             => build(config)
    }
  }

  private def isValidEnvironment(config: Config): Boolean = {
    val commands = if (config.xcodeOutputPath.isDefined) {
      // we do not need 'swift' on the system if the commands are taken from Xcode
      Seq(SwiftcVersionCommand)
    } else {
      SwiftVersionCommands
    }
    commands.forall { command =>
      io.shiftleft.semanticcpg.utils.ExternalCommand
        .run(command, mergeStdErrInStdOut = true)
        .successOption match {
        case Some(outLines) =>
          val swiftVersion = outLines.find(_.startsWith("Swift version ")).map { str =>
            str.substring("Swift version ".length, str.indexOf(" ("))
          }
          swiftVersion.exists(v => Try(VersionHelper.compare(v, "6.1")).toOption.getOrElse(-1) >= 0)
        case _ => false
      }
    }
  }

  def build(config: Config): Option[SwiftTypesProvider] = {
    if (isValidEnvironment(config)) {
      io.shiftleft.semanticcpg.utils.ExternalCommand
        .run(SwiftBuildCommand, mergeStdErrInStdOut = true, workingDir = Some(Paths.get(config.inputPath)))
        .successOption match {
        case Some(outLines) => build(config, outLines)
        case _              => None
      }
    } else None
  }

  private def parseSwiftcArgs(args: Array[String]): Array[String] = {
    args match {
      case a if a.isEmpty =>
        Array.empty
      case a if a.tail.isEmpty && (a.head.startsWith("-emit") || a.head == "-output-file-map") =>
        Array.empty
      case a if a.tail.nonEmpty && (a.head.startsWith("-emit") || a.head == "-output-file-map") =>
        if (a.tail.head.startsWith("-")) {
          parseSwiftcArgs(a.tail)
        } else {
          val indexForParamArgs = a.tail.indexWhere(_.startsWith("-"))
          parseSwiftcArgs(a.tail.slice(indexForParamArgs, a.tail.length))
        }
      case other =>
        other.head +: parseSwiftcArgs(other.tail)
    }
  }

  def build(config: Config, compilerOutput: Seq[String]): Option[SwiftTypesProvider] = {
    val swiftcInvocations = compilerOutput.filter { line =>
      (line.contains("\\swiftc.exe -") || line.contains("/swiftc -"))
      && line.contains(" -module-name ")
      && !line.contains(" -emit-executable ")
    }
    val parsedSwiftcCommands = swiftcInvocations.map { line =>
      val parts            = line.replace("\\ -", " -").replace("/ -", " -").split("(?<![\\\\]) ")
      val swiftcBinary     = parts.head
      val swiftcArgs       = parts.tail.filterNot(SwiftcIgnoredArgs.contains)
      val parsedSwiftcArgs = parseSwiftcArgs(swiftcArgs)
      swiftcBinary +: (parsedSwiftcArgs.toSeq ++ SwiftcDumpOptions)
    }
    Some(new SwiftTypesProvider(parsedSwiftcCommands))
  }
}

case class SwiftTypesProvider(parsedSwiftcCommands: Seq[Seq[String]]) {}
