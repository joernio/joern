package io.joern.rubysrc2cpg.utils

import better.files.File
import io.joern.rubysrc2cpg.Config
import io.joern.rubysrc2cpg.passes.AstPackagePass
import io.joern.x2cpg.utils.ExternalCommand
import io.shiftleft.codepropertygraph.Cpg
import org.slf4j.LoggerFactory

import scala.io.Source
import sys.process.*
import scala.util.{Failure, Success}

object ExternalDependenciesResolver {

  private val logger = LoggerFactory.getLogger(this.getClass)

  private def downloadGemDependency(tempPath: String, gemName: String, gemVersion: String): Unit = {
    val gemCommand = s"gem fetch $gemName ${if (gemVersion.nonEmpty) s"-v $gemVersion" else ""}"
    ExternalCommand.run(gemCommand, tempPath) match
      case Success(output) =>
        logger.info(s"Gem successfully downloaded: $tempPath")
      case Failure(exception) =>
        logger.error(s"Error while downloading dependency: ${exception.getMessage}")
  }

  private def unpackGemDependency(tempPath: String): Unit = {
    val currentDir  = File(tempPath)
    val gemFileList = currentDir.list.filter(_.extension.exists(_ == ".gem")).map(_.path.toString).toList
    gemFileList.foreach(gemFile => {
      ExternalCommand.run(s"gem unpack $gemFile", s"${tempPath.toString}/unpack") match
        case Success(output) =>
          logger.info(s"Gem unpacked Successfully: $output")
        case Failure(exception) =>
          logger.error(s"Error while unpacking: ${exception.getMessage}")
    })
  }

  private def fetchDependencyList(inputPath: String): List[(String, String)] = {
    val gemFileContent = Source.fromFile(s"$inputPath${java.io.File.separator}Gemfile").mkString
    val gemRegex       = """gem ['"]([^'"]+)['"](?:,\s*['"]([^'"]+)['"])?""".r

    gemRegex
      .findAllMatchIn(gemFileContent)
      .flatMap { matchResult =>
        val gemName    = matchResult.group(1)
        val gemVersion = Option(matchResult.group(2)).map(extractVersion => extractVersion).getOrElse("")
        Some(gemName -> gemVersion)
      }
      .toList
      .distinctBy(_._1)
  }

  private def extractVersion(version: String): String = {
    val versionRegex = """(?:[>=<~]+)\s*([\d.]+)""".r
    version match {
      case versionRegex(versionNumber) => versionNumber
      case _                           => ""
    }
  }

  def downloadDependencies(config: Config, cpg: Cpg, packageTable: PackageTable): Unit = {
    if (File(s"${config.inputPath}${java.io.File.separator}Gemfile").exists) {
      val dependenciesList = fetchDependencyList(config.inputPath)
      if (config.enableDependencyDownload && !scala.util.Properties.isWin) {
        if (checkDownloadPrerequisite()) {
          val tempDir = File.newTemporaryDirectory()
          (tempDir / "unpack").createDirectoryIfNotExists()
          try {
            dependenciesList.foreach((gemName, version) => {
              downloadGemDependency(tempDir.toString, gemName, extractVersion(version))
            })
            unpackGemDependency(tempDir.toString)
            new AstPackagePass(cpg, tempDir.toString(), packageTable, config.inputPath).createAndApply()
          } catch {
            case ex: Exception =>
              println(s"Error while parsing dependency: ${ex.getMessage}")
          } finally {
            tempDir.delete()
          }
        }
      }
    }
  }

  private def checkDownloadPrerequisite(): Boolean = {
    val isRubyInstalled = "ruby -v".! == 0
    if (!isRubyInstalled) {
      logger.error("Skipping Dependency Download: Ruby is not installed.")
    }
    isRubyInstalled
  }
}
