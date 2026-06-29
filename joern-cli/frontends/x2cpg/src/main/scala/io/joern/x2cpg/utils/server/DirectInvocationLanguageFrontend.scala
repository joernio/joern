package io.joern.x2cpg.utils.server

import io.shiftleft.semanticcpg.utils.ExternalCommand
import scala.sys.env

import java.io.File
import java.nio.file.{Files, Path}

trait DirectInvocationLanguageFrontend {
  protected val envVar: String
  protected val fallbackPath: String
  protected val executablePath: String
  protected val cpgFilePrefix: String
  protected def cpgFileSuffix: String

  protected def executablePathOverride: Option[Path] = None

  private def basePath: String = env.getOrElse(envVar, fallbackPath)

  final protected def fullExecutablePath: Path =
    executablePathOverride.getOrElse(Path.of(basePath, executablePath).toRealPath())

  final def isAvailable: Boolean = Files.exists(fullExecutablePath)

  final protected def newTemporaryCpgOutputFile(cpgFilePrefix: String): File = {
    val cpgFile = File.createTempFile(cpgFilePrefix, cpgFileSuffix)
    cpgFile.deleteOnExit()
    cpgFile
  }

  protected def makeCommand(inputPath: File, cpgOutputFile: File): Seq[String]

  /** Executes the frontend process and returns the generated CPG output file. */
  def execute(inputDir: File, extraArgs: String*): File = {
    val cpgFile = newTemporaryCpgOutputFile(cpgFilePrefix)
    val command = makeCommand(inputDir, cpgFile)
    ExternalCommand.run(command ++ extraArgs).logIfFailed().verifySuccess()
    cpgFile
  }
}
