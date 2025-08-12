package io.joern.x2cpg.testfixtures

import io.shiftleft.codepropertygraph.generated.Cpg

import java.io.File
import io.joern.x2cpg.X2CpgConfig

/** LanguageFrontend encapsulates the logic that translates the source code directory into CPGs
  */
trait LanguageFrontend {
  type ConfigType <: X2CpgConfig[ConfigType]

  private var config: Option[ConfigType] = None

  def setConfig(config: ConfigType): Unit = {
    if (this.config.isDefined) {
      throw new RuntimeException("Frontend config may only be set once per test")
    }
    this.config = Some(config)
  }

  def getConfig(): Option[ConfigType] = config

  /** A standard file extension for the source code files of the given language. E.g. `.c` for C language
    */
  val fileSuffix: String

  /** Generate CPG for the given source code directory
    * @param sourceCodeFile
    *   directory where source code is located
    * @return
    *   CPG representation stored in a file
    */
  def execute(sourceCodeFile: File): Cpg
}
