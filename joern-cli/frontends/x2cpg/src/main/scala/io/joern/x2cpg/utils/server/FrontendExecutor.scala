package io.joern.x2cpg.utils.server

import java.io.File

trait FrontendExecutor {
  def execute(input: File, extraArgs: String*): File

  def isAvailable: Boolean

  def shutdown(): Unit = {}
}

object FrontendExecutor {
  def newTemporaryCpgOutputFile(prefix: String, suffix: String): File = {
    val cpgFile = File.createTempFile(prefix, suffix)
    cpgFile.deleteOnExit()
    cpgFile
  }
}
