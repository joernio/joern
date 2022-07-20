package io.joern.jssrc2cpg.utils

import org.slf4j.LoggerFactory
import java.nio.file.Paths

object Environment {

  val IS_WIN: Boolean   = scala.util.Properties.isWin
  val IS_MAC: Boolean   = scala.util.Properties.isMac
  val IS_LINUX: Boolean = scala.util.Properties.isLinux

  private val logger = LoggerFactory.getLogger(getClass)

  def pathExists(path: String): Boolean = {
    if (!Paths.get(path).toFile.exists()) {
      logger.error(s"Input path '$path' does not exist!")
      false
    } else {
      true
    }
  }

}
