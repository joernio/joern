package io.joern.x2cpg.utils

import org.slf4j.LoggerFactory

import java.nio.file.Paths

object Environment {

  object OperatingSystemType extends Enumeration {
    type OperatingSystemType = Value

    val Windows, Linux, Mac, Unknown = Value
  }

  object ArchitectureType extends Enumeration {
    type ArchitectureType = Value

    val X86, ARM = Value
  }

  lazy val operatingSystem: OperatingSystemType.OperatingSystemType =
    if (scala.util.Properties.isMac) OperatingSystemType.Mac
    else if (scala.util.Properties.isLinux) OperatingSystemType.Linux
    else if (scala.util.Properties.isWin) OperatingSystemType.Windows
    else OperatingSystemType.Unknown

  lazy val architecture: ArchitectureType.ArchitectureType =
    if (scala.util.Properties.propOrNone("os.arch").contains("aarch64")) ArchitectureType.ARM
    // We do not distinguish between x86 and x64. E.g, a 64 bit Windows will always lie about
    // this and will report x86 anyway for backwards compatibility with 32 bit software.
    else ArchitectureType.X86

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
