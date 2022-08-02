package io.joern.c2cpg.parser

object FileDefaults {

  val C_EXT: String   = ".c"
  val CPP_EXT: String = ".cpp"

  private val CC_EXT           = ".cc"
  private val C_HEADER_EXT     = ".h"
  private val CPP_HEADER_EXT   = ".hpp"
  private val OTHER_HEADER_EXT = ".hh"

  val SOURCE_FILE_EXTENSIONS: Set[String] = Set(C_EXT, CC_EXT, CPP_EXT)

  val HEADER_FILE_EXTENSIONS: Set[String] = Set(C_HEADER_EXT, CPP_HEADER_EXT, OTHER_HEADER_EXT)

  private val CPP_FILE_EXTENSIONS = Set(CC_EXT, CPP_EXT, CPP_HEADER_EXT)

  def isHeaderFile(filePath: String): Boolean =
    HEADER_FILE_EXTENSIONS.exists(filePath.endsWith)

  def isCPPFile(filePath: String): Boolean =
    CPP_FILE_EXTENSIONS.exists(filePath.endsWith)
}
