package io.joern.c2cpg.parser

object FileDefaults {

  val C_EXT: String            = ".c"
  val CPP_EXT: String          = ".cpp"
  val PREPROCESSED_EXT: String = ".i"

  val SOURCE_FILE_EXTENSIONS: Set[String] =
    Set(".c", ".cc", ".cxx", ".cpp", ".cp", ".ccm", ".cxxm", ".c++m")

  val HEADER_FILE_EXTENSIONS: Set[String] =
    Set(".h", ".hpp", ".hh", ".hp", ".hxx", ".h++", ".tcc")

  val CPP_FILE_EXTENSIONS: Set[String] =
    Set(".cc", ".cxx", ".cpp", ".cp", ".ccm", ".cxxm", ".c++m") ++
      Set(".h", ".hpp", ".hh", ".hp", ".hxx", ".h++", ".tcc")

  def isCPPFile(filePath: String): Boolean =
    CPP_FILE_EXTENSIONS.exists(filePath.endsWith)

  def isSourceFile(filePath: String): Boolean =
    SOURCE_FILE_EXTENSIONS.exists(filePath.endsWith)

}
