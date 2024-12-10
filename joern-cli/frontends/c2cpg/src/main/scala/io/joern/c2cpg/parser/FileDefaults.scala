package io.joern.c2cpg.parser

import org.apache.commons.lang3.StringUtils

object FileDefaults {

  val CExt: String            = ".c"
  val CppExt: String          = ".cpp"
  val PreprocessedExt: String = ".i"

  val HeaderFileExtensions: Set[String] =
    Set(".h", ".hpp", ".hh", ".hp", ".hxx", ".h++", ".tcc")

  val CppSourceFileExtensions: Set[String] =
    Set(".cc", ".cxx", ".cpp", ".cp", ".ccm", ".cxxm", ".c++m")

  val CppFileExtensions: Set[String] =
    CppSourceFileExtensions ++ HeaderFileExtensions

  val SourceFileExtensions: Set[String] =
    CppSourceFileExtensions ++ Set(CExt)

  def hasCppFileExtension(filePath: String): Boolean =
    CppFileExtensions.exists(ext => StringUtils.endsWithIgnoreCase(filePath, ext))

  def hasSourceFileExtension(filePath: String): Boolean =
    SourceFileExtensions.exists(ext => StringUtils.endsWithIgnoreCase(filePath, ext))

  def hasPreprocessedFileExtension(filePath: String): Boolean =
    StringUtils.endsWithIgnoreCase(filePath, PreprocessedExt)
}
