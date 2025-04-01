package io.joern.c2cpg.parser

import org.apache.commons.lang3.StringUtils

object FileDefaults {

  val CExt: String            = ".c"
  val CppExt: String          = ".cpp"
  val PreprocessedExt: String = ".i"

  private val CHeaderFileExtension: String = ".h"

  private val CppHeaderFileExtensions: Set[String] =
    Set(".hpp", ".hh", ".hp", ".hxx", ".h++", ".tcc")

  val HeaderFileExtensions: Set[String] =
    CppHeaderFileExtensions ++ Set(CHeaderFileExtension)

  private val CppSourceFileExtensions: Set[String] =
    Set(".cc", ".cxx", ".cpp", ".cp", ".ccm", ".cxxm", ".c++m")

  val CppFileExtensions: Set[String] =
    CppSourceFileExtensions ++ CppHeaderFileExtensions

  val SourceFileExtensions: Set[String] =
    CppSourceFileExtensions ++ Set(CExt)

  def hasCppFileExtension(filePath: String): Boolean =
    CppFileExtensions.exists(ext => StringUtils.endsWithIgnoreCase(filePath, ext))

  def hasSourceFileExtension(filePath: String): Boolean =
    SourceFileExtensions.exists(ext => StringUtils.endsWithIgnoreCase(filePath, ext))

  def hasPreprocessedFileExtension(filePath: String): Boolean =
    StringUtils.endsWithIgnoreCase(filePath, PreprocessedExt)

  def hasCHeaderFileExtension(filePath: String): Boolean =
    StringUtils.endsWithIgnoreCase(filePath, CHeaderFileExtension)

  def hasCFileExtension(filePath: String): Boolean =
    StringUtils.endsWithIgnoreCase(filePath, CExt)

}
