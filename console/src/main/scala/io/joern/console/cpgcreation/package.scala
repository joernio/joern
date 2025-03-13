package io.joern.console

import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.semanticcpg.utils.FileUtil

import java.nio.file.{Path, Paths, Files}
import scala.collection.mutable
import scala.util.Try

package object cpgcreation {

  /** For a given language, return CPG generator script
    */
  def cpgGeneratorForLanguage(
    language: String,
    config: FrontendConfig,
    rootPath: Path,
    args: List[String]
  ): Option[CpgGenerator] = {
    lazy val conf = config.withArgs(args)
    language match {
      case Languages.CSHARP             => Some(CSharpCpgGenerator(conf, rootPath))
      case Languages.CSHARPSRC          => Some(CSharpSrcCpgGenerator(conf, rootPath))
      case Languages.C | Languages.NEWC => Some(CCpgGenerator(conf, rootPath))
      case Languages.LLVM               => Some(LlvmCpgGenerator(conf, rootPath))
      case Languages.GOLANG             => Some(GoCpgGenerator(conf, rootPath))
      case Languages.JAVA               => Some(JavaCpgGenerator(conf, rootPath))
      case Languages.JAVASRC            => Some(JavaSrcCpgGenerator(conf, rootPath))
      case Languages.JSSRC | Languages.JAVASCRIPT =>
        val jssrc = JsSrcCpgGenerator(conf, rootPath)
        if (jssrc.isAvailable) Some(jssrc)
        else Some(JsCpgGenerator(conf, rootPath))
      case Languages.PYTHONSRC => Some(PythonSrcCpgGenerator(conf, rootPath))
      case Languages.PYTHON    => Some(PyCpgGenerator(conf, rootPath))
      case Languages.PHP       => Some(PhpCpgGenerator(conf, rootPath))
      case Languages.GHIDRA    => Some(GhidraCpgGenerator(conf, rootPath))
      case Languages.KOTLIN    => Some(KotlinCpgGenerator(conf, rootPath))
      case Languages.RUBYSRC   => Some(RubyCpgGenerator(conf, rootPath))
      case Languages.SWIFTSRC  => Some(SwiftSrcCpgGenerator(conf, rootPath))
      case _                   => None
    }
  }

  /** Heuristically determines language by inspecting file/dir at path.
    */
  def guessLanguage(path: String): Option[String] = {
    val file = Paths.get(path)
    if (Files.isDirectory(file)) {
      guessMajorityLanguageInDir(file)
    } else {
      guessLanguageForRegularFile(file)
    }
  }

  /** Guess the main language for an entire directory (e.g. a whole project), based on a group count of all individual
    * files. Rationale: many projects contain files from different languages, but most often one language is standing
    * out in numbers.
    */
  private def guessMajorityLanguageInDir(directory: Path): Option[String] = {
    assert(Files.isDirectory(directory), s"$directory must be a directory, but wasn't")
    val groupCount = mutable.Map.empty[String, Int].withDefaultValue(0)

    for {
      file <- directory.walk().filterNot(_ == directory)
      if Files.isRegularFile(file)
      guessedLanguage <- guessLanguageForRegularFile(file)
    } {
      val oldValue = groupCount(guessedLanguage)
      groupCount.update(guessedLanguage, oldValue + 1)
    }

    groupCount.toSeq.sortBy(_._2).lastOption.map(_._1)
  }

  private def isJavaBinary(filename: String): Boolean =
    Seq(".jar", ".war", ".ear", ".apk").exists(filename.endsWith)

  private def isCsharpFile(filename: String): Boolean =
    Seq(".csproj", ".cs").exists(filename.endsWith)

  private def isGoFile(filename: String): Boolean =
    filename.endsWith(".go") || Set("gopkg.lock", "gopkg.toml", "go.mod", "go.sum").contains(filename)

  private def isLlvmFile(filename: String): Boolean =
    Seq(".bc", ".ll").exists(filename.endsWith)

  private def isJsFile(filename: String): Boolean =
    Seq(".js", ".ts", ".jsx", ".tsx").exists(filename.endsWith) || filename == "package.json"

  /** check if given filename looks like it might be a C/CPP source or header file mostly copied from
    * io.joern.c2cpg.parser.FileDefaults
    */
  private def isCFile(filename: String): Boolean =
    Seq(".c", ".cc", ".cpp", ".h", ".hpp", ".hh").exists(filename.endsWith)

  private def guessLanguageForRegularFile(file: Path): Option[String] = {
    file.fileName.toLowerCase match {
      case f if isJavaBinary(f)      => Some(Languages.JAVA)
      case f if isCsharpFile(f)      => Some(Languages.CSHARPSRC)
      case f if isGoFile(f)          => Some(Languages.GOLANG)
      case f if isJsFile(f)          => Some(Languages.JSSRC)
      case f if f.endsWith(".java")  => Some(Languages.JAVASRC)
      case f if f.endsWith(".class") => Some(Languages.JAVA)
      case f if f.endsWith(".kt")    => Some(Languages.KOTLIN)
      case f if f.endsWith(".php")   => Some(Languages.PHP)
      case f if f.endsWith(".py")    => Some(Languages.PYTHONSRC)
      case f if f.endsWith(".rb")    => Some(Languages.RUBYSRC)
      case f if f.endsWith(".swift") => Some(Languages.SWIFTSRC)
      case f if isLlvmFile(f)        => Some(Languages.LLVM)
      case f if isCFile(f)           => Some(Languages.NEWC)
      case _                         => None
    }
  }

  def withFileInTmpFile(inputPath: String)(f: Path => Try[String]): Try[String] = {
    val dir = Files.createTempDirectory("cpgcreation")
    Paths.get(inputPath).copyToDirectory(dir)
    val result = f(dir)
    FileUtil.deleteOnExit(dir, swallowIOExceptions = true)
    result
  }

}
