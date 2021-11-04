package io.joern.console

import io.shiftleft.codepropertygraph.generated.Languages
import better.files.File

import java.nio.file.Path
import scala.collection.mutable

package object cpgcreation {

  /**
    * For a given language, return CPG generator script
    * */
  def cpgGeneratorForLanguage(language: String,
                              config: FrontendConfig,
                              rootPath: Path,
                              args: List[String]): Option[CpgGenerator] = {
    language match {
      case Languages.CSHARP          => Some(CSharpCpgGenerator(config.withArgs(args), rootPath))
      case Languages.C               => Some(FuzzyCCpgGenerator(config.withArgs(args), rootPath))
      case Languages.LLVM            => Some(LlvmCpgGenerator(config.withArgs(args), rootPath))
      case Languages.GOLANG          => Some(GoCpgGenerator(config.withArgs(args), rootPath))
      case Languages.JAVA            => Some(JavaCpgGenerator(config.withArgs(args), rootPath))
      case Languages.JAVASRC         => Some(JavaSrcCpgGenerator(config.withArgs(args), rootPath))
      case Languages.JAVASCRIPT      => Some(JsCpgGenerator(config.withArgs(args), rootPath))
      case Languages.FUZZY_TEST_LANG => Some(FuzzyTestLangCpgGenerator(config.withArgs(args), rootPath))
      case Languages.PYTHON          => Some(PythonCpgGenerator(config.withArgs(args), rootPath))
      case Languages.PHP             => Some(PhpCpgGenerator(config.withArgs(args), rootPath))
      case Languages.GHIDRA          => Some(GhidraCpgGenerator(config.withArgs(args), rootPath))
      case Languages.NEWC            => Some(CCpgGenerator(config.withArgs(args), rootPath))
      case _                         => None
    }
  }

  /**
    * Heuristically determines language by inspecting file/dir at path.
    * */
  def guessLanguage(path: String): Option[String] = {
    val lowerCasePath = path.toLowerCase
    if (lowerCasePath.endsWith(".jar") ||
      lowerCasePath.endsWith(".war") ||
      lowerCasePath.endsWith(".ear") ||
      lowerCasePath.endsWith(".apk")) {
      Some(Languages.JAVA)
    } else if (lowerCasePath.endsWith("csproj")) {
      Some(Languages.CSHARP)
    } else if (lowerCasePath.endsWith(".go")) {
      Some(Languages.GOLANG)
    } else if (isLlvmSrcFile(lowerCasePath)) {
      Some(Languages.LLVM)
    } else {
      val file = File(path)
      if (file.isDirectory) determineMajorityLanguageInDir(file)
      else None
    }
  }

  private def determineMajorityLanguageInDir(directory: File): Option[String] = {
    assert(directory.isDirectory, s"$directory must be a directory, but wasn't")
    val groupCount = mutable.Map.empty[String, Int]

    for {
      file <- directory.listRecursively
      if file.isRegularFile
      guess <- guessLanguageForRegularFile(file)
    } {
      val oldValue = groupCount.getOrElse(guess, 0)
      groupCount.update(guess, oldValue + 1)
    }

    groupCount.toSeq.sortBy(_._2).lastOption.map(_._1)
  }

  private def guessLanguageForRegularFile(file: File): Option[String] = {
    assert(file.isRegularFile, s"$file must be a regular file, but wasn't")
     file.name.toLowerCase match {
      case f if f.endsWith(".go") || Set("Gopkg.lock", "Gopkg.toml", "go.mod", "go.sum").contains(f) => Some(Languages.GOLANG)
      case f if f.endsWith(".js") || f == "package.json" => Some(Languages.JAVASCRIPT)
      case f if f.endsWith(".java") => Some(Languages.JAVASRC)
      case f if f.endsWith(".class") => Some(Languages.JAVA)
      case f if f.endsWith(".php") => Some(Languages.PHP)
      case f if f.endsWith(".py") => Some(Languages.FUZZY_TEST_LANG)
      case f if isLlvmSrcFile(f) => Some(Languages.LLVM)
      case f if f.endsWith(".c") => Some(Languages.C)
    }
  }

  private def isLlvmSrcFile(fileName: String): Boolean = {
    fileName.endsWith(".bc") || fileName.endsWith(".ll")
  }

}
