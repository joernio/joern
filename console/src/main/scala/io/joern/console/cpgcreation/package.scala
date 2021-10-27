package io.joern.console

import io.shiftleft.codepropertygraph.generated.Languages

import java.io.File
import java.nio.file.Path

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
    * Heuristically determines language by inspecting
    * file/dir at path.
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
    } else {
      val file = new File(path)
      if (file.isDirectory) {
        val files = file.list()
        if (files.exists(f =>
              f.endsWith(".go") || Set("Gopkg.lock", "Gopkg.toml", "go.mod", "go.sum")
                .contains(f))) {
          Some(Languages.GOLANG)
        } else if (files.exists(f => f.endsWith(".java") || f.endsWith(".class"))) {
          Some(Languages.JAVA)
        } else if (files.exists(f => f.endsWith(".php"))) {
          Some(Languages.PHP)
        } else if (files.exists(f => f.endsWith(".js") || Set("package.json").contains(f))) {
          Some(Languages.JAVASCRIPT)
        } else if (files.exists(f => f.endsWith(".py"))) {
          Some(Languages.FUZZY_TEST_LANG)
        } else if (files.exists(f => isLlvmSrcFile(new File(f).toPath))) {
          Some(Languages.LLVM)
        } else {
          Some(Languages.C)
        }
      } else if (file.isFile) {
        if (isLlvmSrcFile(file.toPath)) {
          Some(Languages.LLVM)
        } else {
          None
        }
      } else {
        None
      }
    }
  }

  private def isLlvmSrcFile(path: Path): Boolean = {
    val filename = path.getFileName.toString
    filename.endsWith(".bc") || filename.endsWith(".ll")
  }

}
