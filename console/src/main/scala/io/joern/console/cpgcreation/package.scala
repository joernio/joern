package io.joern.console

import io.shiftleft.codepropertygraph.generated.Languages
import better.files.File

import java.nio.file.Path
import scala.collection.mutable

package object cpgcreation {

  /** For a given language, return CPG generator script
    */
  def cpgGeneratorForLanguage(
    language: String,
    config: FrontendConfig,
    rootPath: Path,
    args: List[String]
  ): Option[CpgGenerator] = {
    lazy val cCpgGenerator      = CCpgGenerator(config.withArgs(args), rootPath)
    lazy val fuzzycCpgGenerator = FuzzyCCpgGenerator(config.withArgs(args), rootPath)
    language match {
      case Languages.CSHARP     => Some(CSharpCpgGenerator(config.withArgs(args), rootPath))
      case Languages.C          => Seq(fuzzycCpgGenerator, cCpgGenerator).find(_.isAvailable)
      case Languages.LLVM       => Some(LlvmCpgGenerator(config.withArgs(args), rootPath))
      case Languages.GOLANG     => Some(GoCpgGenerator(config.withArgs(args), rootPath))
      case Languages.JAVA       => Some(JavaCpgGenerator(config.withArgs(args), rootPath))
      case Languages.JAVASRC    => Some(JavaSrcCpgGenerator(config.withArgs(args), rootPath))
      case Languages.JAVASCRIPT => Some(JsCpgGenerator(config.withArgs(args), rootPath))
      case Languages.JSSRC      => Some(JsSrcCpgGenerator(config.withArgs(args), rootPath))
      case Languages.PYTHONSRC  => Some(PythonSrcCpgGenerator(config.withArgs(args), rootPath))
      case Languages.PHP        => Some(PhpCpgGenerator(config.withArgs(args), rootPath))
      case Languages.GHIDRA     => Some(GhidraCpgGenerator(config.withArgs(args), rootPath))
      case Languages.NEWC       => Seq(cCpgGenerator, fuzzycCpgGenerator).find(_.isAvailable)
      case Languages.KOTLIN     => Some(KotlinCpgGenerator(config.withArgs(args), rootPath))
      case _                    => None
    }
  }

  /** Heuristically determines language by inspecting file/dir at path.
    */
  def guessLanguage(path: String): Option[String] = {
    val file = File(path)
    if (file.isDirectory) {
      guessMajorityLanguageInDir(file)
    } else {
      guessLanguageForRegularFile(file)
    }
  }

  /** Guess the main language for an entire directory (e.g. a whole project), based on a group count of all individual
    * files. Rationale: many projects contain files from different languages, but most often one language is standing
    * out in numbers.
    */
  private def guessMajorityLanguageInDir(directory: File): Option[String] = {
    assert(directory.isDirectory, s"$directory must be a directory, but wasn't")
    val groupCount = mutable.Map.empty[String, Int].withDefaultValue(0)

    for {
      file <- directory.listRecursively
      if file.isRegularFile
      guessedLanguage <- guessLanguageForRegularFile(file)
    } {
      val oldValue = groupCount(guessedLanguage)
      groupCount.update(guessedLanguage, oldValue + 1)
    }

    groupCount.toSeq.sortBy(_._2).lastOption.map(_._1)
  }

  private def guessLanguageForRegularFile(file: File): Option[String] = {
    file.name.toLowerCase match {
      case f if f.endsWith(".jar") || f.endsWith(".war") || f.endsWith(".ear") || f.endsWith(".apk") =>
        Some(Languages.JAVA)
      case f if f.endsWith(".csproj") || f.endsWith(".cs") => Some(Languages.CSHARP)
      case f if f.endsWith(".go") || Set("gopkg.lock", "gopkg.toml", "go.mod", "go.sum").contains(f) =>
        Some(Languages.GOLANG)
      case f if f.endsWith(".js") || f == "package.json" => Some(Languages.JAVASCRIPT)
      case f if f.endsWith(".java")                      => Some(Languages.JAVASRC)
      case f if f.endsWith(".class")                     => Some(Languages.JAVA)
      case f if f.endsWith(".kt")                        => Some(Languages.KOTLIN)
      case f if f.endsWith(".php")                       => Some(Languages.PHP)
      case f if f.endsWith(".py")                        => Some(Languages.PYTHONSRC)
      case f if f.endsWith(".bc") || f.endsWith(".ll")   => Some(Languages.LLVM)
      case f if isCFile(f)                               => Some(Languages.NEWC)
      case _                                             => None
    }
  }

  /** check if given filename looks like it might be a C/CPP source or header file mostly copied from
    * io.joern.c2cpg.parser.FileDefaults
    */
  private def isCFile(filename: String): Boolean =
    Seq(".c", ".cc", ".cpp", ".h", ".hpp", ".hh").exists(filename.endsWith)

}
