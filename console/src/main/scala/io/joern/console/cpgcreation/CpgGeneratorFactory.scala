package io.joern.console.cpgcreation

import better.files.Dsl.*
import better.files.File
import io.shiftleft.codepropertygraph.cpgloading.CpgLoader
import io.shiftleft.codepropertygraph.generated.Languages
import io.joern.console.{ConsoleConfig, CpgConverter}

import java.nio.file.Path
import scala.util.Try

object CpgGeneratorFactory {
  private val KNOWN_LANGUAGES = Set(
    Languages.C,
    Languages.CSHARP,
    Languages.GOLANG,
    Languages.GHIDRA,
    Languages.JAVA,
    Languages.JAVASCRIPT,
    Languages.JSSRC,
    Languages.PYTHON,
    Languages.PYTHONSRC,
    Languages.LLVM,
    Languages.PHP,
    Languages.KOTLIN,
    Languages.NEWC,
    Languages.JAVASRC,
    Languages.SWIFTSRC
  )
}

class CpgGeneratorFactory(config: ConsoleConfig) {

  /** For a given input path, try to guess a suitable generator and return it
    */
  def forCodeAt(inputPath: String): Option[CpgGenerator] =
    for {
      language     <- guessLanguage(inputPath)
      cpgGenerator <- cpgGeneratorForLanguage(language, config.frontend, config.install.rootPath.path, args = Nil)
    } yield {
      report(s"Using generator for language: $language: ${cpgGenerator.getClass.getSimpleName}")
      cpgGenerator
    }

  /** For a language, return the generator
    */
  def forLanguage(language: String): Option[CpgGenerator] = {
    Option(language)
      .filter(languageIsKnown)
      .flatMap { lang =>
        cpgGeneratorForLanguage(lang, config.frontend, config.install.rootPath.path, args = Nil)
      }
  }

  def languageIsKnown(language: String): Boolean = CpgGeneratorFactory.KNOWN_LANGUAGES.contains(language)

  def runGenerator(generator: CpgGenerator, inputPath: String, outputPath: String): Try[Path] = {
    val outputFileOpt: Try[File] =
      generator.generate(inputPath, outputPath).map(File(_))
    outputFileOpt.map { outFile =>
      val parentPath = outFile.parent.path.toAbsolutePath
      if (CpgLoader.isProtoFormat(outFile.path)) {
        report("Creating database from bin.zip")
        val srcFilename = outFile.path.toAbsolutePath.toString
        val dstFilename = parentPath.resolve("cpg.bin").toAbsolutePath.toString
        // MemoryHelper.hintForInsufficientMemory(srcFilename).map(report)
        convertProtoCpgToFlatgraph(srcFilename, dstFilename)
      } else {
        report("moving cpg.bin.zip to cpg.bin because it is already a database file")
        val srcPath = parentPath.resolve("cpg.bin.zip")
        if (srcPath.toFile.exists()) {
          mv(srcPath, parentPath.resolve("cpg.bin"))
        }
      }
      parentPath
    }
  }

  @deprecated("method got renamed to `convertProtoCpgToFlatgraph, please use that instead", "joern v3")
  def convertProtoCpgToOverflowDb(srcFilename: String, dstFilename: String): Unit =
    convertProtoCpgToFlatgraph(srcFilename, dstFilename)

  def convertProtoCpgToFlatgraph(srcFilename: String, dstFilename: String): Unit = {
    CpgConverter.convertProtoCpgToFlatgraph(srcFilename, dstFilename)
    File(srcFilename).delete()
  }

  private def report(str: String): Unit = System.err.println(str)

}
