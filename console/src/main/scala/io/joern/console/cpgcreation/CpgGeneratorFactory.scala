package io.joern.console.cpgcreation

import better.files.Dsl._
import better.files.File
import io.shiftleft.codepropertygraph.cpgloading.{CpgLoader, CpgLoaderConfig}
import io.shiftleft.codepropertygraph.generated.Languages
import io.joern.console.ConsoleConfig
import overflowdb.Config

import java.nio.file.Path
import scala.util.Try

class CpgGeneratorFactory(config: ConsoleConfig) {

  /**
    * For a given input path, try to guess a suitable generator and return it
    * */
  def forCodeAt(inputPath: String): Option[CpgGenerator] = {
    guessLanguage(inputPath).flatMap { language =>
      report(s"Using generator for language: $language and frontend=${config.frontend}")
      cpgGeneratorForLanguage(language, config.frontend, config.install.rootPath.path, args = Nil)
    }
  }

  /**
    * For a language, return the generator
    * */
  def forLanguage(language: String): Option[CpgGenerator] = {
    Option(language)
      .filter(languageIsKnown)
      .flatMap { lang =>
        cpgGeneratorForLanguage(
          lang,
          config.frontend,
          config.install.rootPath.path,
          args = Nil)
      }
  }

  def languageIsKnown(language: String): Boolean = {
    Set(
      Languages.C,
      Languages.CSHARP,
      Languages.GOLANG,
      Languages.JAVA,
      Languages.JAVASCRIPT,
      Languages.PYTHON,
      Languages.FUZZY_TEST_LANG,
      Languages.LLVM,
      Languages.PHP,
      Languages.KOTLIN,
      Languages.NEWC,
      Languages.JAVASRC
    ).contains(language)
  }

  def runGenerator(frontend: CpgGenerator,
                   inputPath: String,
                   outputPath: String,
                   namespaces: List[String] = List()): Option[Path] = {
    val outputFileOpt: Option[File] =
      frontend.generate(inputPath, outputPath, namespaces).map(File(_))
    outputFileOpt.map { outFile =>
      val parentPath = outFile.parent.path.toAbsolutePath
      if (isZipFile(outFile)) {
        report("Creating database from bin.zip")
        val srcFilename = outFile.path.toAbsolutePath.toString
        val dstFilename = parentPath.resolve("cpg.bin").toAbsolutePath.toString
        // MemoryHelper.hintForInsufficientMemory(srcFilename).map(report)
        convertProtoCpgToOverflowDb(srcFilename, dstFilename)
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

  def convertProtoCpgToOverflowDb(srcFilename: String, dstFilename: String): Unit = {
    val odbConfig = Config.withDefaults.withStorageLocation(dstFilename)
    val config = CpgLoaderConfig.withDefaults.doNotCreateIndexesOnLoad.withOverflowConfig(odbConfig)
    CpgLoader.load(srcFilename, config).close
    File(srcFilename).delete()
  }

  def isZipFile(file: File): Boolean = {
    val bytes = file.bytes
    Try {
      bytes.next() == 'P' && bytes.next() == 'K'
    }.getOrElse(false)
  }

  private def report(str: String): Unit = System.err.println(str)

}
