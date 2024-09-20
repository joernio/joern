package io.joern.rubysrc2cpg.testfixtures

import better.files.File as BFile
import io.joern.rubysrc2cpg.Config
import io.joern.rubysrc2cpg.parser.{AstPrinter, ResourceManagedParser, RubyParser}
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.utils.{ConcurrentTaskUtil, TestCodeWriter}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import org.slf4j.LoggerFactory

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.util.{Failure, Success, Using}

class RubyParserFixture
    extends RubyFrontend(
      withDownloadDependencies = false,
      disableFileContent = true,
      antlrDebugging = false,
      antlrProfiling = false
    )
    with TestCodeWriter
    with AnyWordSpecLike
    with Matchers {
  private val RubySourceFileExtensions: Set[String] = Set(".rb")
  private val logger                                = LoggerFactory.getLogger(this.getClass)
  private var fileNameCounter                       = 0

  def generateParserTasks(
    resourceManagedParser: ResourceManagedParser,
    config: Config,
    inputPath: String
  ): Iterator[() => RubyParser.ProgramContext] = {
    SourceFiles
      .determine(
        inputPath,
        RubySourceFileExtensions,
        ignoredDefaultRegex = Option(config.defaultIgnoredFilesRegex),
        ignoredFilesRegex = Option(config.ignoredFilesRegex),
        ignoredFilesPath = Option(config.ignoredFiles)
      )
      .map(fileName =>
        () =>
          resourceManagedParser.parse(BFile(config.inputPath), fileName) match {
            case Failure(exception) => throw exception
            case Success(ctx)       => ctx
          }
      )
      .iterator
  }

  def writeCode(code: String, extension: String): Path = {
    val tmpDir  = BFile.newTemporaryDirectory("x2cpgTestTmpDir").deleteOnExit()
    val tmpPath = tmpDir.path
    val codeFiles = {
      val fileName = {
        val filename = s"Test$fileNameCounter$extension"
        fileNameCounter += 1
        filename
      }

      val filePath = Path.of(fileName)
      if (filePath.getParent != null) {
        Files.createDirectories(tmpPath.resolve(filePath.getParent))
      }
      val codeAsBytes = code.getBytes(StandardCharsets.UTF_8)
      val codeFile    = tmpPath.resolve(filePath)
      Files.write(codeFile, codeAsBytes)
      codeFilePreProcessing(codeFile)
      codeFile
    }

    tmpPath
  }

  def parseCode(code: String): List[RubyParser.ProgramContext] = {
    val tempPath = writeCode(code, ".rb")

    Using.resource(new ResourceManagedParser(config.antlrCacheMemLimit)) { parser =>
      ConcurrentTaskUtil.runUsingThreadPool(generateParserTasks(parser, config, tempPath.toString)).flatMap {
        case Failure(exception) => logger.warn(s"Could not parse file, skipping - ", exception); None
        case Success(ctx)       => Option(ctx)
      }
    }
  }

  def test(code: String, expected: String = null): Unit = {
    val astPrinter = parseCode(code).headOption match {
      case Some(head) => Option(AstPrinter().visit(head))
      case None       => None
    }

    astPrinter match {
      case Some(ast) =>
        val compareTo = if (expected != null) expected else code
        ast shouldBe compareTo
      case None => fail("AST Printer failed")
    }
  }
}
