package io.joern.console.cpgcreation

import better.files.File
import io.joern.console.workspacehandling.Project
import io.joern.console.{ConsoleException, FrontendConfig, Reporting}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import overflowdb.traversal.help.Table

import java.nio.file.Path
import scala.util.{Failure, Success, Try}

class ImportCode[T <: Project](console: io.joern.console.Console[T]) extends Reporting {
  import io.joern.console.Console.*

  private val config             = console.config
  private val workspace          = console.workspace
  protected val generatorFactory = new CpgGeneratorFactory(config)

  private def checkInputPath(inputPath: String): Unit = {
    if (!File(inputPath).exists) {
      throw new ConsoleException(s"Input path does not exist: '$inputPath'")
    }
  }

  /** This is the `importCode(...)` method exposed on the console. It attempts to find a suitable CPG generator first by
    * looking at the `language` parameter and if no generator is found for the language, looking the contents at
    * `inputPath` to determine heuristically which generator to use.
    */
  def apply(inputPath: String, projectName: String = "", language: String = ""): Cpg = {
    checkInputPath(inputPath)
    if (language != "") {
      generatorFactory.forLanguage(language) match {
        case None           => throw new ConsoleException(s"No CPG generator exists for language: $language")
        case Some(frontend) => apply(frontend, inputPath, projectName)
      }
    } else {
      generatorFactory.forCodeAt(inputPath) match {
        case None           => throw new ConsoleException(s"No suitable CPG generator found for: $inputPath")
        case Some(frontend) => apply(frontend, inputPath, projectName)
      }
    }
  }

  def c: SourceBasedFrontend    = new CFrontend("c")
  def cpp: SourceBasedFrontend  = new CFrontend("cpp", extension = "cpp")
  def java: SourceBasedFrontend = new SourceBasedFrontend("java", Languages.JAVASRC, "Java Source Frontend", "java")
  def jvm: Frontend =
    new BinaryFrontend("jvm", Languages.JAVA, "Java/Dalvik Bytecode Frontend (based on SOOT's jimple)")
  def ghidra: Frontend = new BinaryFrontend("ghidra", Languages.GHIDRA, "ghidra reverse engineering frontend")
  def kotlin: SourceBasedFrontend =
    new SourceBasedFrontend("kotlin", Languages.KOTLIN, "Kotlin Source Frontend", "kotlin")
  def python: SourceBasedFrontend =
    new SourceBasedFrontend("python", Languages.PYTHONSRC, "Python Source Frontend", "py")
  def golang: SourceBasedFrontend = new SourceBasedFrontend("golang", Languages.GOLANG, "Golang Source Frontend", "go")
  def javascript: SourceBasedFrontend =
    new SourceBasedFrontend("javascript", Languages.JAVASCRIPT, "Javascript Source Frontend", "js")
  def jssrc: SourceBasedFrontend =
    new SourceBasedFrontend("jssrc", Languages.JSSRC, "Javascript/Typescript Source Frontend based on astgen", "js")
  def swiftsrc: SourceBasedFrontend =
    new SourceBasedFrontend("swiftsrc", Languages.SWIFTSRC, "Swift Source Frontend based on swiftastgen", "swift")
  def csharp: Frontend          = new BinaryFrontend("csharp", Languages.CSHARP, "C# Source Frontend (Roslyn)")
  def llvm: Frontend            = new BinaryFrontend("llvm", Languages.LLVM, "LLVM Bitcode Frontend")
  def php: SourceBasedFrontend  = new SourceBasedFrontend("php", Languages.PHP, "PHP source frontend", "php")
  def ruby: SourceBasedFrontend = new RubyFrontend("Ruby source frontend", false)
  def rubyDeprecated: SourceBasedFrontend = new RubyFrontend("Ruby source deprecated frontend", true)

  private def allFrontends: List[Frontend] =
    List(
      c,
      cpp,
      ghidra,
      kotlin,
      java,
      jvm,
      javascript,
      jssrc,
      swiftsrc,
      golang,
      llvm,
      php,
      python,
      csharp,
      ruby,
      rubyDeprecated
    )

  // this is only abstract to force people adding frontends to make a decision whether the frontend consumes binaries or source
  abstract class Frontend(val name: String, val language: String, val description: String = "") {
    def cpgGeneratorForLanguage(
      language: String,
      config: FrontendConfig,
      rootPath: Path,
      args: List[String]
    ): Option[CpgGenerator] =
      io.joern.console.cpgcreation.cpgGeneratorForLanguage(language, config, rootPath, args)

    def isAvailable: Boolean =
      cpgGeneratorForLanguage(language, config.frontend, config.install.rootPath.path, args = Nil).exists(_.isAvailable)

    def apply(inputPath: String, projectName: String = "", args: List[String] = List()): Cpg = {
      val frontend = cpgGeneratorForLanguage(language, config.frontend, config.install.rootPath.path, args)
        .getOrElse(throw new ConsoleException(s"no cpg generator for language=$language available!"))
      new ImportCode(console)(frontend, inputPath, projectName)
    }
  }

  private class BinaryFrontend(name: String, language: String, description: String = "")
      extends Frontend(name, language, description)

  class SourceBasedFrontend(name: String, language: String, description: String, extension: String)
      extends Frontend(name, language, description) {

    def fromString(str: String, args: List[String] = List()): Cpg = {
      withCodeInTmpFile(str, "tmp." + extension) { dir =>
        super.apply(dir.path.toString, args = args)
      } match {
        case Failure(exception) => throw new ConsoleException(s"unable to generate cpg from given String", exception)
        case Success(value)     => value
      }
    }
  }

  /** Only a wrapper so as to more easily pick the deprecated variant without having to provide the
    * `--useDeprecatedFrontend` flag each time.
    *
    * @param useDeprecatedFrontend
    *   If set, will invoke the frontend with the `--useDeprecatedFrontend` flag
    */
  private class RubyFrontend(description: String, useDeprecatedFrontend: Boolean = false)
      extends SourceBasedFrontend("ruby", Languages.RUBYSRC, description, "rb") {
    private val deprecatedFlag = "--useDeprecatedFrontend"

    private def addDeprecatedFlagIfNeeded(args: List[String]): List[String] = {
      Option.when(useDeprecatedFrontend && !args.contains(deprecatedFlag))(deprecatedFlag).toList ++ args
    }

    override def cpgGeneratorForLanguage(
      language: String,
      config: FrontendConfig,
      rootPath: Path,
      args: List[String]
    ): Option[CpgGenerator] = {
      super.cpgGeneratorForLanguage(language, config, rootPath, addDeprecatedFlagIfNeeded(args))
    }
  }

  class CFrontend(name: String, extension: String = "c")
      extends SourceBasedFrontend(name, Languages.NEWC, "Eclipse CDT Based Frontend for C/C++", extension)

  private def withCodeInTmpFile(str: String, filename: String)(f: File => Cpg): Try[Cpg] = {
    val dir = File.newTemporaryDirectory("console")
    val result = Try {
      (dir / filename).write(str)
      f(dir)
    }
    dir.deleteOnExit(swallowIOExceptions = true)
    result
  }

  /** Provide an overview of the available CPG generators (frontends)
    */
  override def toString: String = {
    val cols = List("name", "description", "available")
    val rows = allFrontends.map { frontend =>
      List(frontend.name, frontend.description, frontend.isAvailable.toString)
    }
    "Type `importCode.<language>` to run a specific language frontend\n" +
      "\n" + Table(cols, rows).render
  }

  private def apply(generator: CpgGenerator, inputPath: String, projectName: String): Cpg = {
    checkInputPath(inputPath)

    val name = Option(projectName).filter(_.nonEmpty).getOrElse(deriveNameFromInputPath(inputPath, workspace))
    report(s"Creating project `$name` for code at `$inputPath`")

    val cpgMaybe = workspace.createProject(inputPath, name).flatMap { pathToProject =>
      val frontendCpgOutFile = pathToProject.resolve(nameOfLegacyCpgInProject)
      generatorFactory.runGenerator(generator, inputPath, frontendCpgOutFile.toString) match {
        case Success(_) =>
          console.open(name).flatMap(_.cpg)
        case Failure(exception) =>
          throw new ConsoleException(s"Error creating project for input path: `$inputPath`", exception)
      }
    }

    cpgMaybe
      .map { cpg =>
        report("""|Code successfully imported. You can now query it using `cpg`.
          |For an overview of all imported code, type `workspace`.""".stripMargin)
        console.applyDefaultOverlays(cpg)
        generator.applyPostProcessingPasses(cpg)
      }
      .getOrElse(throw new ConsoleException(s"Error creating project for input path: `$inputPath`"))
  }
}
