package io.joern.console.cpgcreation

import better.files.File
import io.joern.console.FrontendConfig
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.joern.console.workspacehandling.Project
import overflowdb.traversal.help.Table

import java.nio.file.Path
import scala.util.Try

class ImportCode[T <: Project](console: io.joern.console.Console[T]) {
  import io.joern.console.Console._

  private val config = console.config
  private val workspace = console.workspace
  protected val generatorFactory = new CpgGeneratorFactory(config)

  /**
    * This is the `importCode(...)` method exposed on the console. It attempts
    * to find a suitable CPG generator first by looking at the `language`
    * parameter and if no generator is found for the language, looking the
    * contents at `inputPath` to determine heuristically which generator to use.
    * */
  def apply(inputPath: String,
            projectName: String = "",
            namespaces: List[String] = List(),
            language: String = ""): Option[Cpg] = {

    if (language != "") {
      val generator = generatorFactory.forLanguage(language)
      if (generator.isEmpty) {
        report("No CPG generator exists for language: " + language)
        return None
      }
      generator.flatMap { frontend =>
        apply(frontend, inputPath, projectName, namespaces)
      }
    } else {
      val generator = generatorFactory.forCodeAt(inputPath)
      if (generator.isEmpty) {
        report("No suitable CPG generator found for: " + inputPath)
        return None
      }
      generator.flatMap { frontend =>
        apply(frontend, inputPath, projectName, namespaces)
      }
    }
  }

  def oldc: SourceBasedFrontend = new SourceBasedFrontend("oldc", Languages.C)
  def c: SourceBasedFrontend = new SourceBasedFrontend("c", Languages.NEWC, "Eclipse CDT Based Frontend for C/C++")
  def cpp: SourceBasedFrontend =
    new SourceBasedFrontend("cpp", Languages.NEWC, "Eclipse CDT Based Frontend for C/C++", "cpp")
  def java: SourceBasedFrontend = new SourceBasedFrontend("java", Languages.JAVASRC, "Java Source Frontend", "java")

  def jvm: Frontend = new Frontend("jvm", Languages.JAVA, "Java/Dalvik Bytecode Frontend (based on SOOT's jimple)")
  def ghidra: Frontend = new Frontend("ghidra", Languages.GHIDRA, "ghidra reverse engineering frontend")

  def python: Frontend = new Frontend("python", Languages.PYTHON, "Python Source Frontend")
  def golang: Frontend = new Frontend("golang", Languages.GOLANG, "Golang Source Frontend")
  def javascript: Frontend = new Frontend("javascript", Languages.JAVASCRIPT, "Javascript Source Frontend")
  def csharp: Frontend = new Frontend("csharp", Languages.CSHARP, "C# Source Frontend (Roslyn)")

  def llvm: Frontend = new Frontend("llvm", Languages.LLVM, "LLVM Bitcode Frontend")
  def php: Frontend = new Frontend("php", Languages.PHP, "PHP bytecode frontend")

  class Frontend(val name: String, val language: String, val description: String = "") {
    def cpgGeneratorForLanguage(language: String,
                                config: FrontendConfig,
                                rootPath: Path,
                                args: List[String]): Option[CpgGenerator] =
      io.joern.console.cpgcreation.cpgGeneratorForLanguage(language, config, rootPath, args)

    def isAvailable: Boolean = {
      cpgGeneratorForLanguage(language, config.frontend, config.install.rootPath.path, args = Nil)
        .filter(_.isAvailable)
        .isDefined
    }

    def apply(inputPath: String,
              projectName: String = "",
              namespaces: List[String] = List(),
              args: List[String] = List()): Option[Cpg] = {
      val frontend = cpgGeneratorForLanguage(language, config.frontend, config.install.rootPath.path, args)
        .getOrElse(throw new AssertionError(s"no cpg generator for language=$language available!"))
      new ImportCode(console)(frontend, inputPath, projectName, namespaces)
    }
  }

  class SourceBasedFrontend(name: String,
                            language: String = Languages.C,
                            description: String = "Fuzzy Parser for C/C++",
                            extension: String = "c")
      extends Frontend(name, language, description) {
    def fromString(str: String): Option[Cpg] = {
      withCodeInTmpFile(str, "tmp." + extension) { dir =>
        apply(dir.path.toString)
      }
    }
  }

  private def withCodeInTmpFile(str: String, filename: String)(f: File => Option[Cpg]): Option[Cpg] = {
    val dir = File.newTemporaryDirectory("console")
    val result = Try {
      (dir / filename).write(str)
      f(dir)
    }.toOption.flatten
    dir.delete()
    result
  }

  private def allFrontends: List[Frontend] = List(
    c,
    cpp,
    ghidra,
    java,
    jvm,
    javascript,
    golang,
    llvm,
    oldc,
    python,
    csharp,
  )

  /**
    * Provide an overview of the available CPG generators (frontends)
    * */
  override def toString: String = {
    val cols = List("name", "description", "available")
    val rows = allFrontends.map { frontend =>
      List(frontend.name, frontend.description, frontend.isAvailable.toString)
    }
    "Type `importCode.<language>` to run a specific language frontend\n" +
      "\n" + Table(cols, rows).render
  }

  private def apply(frontend: CpgGenerator,
                    inputPath: String,
                    projectName: String,
                    namespaces: List[String]): Option[Cpg] = {
    val name =
      Option(projectName).filter(_.nonEmpty).getOrElse(deriveNameFromInputPath(inputPath, workspace))
    report(s"Creating project `$name` for code at `$inputPath`")
    val pathToProject = workspace.createProject(inputPath, name)
    val frontendCpgOutFileOpt = pathToProject.map(_.resolve(nameOfLegacyCpgInProject))

    if (frontendCpgOutFileOpt.isEmpty) {
      report(s"Error creating project for input path: `$inputPath`")
    }

    val result = frontendCpgOutFileOpt.flatMap { frontendCpgOutFile =>
      Some(frontend).flatMap { frontend =>
        generatorFactory
          .runGenerator(
            frontend,
            inputPath,
            frontendCpgOutFile.toString,
            namespaces
          )
          .flatMap(_ => console.open(name).flatMap(_.cpg))
          .map { c =>
            console.applyDefaultOverlays(c)
          }
      }
    }
    if (result.isDefined) {
      report(
        """|Code successfully imported. You can now query it using `cpg`.
           |For an overview of all imported code, type `workspace`.""".stripMargin
      )
    }
    result
  }

}
