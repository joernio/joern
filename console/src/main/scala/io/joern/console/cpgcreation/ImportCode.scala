package io.joern.console.cpgcreation

import better.files.File
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.joern.console.workspacehandling.Project
import overflowdb.traversal.help.Table

import scala.util.Try

class ImportCode[T <: Project](console: io.shiftleft.console.Console[T]) {
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

  def c: CFrontend = new CFrontend()
  def newc: CFrontend = new CFrontend(Languages.NEWC, "Eclipse CDT Based Frontend for C/C++")
  def llvm: Frontend = new Frontend(Languages.LLVM, "LLVM Bitcode Frontend")
  def java: Frontend = new Frontend(Languages.JAVA, "Java/Dalvik Bytecode Frontend")
  def javasrc: Frontend = new Frontend(Languages.JAVASRC, "Java Source Frontend")
  def golang: Frontend = new Frontend(Languages.GOLANG, "Golang Source Frontend")
  def javascript: Frontend = new Frontend(Languages.JAVASCRIPT, "Javascript Source Frontend")
  def csharp: Frontend = new Frontend(Languages.CSHARP, "C# Source Frontend (Roslyn)")
  def python: Frontend = new Frontend(Languages.PYTHON, "Python Source Frontend")
  def php: Frontend = new Frontend(Languages.PHP, "PHP bytecode frontend")
  def ghidra: Frontend = new Frontend(Languages.GHIDRA, "ghidra reverse engineering frontend")

  class Frontend(val language: String, val description: String = "") {
    def isAvailable: Boolean = {
      cpgGeneratorForLanguage(language, config.frontend, config.install.rootPath.path, args = Nil).get.isAvailable
    }

    def apply(inputPath: String,
              projectName: String = "",
              namespaces: List[String] = List(),
              args: List[String] = List()): Option[Cpg] = {
      val frontend = {
        cpgGeneratorForLanguage(language, config.frontend, config.install.rootPath.path, args)
      }
      new ImportCode(console)(frontend.get, inputPath, projectName, namespaces)
    }
  }

  class CFrontend(language: String = Languages.C, description: String = "Fuzzy Parser for C/C++")
      extends Frontend(language, description) {
    def fromString(str: String): Option[Cpg] = {
      withCodeInTmpFile(str, "tmp.c") { dir =>
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
    csharp,
    golang,
    java,
    javascript,
    llvm,
    python,
    ghidra,
  )

  /**
    * Provide an overview of the available CPG generators (frontends)
    * */
  override def toString: String = {
    val cols = List("name", "description", "available")
    val rows = allFrontends.map { frontend =>
      List(frontend.language.toLowerCase, frontend.description, frontend.isAvailable.toString)
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
