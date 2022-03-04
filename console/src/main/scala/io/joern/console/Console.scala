package io.joern.console

import better.files.Dsl._
import better.files.File
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.cpgloading.CpgLoader
import io.joern.console.cpgcreation.ImportCode
import io.joern.console.scripting.{AmmoniteExecutor, ScriptManager}
import io.joern.console.workspacehandling.{Project, WorkspaceLoader, WorkspaceManager}
import io.joern.x2cpg.layers.{Base, CallGraph, ControlFlow, TypeRelations}
import io.shiftleft.semanticcpg.Overlays
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.dotextension.ImageViewer
import io.shiftleft.semanticcpg.layers.{LayerCreator, LayerCreatorContext}
import overflowdb.traversal.help.Doc

import scala.sys.process.Process
import scala.util.control.NoStackTrace
import scala.util.{Failure, Success, Try}

class Console[T <: Project](
  executor: AmmoniteExecutor,
  loader: WorkspaceLoader[T],
  baseDir: File = File.currentWorkingDirectory
) extends ScriptManager(executor) {

  import Console._

  private val _config       = new ConsoleConfig()
  def config: ConsoleConfig = _config
  def console: Console[T]   = this

  protected var workspaceManager: WorkspaceManager[T] = _
  switchWorkspace(baseDir.path.resolve("workspace").toString)
  protected def workspacePathName: String = workspaceManager.getPath

  private val nameOfCpgInProject = "cpg.bin"

  implicit object ConsoleImageViewer extends ImageViewer {
    def view(imagePathStr: String): Try[String] = {
      // We need to copy the file as the original one is only temporary
      // and gets removed immediately after running this viewer instance asynchronously via .run().
      val tmpFile = File(imagePathStr).copyTo(File.newTemporaryFile(suffix = ".svg"), overwrite = true)
      tmpFile.deleteOnExit(swallowIOExceptions = true)
      Try {
        Process(Seq(config.tools.imageViewer, tmpFile.path.toAbsolutePath.toString)).run()
      } match {
        case Success(_) =>
          // We never handle the actual result anywhere.
          // Hence, we just pass a success message.
          Success(s"Running viewer for '$tmpFile' finished.")
        case Failure(exc) =>
          System.err.println("Executing image viewer failed. Is it installed? ")
          System.err.println(exc)
          Failure(exc)
      }
    }
  }

  @Doc(
    info = "Access to the workspace directory",
    longInfo = """
      |All auditing projects are stored in a workspace directory, and `workspace`
      |provides programmatic access to this directory. Entering `workspace` provides
      |a list of all projects, indicating which code the project makes accessible,
      |whether the project is open, and which analyzers have been run to produce it.
      |Multiple projects can be open at any given time, however, only one project
      |can be active. Queries and edit-operations are executed on the active project
      |only.
      |
      |Operations
      |
      |----------
      |
      |`workspace` provides low-level access to the workspace directory. In most cases,
      |it is a better idea to use higher-level operations such as `importCode`, `open`,
      |`close`, and `delete`, which make use of workspace operations internally.
      |
      |* workspace.open([name]): open project by name and make it the active project.
      | If `name` is omitted, the last project in the workspace list is opened. If
      | the project is already open, this has the same effect as `workspace.setActiveProject([name])`
      |
      |* workspace.close([name]): close project by name. Does not remove the project.
      |
      |* workspace.remove([name]): close and remove project by name.
      |
      |* workspace.reset: create a fresh workspace directory, deleting the current
      |workspace directory
      |
      |""",
    example = "workspace"
  )
  def workspace: WorkspaceManager[T] = workspaceManager

  @Doc(
    info = "Close current workspace and open a different one",
    longInfo = """ | By default, the workspace in $INSTALL_DIR/workspace is used.
      | This method allows specifying a different workspace directory
      | via the `pathName` parameter.
      | Before changing the workspace, the current workspace will be
      | closed, saving any unsaved changes.
      | If `pathName` points to a non-existing directory, then a new
      | workspace is first created.
      |"""
  )
  def switchWorkspace(pathName: String): Unit = {
    if (workspaceManager != null) {
      report("Saving current workspace before changing workspace")
      workspaceManager.projects.foreach { p =>
        p.close
      }
    }
    workspaceManager = new WorkspaceManager[T](pathName, loader)
  }

  @Doc(info = "Currently active project", example = "project")
  def project: T =
    workspace.projectByCpg(cpg).getOrElse(throw new RuntimeException("No active project"))

  @Doc(
    info = "CPG of the active project",
    longInfo = """
      |Upon importing code, a project is created that holds
      |an intermediate representation called `Code Property Graph`. This
      |graph is a composition of low-level program representations such
      |as abstract syntax trees and control flow graphs, but it can be arbitrarily
      |extended to hold any information relevant in your audit, information
      |about HTTP entry points, IO routines, information flows, or locations
      |of vulnerable code. Think of Ocular and Joern as a CPG editors.
      |
      |In practice, `cpg` is the root object of the query language, that is, all
      |query language constructs can be invoked starting from `cpg`. For example,
      |`cpg.method.l` lists all methods, while `cpg.finding.l` lists all findings
      |of potentially vulnerable code.
      |""",
    example = "cpg.method.l"
  )
  implicit def cpg: Cpg = workspace.cpg

  /** All cpgs loaded in the workspace
    */
  def cpgs: Iterator[Cpg] = {
    if (workspace.projects.lastOption.isEmpty) {
      Iterator()
    } else {
      val activeProjectName = project.name
      (workspace.projects.filter(_.cpg.isDefined).iterator.flatMap { project =>
        open(project.name)
        Some(project.cpg)
      } ++ Iterator({ open(activeProjectName); None })).flatten
    }
  }

  // Provide `.l` on iterators, specifically so
  // that `cpgs.flatMap($query).l` is possible
  implicit class ItExtend[X](it: Iterator[X]) {
    def l: List[X] = it.toList
  }

  @Doc(
    info = "Open project by name",
    longInfo = """
      |open([projectName])
      |
      |Opens the project named `name` and make it the active project.
      |If `name` is not provided, the active project is opened. If `name`
      |is a path, the project name is derived from and a deprecation
      |warning is printed.
      |
      |Upon completion of this operation, the CPG stored in this project
      |can be queried via `cpg`. Returns an optional reference to the
      |project, which is empty on error.
      |""",
    example = """open("projectName")"""
  )
  def open(name: String): Option[Project] = {
    val projectName = fixProjectNameAndComplainOnFix(name)
    workspace.openProject(projectName).map { project =>
      project
    }
  }

  @Doc(
    info = "Open project for input path",
    longInfo = """
      |openForInputPath([input-path])
      |
      |Opens the project of the CPG generated for the input path `input-path`.
      |
      |Upon completion of this operation, the CPG stored in this project
      |can be queried via `cpg`. Returns an optional reference to the
      |project, which is empty on error.
      |"""
  )
  def openForInputPath(inputPath: String): Option[Project] = {
    val absInputPath = File(inputPath).path.toAbsolutePath.toString
    workspace.projects
      .filter(x => x.inputPath == absInputPath)
      .map(_.name)
      .map(open)
      .headOption
      .flatten
  }

  /** Open the active project
    */
  def open: Option[Project] = {
    workspace.projects.lastOption.flatMap { p =>
      open(p.name)
    }
  }

  /** Delete project from disk and remove it from the workspace manager. Returns the (now invalid) project.
    * @param name
    *   the name of the project
    */
  @Doc(info = "Close and remove project from disk", example = "delete(projectName)")
  def delete(name: String): Option[Unit] = {
    workspaceManager.getActiveProject.foreach(_.cpg.foreach(_.close()))
    defaultProjectNameIfEmpty(name).flatMap(workspace.deleteProject)
  }

  /** Delete the active project
    */
  def delete: Option[Unit] = delete("")

  protected def defaultProjectNameIfEmpty(name: String): Option[String] = {
    if (name.isEmpty) {
      val projectNameOpt = workspace.projectByCpg(cpg).map(_.name)
      if (projectNameOpt.isEmpty) {
        report("Fatal: cannot find project for active CPG")
      }
      projectNameOpt
    } else {
      Some(fixProjectNameAndComplainOnFix(name))
    }
  }

  @Doc(
    info = "undo effects of analyzer",
    longInfo = """|Undo the last change, that is, unapply the last
       |overlay applied to the active project.
       |""",
    example = "undo"
  )
  def undo: File = {
    project.overlayDirs.lastOption
      .map { dir =>
        if (dir.isRegularFile) {
          System.err.println("Detected undo information in old format. That's ok.")
          CpgLoader.addDiffGraphs(List(dir.path.toString), cpg)
        } else {
          val zipFiles = dir.list.toList.sortWith(compareFiles).reverse.map(_.path.toString)
          CpgLoader.addDiffGraphs(zipFiles, cpg)
        }
        Overlays.removeLastOverlayName(cpg)
        dir.delete()
      }
      .getOrElse(throw new RuntimeException("No overlays present"))
  }

  private def compareFiles(a: File, b: File): Boolean = {
    val splitA = a.name.split("_")
    val splitB = b.name.split("_")
    if (splitA.length < 2 || splitB.length < 2)
      a.toString < b.toString
    else
      splitA(0).toInt < splitB(0).toInt
  }

  @Doc(
    info = "Write all changes to disk",
    longInfo = """
      |Close and reopen all loaded CPGs. This ensures
      |that changes have been flushed to disk.
      |
      |Returns list of affected projects
      |""",
    example = "save"
  )
  def save: List[Project] = {
    report("Saving graphs on disk. This may take a while.")
    workspace.projects.collect {
      case p: Project if p.cpg.isDefined =>
        p.close
        workspace.openProject(p.name)
    }.flatten
  }

  @Doc(
    info = "Create new project from code",
    longInfo = """
      |importCode(<inputPath>, [projectName], [namespaces], [language])
      |
      |Type `importCode` alone to get a list of all supported languages
      |
      |Import code at `inputPath`. Creates a new project, generates a CPG,
      |and opens the project. Upon success, the CPG can be queried via the `cpg`
      |object. Default overlays are already applied to the newly created CPG.
      |Returns new CPG and ensures that `cpg` now refers to this new CPG.
      |
      |By default, `importCode` attempts to guess the source language of
      |the code you provide. You can also specify the source language
      |manually, by running `importCode.<language>`. For example, `importCode.c`
      |runs the C/C++ frontend.
      |
      |Type `importCode` alone to get an overview of all available language modules.
      |
      |Parameters:
      |
      |-----------
      |
      |inputPath: location on disk of the code to analyze. e.g., a directory
      |containing source code or a Java archive (JAR).
      |
      |projectName: a unique name used for project management. If this parameter
      |is omitted, the name will be derived from `inputPath`
      |
      |namespaces: the whitelist of namespaces to analyse. Specifying this
      |parameter is only effective if the language frontend supports it.
      |If the list is omitted or empty, namespace selection is performed
      |automatically via heuristics.
      |
      |language: the programming language which the code at `inputPath` is written in.
      |If `language` is empty, the language used is guessed by inspecting
      |the filename found and possibly by looking into the file/directory.
      |
      |""",
    example = """importCode("example.jar")"""
  )
  def importCode = new ImportCode(this)

  @Doc(
    info = "Create new project from existing CPG",
    longInfo = """
      |importCpg(<inputPath>, [projectName])
      |
      |Import an existing CPG. The CPG is stored as part
      |of a new project and blanks are filled in by analyzing the CPG.
      |If we find that default overlays have not been applied, these
      |are applied to the CPG after loading it.
      |
      |Parameters:
      |
      |inputPath: path where the existing CPG (in overflowdb format)
      |is stored
      |
      |projectName: name of the new project. If this parameter
      |is omitted, the path is derived from `inputPath`
      |""",
    example = """importCpg("cpg.bin.zip")"""
  )
  def importCpg(inputPath: String, projectName: String = ""): Option[Cpg] = {
    val name =
      Option(projectName).filter(_.nonEmpty).getOrElse(deriveNameFromInputPath(inputPath, workspace))
    val cpgFile = File(inputPath)

    if (!cpgFile.exists) {
      report(s"CPG at $inputPath does not exist. Bailing out.")
      return None
    }

    System.err.println(s"Creating project `$name` for CPG at `$inputPath`")
    val pathToProject         = workspace.createProject(inputPath, name)
    val cpgDestinationPathOpt = pathToProject.map(_.resolve(nameOfCpgInProject))

    if (cpgDestinationPathOpt.isEmpty) {
      report(s"Error creating project for input path: `$inputPath`")
      return None
    }

    val cpgDestinationPath = cpgDestinationPathOpt.get

    if (CpgLoader.isLegacyCpg(cpgFile)) {
      report("You have provided a legacy proto CPG. Attempting conversion.")
      try {
        CpgConverter.convertProtoCpgToOverflowDb(cpgFile.path.toString, cpgDestinationPath.toString)
      } catch {
        case exc: Exception =>
          report("Error converting legacy CPG: " + exc.getMessage)
          return None
      }
    } else {
      cp(cpgFile, cpgDestinationPath)
    }

    val cpgOpt = open(name).flatMap(_.cpg)

    if (cpgOpt.isEmpty) {
      workspace.deleteProject(name)
    }

    cpgOpt
      .filter(_.metaData.hasNext)
      .foreach(applyDefaultOverlays)

    cpgOpt
  }

  @Doc(
    info = "Close project by name",
    longInfo = """|Close project. Resources are freed but the project remains on disk.
       |The project remains active, that is, calling `cpg` now raises an
       |exception. A different project can now be activated using `open`.
       |""",
    example = "close(projectName)"
  )
  def close(name: String): Option[Project] = {
    defaultProjectNameIfEmpty(name).flatMap(workspace.closeProject)
  }

  def close: Option[Project] = close("")

  /** Close the project and open it again.
    *
    * @param name
    *   the name of the project
    */
  def reload(name: String): Option[Project] = {
    close(name).flatMap(p => open(p.name))
  }

  def applyDefaultOverlays(cpg: Cpg): Cpg = {
    val appliedOverlays = io.shiftleft.semanticcpg.Overlays.appliedOverlays(cpg)
    if (appliedOverlays.isEmpty) {
      report("Adding default overlays to base CPG")
      val overlayCreators = List(new Base, new ControlFlow, new TypeRelations, new CallGraph)
      _runAnalyzer(overlayCreators: _*)
    }
    cpg
  }

  def _runAnalyzer(overlayCreators: LayerCreator*): Cpg = {

    overlayCreators.foreach { creator =>
      val overlayDirName =
        workspace.getNextOverlayDirName(cpg, creator.overlayName)

      val projectOpt = workspace.projectByCpg(cpg)
      if (projectOpt.isEmpty) {
        throw new RuntimeException("No record for CPG. Please use `importCode`/`importCpg/open`")
      }

      if (projectOpt.get.appliedOverlays.contains(creator.overlayName)) {
        report(s"Overlay ${creator.overlayName} already exists - skipping")
      } else {
        mkdirs(File(overlayDirName))
        runCreator(creator, Some(overlayDirName))
      }
    }
    report(
      "The graph has been modified. You may want to use the `save` command to persist changes to disk.  All changes will also be saved collectively on exit"
    )
    cpg
  }

  protected def runCreator(creator: LayerCreator, overlayDirName: Option[String]): Unit = {
    val context = new LayerCreatorContext(cpg, overlayDirName)
    creator.run(context, storeUndoInfo = true)
  }

  // We still tie the project name to the input path here
  // if no project name has been provided.

  def fixProjectNameAndComplainOnFix(name: String): String = {
    val projectName = Some(name)
      .filter(_.contains(java.io.File.separator))
      .map(x => deriveNameFromInputPath(x, workspace))
      .getOrElse(name)
    if (name != projectName) {
      System.err.println("Passing paths to `loadCpg` is deprecated, please use a project name")
    }
    projectName
  }

  def report(string: String): Unit = Console.report(string)

}

object Console {
  def report(string: String): Unit = System.err.println(string)
  val nameOfLegacyCpgInProject     = "cpg.bin.zip"

  def deriveNameFromInputPath[T <: Project](inputPath: String, workspace: WorkspaceManager[T]): String = {
    val name    = File(inputPath).name
    val project = workspace.project(name)
    if (project.isDefined && project.exists(_.inputPath != inputPath)) {
      var i = 1
      while (workspace.project(name + i).isDefined) {
        i += 1
      }
      name + i
    } else {
      name
    }
  }
}

class ConsoleException(message: String, cause: Option[Throwable])
    extends RuntimeException(message, cause.orNull)
    with NoStackTrace {
  def this(message: String) = this(message, None)
  def this(message: String, cause: Throwable) = this(message, Option(cause))
}
