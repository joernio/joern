package io.joern.console.workspacehandling

import io.joern.console
import io.joern.console.defaultAvailableWidthProvider
import io.joern.console.Reporting
import io.joern.x2cpg.utils.FileUtil
import io.joern.x2cpg.utils.FileUtil.*
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.cpgloading.CpgLoader
import org.json4s.DefaultFormats
import org.json4s.native.Serialization.write as jsonWrite

import java.net.URLEncoder
import java.nio.charset.Charset
import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

object DefaultLoader extends WorkspaceLoader[Project] {
  override def createProject(projectFile: ProjectFile, path: Path): Project = {
    Project(projectFile, path)
  }
}

/** WorkspaceManager: a component, which loads and maintains the list of projects made accessible via Ocular/Joern.
  *
  * @param path
  *   path to to workspace.
  */
class WorkspaceManager[ProjectType <: Project](path: String, loader: WorkspaceLoader[ProjectType] = DefaultLoader)
    extends Reporting {

  def getPath: String = path

  import WorkspaceManager._

  /** The workspace managed by this WorkspaceManager
    */
  private var workspace: Workspace[ProjectType] = loader.load(path)
  private val dirPath                           = Paths.get(path).toAbsolutePath

  private val LEGACY_BASE_CPG_FILENAME = "cpg.bin.zip"
  private val OVERLAY_DIR_NAME         = "overlays"

  /** Create project for code stored at `inputPath` with the project name `name`. If `name` is empty, the project name
    * is derived from `inputPath`. If a project for this `name` already exists, it is deleted from the workspace first.
    * If no file or directory exists at `inputPath`, then no project is created. Returns the path to the project
    * directory as an optional String, and None if there was an error.
    */
  def createProject(inputPath: String, projectName: String): Option[Path] = {
    Some(Paths.get(inputPath)).filter(Files.exists(_)).map { _ =>
      val pathToProject = projectNameToDir(projectName)

      if (project(projectName).isDefined) {
        System.err.println(s"Project with name $projectName already exists - overwriting")
        removeProject(projectName)
      }

      createProjectDirectory(inputPath, name = projectName)
      loader.loadProject(pathToProject).foreach(addProjectToProjectsList)
      pathToProject
    }
  }

  /** Remove project named `name` from disk
    * @param name
    *   name of the project
    */
  def removeProject(name: String): Unit = {
    closeProject(name)
    removeProjectFromList(name)
    FileUtil.delete(projectNameToDir(name))
  }

  /** Create new project directory containing project file.
    */
  private def createProjectDirectory(inputPath: String, name: String): Unit = {
    val dirPath = projectNameToDir(name)
    Files.createDirectories(dirPath)
    val absoluteInputPath = Paths.get(inputPath).absolutePathAsString
    Files.createDirectories(Paths.get(overlayDir(name)))
    val projectFile = ProjectFile(absoluteInputPath, name)
    writeProjectFile(projectFile, dirPath)
    (dirPath / "cpg.bin").createWithParentsIfNotExists()
  }

  /** Write the project's `project.json`, a JSON file that holds meta information.
    */
  private def writeProjectFile(projectFile: ProjectFile, dirPath: Path): Path = {
    // TODO proguard and json4s don't play along. We actually want to
    // serialize the case class ProjectFile here, but it comes out
    // empty. This code will be moved to `codepropertgraph` at
    // which point serialization should work.
    // val content = jsonWrite(projectFile)
    implicit val formats: DefaultFormats.type = DefaultFormats
    val PROJECTFILE_NAME                      = "project.json"
    val content     = jsonWrite(Map("inputPath" -> projectFile.inputPath, "name" -> projectFile.name))
    val projectPath = dirPath.resolve(PROJECTFILE_NAME)
    Files.writeString(projectPath, content)
    projectPath
  }

  /** Delete the workspace from disk, then initialize it again.
    */
  def reset: Unit = {
    Try(cpg.close())
    deleteWorkspace()
    workspace = loader.load(path)
  }

  private def deleteWorkspace(): Unit = {
    if (dirPath == null || dirPath.toString == "") {
      throw console.Error("dirPath is not set")
    }

    if (!Files.exists(dirPath.toAbsolutePath)) {
      throw console.Error(s"Directory ${dirPath.toString} does not exist")
    }

    FileUtil.delete(dirPath.toAbsolutePath)
  }

  /** Return the number of projects currently present in this workspace.
    */
  def numberOfProjects: Int = workspace.projects.size

  def projects: List[Project] = workspace.projects.toList

  def project(name: String): Option[Project] = workspace.projects.find(_.name == name)

  override def toString: String = workspace.toString

  private def getProjectByPath(projectPath: Path): Option[Project] =
    workspace.projects.find(_.path.toAbsolutePath == projectPath.toAbsolutePath)

  /** A sorted list of all loaded CPGs
    */
  def loadedCpgs: List[Cpg] = workspace.projects.flatMap(_.cpg).toList

  /** Indicates whether a workspace record exists for @inputPath.
    */
  def projectExists(inputPath: String): Boolean = projectDir(inputPath).toFile.exists

  /** Indicates whether a base CPG exists for @inputPath.
    */
  def cpgExists(inputPath: String, isLegacy: Boolean = false): Boolean = {
    val baseFileName = if (isLegacy) LEGACY_BASE_CPG_FILENAME else BASE_CPG_FILENAME
    projectExists(inputPath) &&
    projectDir(inputPath).resolve(baseFileName).toFile.exists
  }

  /** Overlay directory for CPG with given @inputPath
    */
  def overlayDir(inputPath: String): String = {
    projectDir(inputPath).resolve(OVERLAY_DIR_NAME).toString
  }

  def overlayDirByProjectName(name: String): String = {
    projectNameToDir(name).resolve(OVERLAY_DIR_NAME).toString
  }

  /** Filename for the base CPG for @inputPath
    */
  private def baseCpgFilename(inputPath: String, isLegacy: Boolean = false): String = {
    val baseFileName = if (isLegacy) LEGACY_BASE_CPG_FILENAME else BASE_CPG_FILENAME
    projectDir(inputPath).resolve(baseFileName).toString
  }

  /** The safe directory name for a given input file that can be used to store its base CPG, along with all overlays.
    * This method returns a directory name regardless of whether the directory exists or not.
    */
  private def projectDir(inputPath: String): Path = {
    val filename = Paths.get(inputPath).getFileName
    if (filename == null) {
      throw RuntimeException("invalid input path: " + inputPath)
    }
    dirPath
      .resolve(URLEncoder.encode(filename.toString, Charset.defaultCharset()))
      .toAbsolutePath
  }

  private def projectNameToDir(name: String): Path = {
    dirPath
      .resolve(URLEncoder.encode(name, Charset.defaultCharset()))
      .toAbsolutePath
  }

  /** Record for the given name. None if it is not in the workspace.
    */
  private def projectByName(name: String): Option[ProjectType] = {
    workspace.projects.find(r => r.name == name)
  }

  /** Workspace record for the CPG, or none, if the CPG is not in the workspace
    */
  def projectByCpg(baseCpg: Cpg): Option[ProjectType] =
    workspace.projects.find(_.cpg.contains(baseCpg))

  def projectExistsForCpg(baseCpg: Cpg): Boolean = projectByCpg(baseCpg).isDefined

  def getNextOverlayDirName(baseCpg: Cpg, overlayName: String): String = {
    val project          = projectByCpg(baseCpg).get
    val overlayDirectory = Paths.get(overlayDirByProjectName(project.name))

    val overlayFile = overlayDirectory
      .resolve(overlayName)

    overlayFile.absolutePathAsString
  }

  /** Obtain the cpg that was last loaded. Throws a runtime exception if no CPG has been loaded.
    */
  def cpg: Cpg = {
    workspace.projects.lastOption match {
      case Some(project) =>
        project.cpg.getOrElse(
          throw console.Error(s"No CPG loaded for project ${project.name} - try e.g. `help|importCode|importCpg|open`")
        )
      case None => throw console.Error("No projects loaded")
    }
  }

  /** Set active project to project with name `name`. If a project with this name does not exist, does nothing.
    */
  def setActiveProject(name: String): Option[ProjectType] = {
    val project = projectByName(name)
    if (project.isEmpty) {
      System.err.println(s"Error: project with name $name does not exist")
      None
    } else {
      removeProjectFromList(name).map { p =>
        addProjectToProjectsList(p)
        p
      }
    }
  }

  /** Retrieve the currently active project. If no project is active, None is returned.
    */
  def getActiveProject: Option[Project] = {
    workspace.projects.lastOption
  }

  /** Open project by name and return it. If a project with this name does not exist, None is returned. If the CPG of
    * this project is loaded, it is unloaded first and then reloaded. Returns project or None on error.
    *
    * @param name
    *   of the project to load
    * @param loader
    *   function to perform CPG loading. This parameter only exists for testing purposes.
    */
  def openProject(
    name: String,
    loader: String => Option[Cpg] = { x =>
      loadCpgRaw(x)
    }
  ): Option[Project] = {
    if (!projectExists(name)) {
      report(s"Project does not exist in workspace. Try `importCode/importCpg(inputPath)` to create it")
      None
    } else if (!Files.exists(Paths.get(baseCpgFilename(name)))) {
      report(s"CPG for project $name does not exist at ${baseCpgFilename(name)}, bailing out")
      None
    } else if (project(name).exists(_.cpg.isDefined)) {
      setActiveProject(name)
      project(name)
    } else {
      val cpgFilename = baseCpgFilename(name)
      report("Creating working copy of CPG to be safe")
      val cpgFile         = Paths.get(cpgFilename)
      val workingCopyPath = projectDir(name).resolve(Project.workCpgFileName)
      val workingCopyName = workingCopyPath.absolutePathAsString
      FileUtil.copyFiles(cpgFile, workingCopyPath)
      report(s"Loading base CPG from: $workingCopyName")

      val result = {
        val newCpg      = loader(workingCopyName)
        val projectPath = Paths.get(workingCopyName).getParent
        newCpg.flatMap { c =>
          unloadCpgIfExists(name)
          setCpgForProject(c, projectPath)
          projectByCpg(c)
        }
      }
      result
    }
  }

  /** Free up resources occupied by this project but do not remove project from disk.
    */
  def closeProject(name: String): Option[Project] =
    projectByName(name).map(_.close)

  /** Set CPG for existing project. It is assumed that the CPG is loaded.
    */
  private def setCpgForProject(newCpg: Cpg, projectPath: Path): Unit = {
    val project = getProjectByPath(projectPath)
    project match {
      case Some(p) =>
        p.cpg = Some(newCpg)
        setActiveProject(p.name)
      case None =>
        System.err.println(s"Error setting CPG for non-existing/unloaded project at $projectPath")
    }
  }

  private def loadCpgRaw(cpgFilename: String): Option[Cpg] = {
    Try {
      CpgLoader.load(cpgFilename)
    } match {
      case Success(v) => Some(v)
      case Failure(ex) =>
        System.err.println("Error loading CPG")
        ex.printStackTrace()
        None
    }
  }

  private def addProjectToProjectsList(project: ProjectType): ListBuffer[ProjectType] = {
    workspace.projects += project
  }

  def unloadCpgByProjectName(name: String): Unit = {
    projectByName(name).foreach { record =>
      record.cpg.foreach(_.close)
      record.cpg = None
    }
  }

  def reloadCpgByName(name: String, loadCpg: String => Option[Cpg]): Option[Cpg] = {
    projectByName(name).flatMap { record =>
      record.cpg = loadCpg(record.name)
      record.cpg
    }
  }

  private def unloadCpgIfExists(name: String): Unit = {
    projectByName(projectDir(name).getFileName.toString)
      .flatMap(_.cpg)
      .foreach { c =>
        try {
          c.close
        } catch {
          case _: IllegalStateException => // Store is already closed
        }
      }
  }

  /** Remove currently active project from workspace and delete all associated workspace files from disk.
    */
  def deleteCurrentProject(): Unit = {
    val project = projectByCpg(cpg)
    project match {
      case Some(p) => deleteProject(p)
      case None =>
        report(s"Project for active CPG does not exist")
    }
  }

  /** Remove project with name `name` from workspace and delete all associated workspace files from disk.
    * @param name
    *   the name of the project that should be removed
    */
  def deleteProject(name: String): Option[Unit] = {
    val project = projectByName(name)
    project match {
      case Some(p) =>
        deleteProject(p)
        Option[Unit](())
      case None =>
        report(s"Project with name $name does not exist")
        None
    }
  }

  private def deleteProject(project: Project): Unit = {
    removeProjectFromList(project.name)
    if (project.path.toString != "") {
      FileUtil.delete(project.path)
    }
  }

  private def removeProjectFromList(name: String): Option[ProjectType] = {
    workspace.projects.zipWithIndex
      .find { case (record, _) =>
        record.name == name
      }
      .map(_._2)
      .map { index =>
        workspace.projects.remove(index)
      }
  }

  // Kept for backward compatibility
  @deprecated("", "")
  def recordExists(inputPath: String): Boolean = projectExists(inputPath)

  @deprecated("", "")
  def baseCpgExists(inputPath: String, isLegacy: Boolean = false): Boolean =
    cpgExists(inputPath, isLegacy)

}

object WorkspaceManager {

  private val BASE_CPG_FILENAME = "cpg.bin"

  def overlayFilesForDir(dirName: String): List[Path] = {
    Paths
      .get(dirName)
      .listFiles()
      .filter(f => Files.isRegularFile(f) && f.getFileName.toString != BASE_CPG_FILENAME)
      .toList
      .sortBy(_.getFileName.toString)
  }

}
