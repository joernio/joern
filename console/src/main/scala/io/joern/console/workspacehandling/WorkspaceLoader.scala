package io.joern.console.workspacehandling

import flatgraph.help.Table.AvailableWidthProvider

import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

/** This component loads a workspace from disk and creates a corresponding `Workspace` object.
  */
abstract class WorkspaceLoader[ProjectType <: Project](implicit availableWidthProvider: AvailableWidthProvider) {

  /** Initialize workspace from a directory
    * @param path
    *   path to the directory
    */
  def load(path: String): Workspace[ProjectType] = {
    val dirFile = Paths.get(path)
    val dirPath = dirFile.toAbsolutePath

    Files.createDirectories(Paths.get(path))

    new Workspace(ListBuffer.from(loadProjectsFromFs(dirPath)))
  }

  private def loadProjectsFromFs(cpgsPath: Path): LazyList[ProjectType] = {
    cpgsPath.toFile.listFiles
      .filter(_.isDirectory)
      .to(LazyList)
      .flatMap(f => loadProject(f.toPath))
  }

  def loadProject(path: Path): Option[ProjectType] = {
    Try {
      val projectFile = readProjectFile(path)
      createProject(projectFile, path)
    } match {
      case Success(v) => Some(v)
      case Failure(e) =>
        System.err.println(s"Error loading project at $path - skipping: ")
        e.printStackTrace
        None
    }
  }

  def createProject(projectFile: ProjectFile, path: Path): ProjectType

  private val PROJECTFILE_NAME = "project.json"

  private def readProjectFile(projectDirName: Path): ProjectFile = {
    // TODO see `writeProjectFile`
    val data = ujson.read(projectDirName.resolve(PROJECTFILE_NAME))
    ProjectFile(data("inputPath").str, data("name").str)
  }

}
