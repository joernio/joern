package io.shiftleft.console.workspacehandling

import better.files.Dsl.mkdirs
import better.files.File
import org.json4s.DefaultFormats
import org.json4s.native.Serialization.{read => jsonRead}

import java.nio.file.Path
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

/**
  * This component loads a workspace from disk and creates
  * a corresponding `Workspace` object.
  * */
abstract class WorkspaceLoader[ProjectType <: Project] {

  /**
    * Initialize workspace from a directory
    * @param path path to the directory
    * */
  def load(path: String): Workspace[ProjectType] = {
    val dirFile = File(path)
    val dirPath = dirFile.path.toAbsolutePath

    if (!dirFile.exists) {
      println(s"creating workspace directory: ${dirFile.path.toString}")
      mkdirs(dirFile)
    }
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
        System.err.println(e)
        None
    }
  }

  def createProject(projectFile: ProjectFile, path: Path): ProjectType

  private val PROJECTFILE_NAME = "project.json"
  implicit val formats: DefaultFormats.type = DefaultFormats

  private def readProjectFile(projectDirName: Path): ProjectFile = {
    // TODO see `writeProjectFile`
    val content = File(projectDirName.resolve(PROJECTFILE_NAME)).contentAsString
    val map = jsonRead[Map[String, String]](content)
    ProjectFile(map("inputPath"), map("name"))
  }

}
