package io.joern.console.workspacehandling

import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.Overlays
import io.shiftleft.semanticcpg.utils.FileUtil

import java.nio.file.{Path, Paths}

object Project {
  val workCpgFileName       = "cpg.bin.tmp"
  val persistentCpgFileName = "cpg.bin"
}

case class ProjectFile(inputPath: String, name: String)

/** @param path
  *   absolute path to directory holding the project
  * @param cpg
  *   reference to loaded CPG or None, if the CPG is not loaded
  */
case class Project(projectFile: ProjectFile, var path: Path, var cpg: Option[Cpg] = None) {

  import Project._

  def name: String = projectFile.name

  def inputPath: String = projectFile.inputPath

  def isOpen: Boolean = cpg.isDefined

  def appliedOverlays: Seq[String] = {
    cpg.map(Overlays.appliedOverlays).getOrElse(Nil)
  }

  def availableOverlays: List[String] = {
    path.resolve("overlays").listFiles().map(_.fileName).toList
  }

  def overlayDirs: Seq[Path] = {
    val overlayDir = path.resolve("overlays")
    appliedOverlays.map(o => overlayDir / o)
  }

  override def toString: String =
    toTableRow.mkString("\t")

  def toTableRow: List[String] = {
    val cpgLoaded = cpg.isDefined
    val overlays  = availableOverlays.mkString(",")
    val inputPath = projectFile.inputPath
    List(name, overlays, inputPath, cpgLoaded.toString)
  }

  /** Close project if it is open and do nothing otherwise.
    */
  def close: Project = {
    cpg.foreach { c =>
      c.close()
      System.err.println(s"closing/saving project `$name`")
      val workingCopy = path.resolve(workCpgFileName)
      val persistent  = path.resolve(persistentCpgFileName)
      FileUtil.copyFiles(workingCopy, persistent)
    }
    cpg = None
    this
  }

}
