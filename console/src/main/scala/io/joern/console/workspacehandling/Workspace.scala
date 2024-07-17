package io.joern.console.workspacehandling

import io.joern.console.defaultAvailableWidthProvider
import flatgraph.help.Table
import flatgraph.help.Table.AvailableWidthProvider

import scala.collection.mutable.ListBuffer

/** Create a workspace from a list of projects. Workspace is a passive object that is managed by WorkspaceManager
  * @param projects
  *   list of projects present in this workspace
  */
class Workspace[ProjectType <: Project](var projects: ListBuffer[ProjectType])(implicit
  availableWidthProvider: AvailableWidthProvider = defaultAvailableWidthProvider
) {

  /** Returns total number of projects in this workspace
    */
  def numberOfProjects: Int = projects.size

  /** Provide a human-readable overview of the workspace
    */
  override def toString: String = {
    if (projects.isEmpty) {
      System.err.println("The workpace is empty. Use `importCode` or `importCpg` to populate it")
      "empty"
    } else {
      """
        |Overview of all projects present in your workspace. You can use `open` and `close`
        |to load and unload projects respectively. `cpgs` allows you to query all projects
        |at once. `cpg` points to the Code Property Graph of the *selected* project, which is
        |always the last project in the list. You can select a project by calling `open(name)`
        |on it, even if it is already open.
        |
        | Type `run` to add additional overlays to code property graphs
        |""".stripMargin
      "\n" + Table(
        columnNames = List("name", "overlays", "inputPath", "open"),
        rows = projects.map(_.toTableRow).toList
      ).render
    }
  }

}
