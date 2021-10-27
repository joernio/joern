package io.shiftleft.console.workspacehandling

import java.nio.file.Path

object TestLoader {
  def apply(): TestLoader = {
    new TestLoader()
  }
}

class TestLoader extends WorkspaceLoader[Project] {
  override def createProject(projectFile: ProjectFile, path: Path): Project = {
    Project(projectFile, path)
  }
}
