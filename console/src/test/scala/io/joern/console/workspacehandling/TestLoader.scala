package io.joern.console.workspacehandling

import io.joern.console.testing.availableWidthProvider
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
