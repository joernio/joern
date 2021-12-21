package io.joern.console

import java.nio.file.Path

import io.shiftleft.codepropertygraph.Cpg
import io.joern.console.workspacehandling.{Project, ProjectFile}
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.dataflowengineoss.semanticsloader.Semantics

class JoernProject(
    projectFile: ProjectFile,
    path: Path,
    cpg: Option[Cpg] = None,
    var context: EngineContext = EngineContext(Semantics.empty)
) extends Project(projectFile, path, cpg) {}
