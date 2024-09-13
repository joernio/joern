package io.joern.joerncli.console

import io.joern.console.workspacehandling.{Project, ProjectFile}
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.dataflowengineoss.semanticsloader.NoSemantics
import io.shiftleft.codepropertygraph.generated.Cpg

import java.nio.file.Path

class JoernProject(
  projectFile: ProjectFile,
  path: Path,
  cpg: Option[Cpg] = None,
  var context: EngineContext = EngineContext(NoSemantics)
) extends Project(projectFile, path, cpg) {}
