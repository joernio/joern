package io.shiftleft.joern.console

import java.nio.file.Path

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.console.workspacehandling.{Project, ProjectFile}
import io.shiftleft.dataflowengineoss.semanticsloader.Semantics

class JoernProject(projectFile: ProjectFile,
                   path: Path,
                   cpg: Option[Cpg] = None,
                   var semantics: Semantics = Semantics.empty)
    extends Project(projectFile, path, cpg) {}
