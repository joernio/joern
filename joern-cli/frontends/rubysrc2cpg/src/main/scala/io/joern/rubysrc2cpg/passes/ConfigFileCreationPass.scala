package io.joern.rubysrc2cpg.passes

import io.joern.x2cpg.passes.frontend.XConfigFileCreationPass
import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.utils.FileUtil

import java.nio.file.{Path, Paths}

import scala.util.Try

/** Creates the CONFIGURATION layer from any existing `Gemfile` or `Gemfile.lock` files found at root level.
  */
class ConfigFileCreationPass(cpg: Cpg) extends XConfigFileCreationPass(cpg) {

  private val validGemfilePaths = Try(Paths.get(cpg.metaData.root.headOption.getOrElse(""))).toOption match {
    case Some(rootPath) => Seq("Gemfile", "Gemfile.lock").map(rootPath / _)
    case None           => Seq()
  }

  override protected val configFileFilters: List[Path => Boolean] = List(
    // Gemfiles
    validGemfilePaths.contains,
    extensionFilter(".ini"),
    // YAML files
    extensionFilter(".yaml"),
    extensionFilter(".yml"),
    // XML files
    extensionFilter(".xml"),
    // ERB files
    extensionFilter(".erb")
  )
}
