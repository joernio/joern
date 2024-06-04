package io.joern.rubysrc2cpg.passes

import better.files.File
import io.joern.x2cpg.passes.frontend.XConfigFileCreationPass
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*

import scala.util.Try

/** Creates the CONFIGURATION layer from any existing `Gemfile` or `Gemfile.lock` files found at root level.
  */
class ConfigFileCreationPass(cpg: Cpg) extends XConfigFileCreationPass(cpg) {

  private val validGemfilePaths = Try(File(cpg.metaData.root.headOption.getOrElse(""))).toOption match {
    case Some(rootPath) => Seq("Gemfile", "Gemfile.lock").map(rootPath / _)
    case None           => Seq()
  }

  override protected val configFileFilters: List[File => Boolean] = List(validGemfilePaths.contains)
}
