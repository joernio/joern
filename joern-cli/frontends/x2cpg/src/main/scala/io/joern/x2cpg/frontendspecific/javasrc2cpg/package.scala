package io.joern.x2cpg.frontendspecific

import io.joern.x2cpg.passes.frontend.XTypeRecoveryConfig
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.CpgPassBase

import java.util.regex.Pattern
import scala.util.matching.Regex

package object javasrc2cpg {

  object ParameterNames {
    val EnableTypeRecovery = "enable-type-recovery"
  }

  def typeRecoveryPasses(cpg: Cpg, xtypeRecoveryConfig: XTypeRecoveryConfig): List[CpgPassBase] = {
    new JavaTypeRecoveryPassGenerator(cpg, xtypeRecoveryConfig).generate() :+
      new JavaTypeHintCallLinker(cpg)
  }

  /** Regexes matching common JVM build-tool / IDE / VCS folder names. Used to prune tree traversal so we don't descend
    * into caches, generated output, repo metadata, or test roots.
    */
  val JvmDefaultIgnoredFolders: List[Regex] =
    List(".git", ".mvn", ".gradle", "build", "target", "out", "node_modules", ".idea", "test")
      .map(Pattern.quote)
      .flatMap { name =>
        List(s"(^|\\\\)$name($$|\\\\)".r.unanchored, s"(^|/)$name($$|/)".r.unanchored)
      }
}
