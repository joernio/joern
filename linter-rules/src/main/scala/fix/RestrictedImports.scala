package fix

import scalafix.v1._

import scala.meta._

object RestrictedImports {

  /** If a file is within any of the packages defined below, it will not be checked for restricted imports.
    */
  private val ignoredPackages: Seq[String] = Seq()

  /** The rules for restricted imports.
    */
  private val rules: Map[String, ImportRule] = Seq(
    ImportRule(
      "BetterFiles",
      "Avoid the 'Better Files' library, use `io.shiftleft.semanticcpg.utils.FileUtil`, `os-lib` or `java.nio.file` instead.",
      "better.files"
    ),
    ImportRule(
      "ScalaSysProcess",
      "Do not manually create processes with `scala.sys.process`, use `io.shiftleft.semanticcpg.utils.ExternalCommand` instead.",
      "scala.sys.process"
    )
  ).map(x => x.path -> x).toMap

}

case class ImportRule(ruleName: String, ruleMessage: String, path: String)

/** A set of rules to detect undesired library usage.
  */
class RestrictedImports extends SyntacticRule("RestrictedImports") {

  override def isLinter = true

  override def fix(implicit doc: SyntacticDocument): Patch = {
    val pkgName = doc.tree
      .collect { case pkg: Pkg =>
        pkg.ref.toString()
      }
      .headOption
      .getOrElse("<no-package>")

    if (!RestrictedImports.ignoredPackages.exists(ignored => pkgName.startsWith(ignored))) {
      val patches = doc.tree.collect { case imp: Import =>
        val importName = imp.importers.mkString
        RestrictedImports.rules
          .find { case (importPath, _) => importName.startsWith(importPath) }
          .map { case (_, ImportRule(ruleName, ruleMessage, _)) =>
            val diag = Diagnostic(id = ruleName, message = ruleMessage, position = imp.pos)
            Patch.lint(diag)
          }
          .getOrElse(Patch.empty)
      }
      Patch.fromIterable(patches)
    } else {
      Patch.empty
    }
  }

}
