package fix

import scalafix.v1._

import scala.meta._

object RestrictedImports {

  private val rules: Map[String, ImportRule] = Seq(
    ImportRule(
      "no-better-files",
      "Do not use the Better Files library, use `io.joern.x2cpg.utils.FileUtil` or `java.nio.file` instead.",
      "better.files"
    )
  ).map(x => x.path -> x).toMap

}

case class ImportRule(ruleName: String, ruleMessage: String, path: String)

class RestrictedImports extends SyntacticRule("RestrictedImports") {

  override def isLinter = true

  override def fix(implicit doc: SyntacticDocument): Patch = {
    val patches = doc.tree.collect { case imp: Import =>
      val importName = imp.importers.mkString
      RestrictedImports.rules
        .find { case (k, _) => importName.startsWith(k) }
        .map { case (_, ImportRule(ruleName, ruleMessage, path)) =>
          val diag = Diagnostic(id = ruleName, message = ruleMessage, position = imp.pos)
          Patch.lint(diag)
        }
        .getOrElse(Patch.empty)
    }
    Patch.fromIterable(patches)
  }

}
