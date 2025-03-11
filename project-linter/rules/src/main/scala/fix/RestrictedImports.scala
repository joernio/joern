package fix

import scalafix.v1._

import scala.meta._

trait RestrictedImports { this: SemanticRule =>

  val ruleName: String
  val ruleMessage: String
  val restrictedImports: Seq[String]

  override def isLinter = true

  override def fix(implicit doc: SemanticDocument): Patch = {
    val patches = doc.tree.collect {
      case imp: Import if isNotAllowed(imp) =>
        val diag = Diagnostic(id = ruleName, message = ruleMessage, position = imp.pos)
        Patch.lint(diag)
    }
    Patch.fromIterable(patches)
  }

  private def isNotAllowed(imp: Import): Boolean = restrictedImports.exists(_.startsWith(imp.toString()))

}
