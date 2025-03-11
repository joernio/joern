package fix

import scalafix.v1._

class NoBetterFiles extends SemanticRule("NoBetterFiles") with RestrictedImports {

  override val ruleName: String = "no-better-files"
  override val ruleMessage: String = "Do not use the Better Files library, use `FileUtils` or `java.nio.file` instead."
  override val restrictedImports: Seq[String] = Seq("better.files")

}
