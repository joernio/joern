package ammonite.foo

import dotty.tools.dotc.parsing.Parsers.Parser
import dotty.tools.dotc.parsing.Tokens

trait CompatibilityParser { self: Parser =>

  def compatibilityImportClause() =
    importClause(Tokens.IMPORT, mkImport())

}
