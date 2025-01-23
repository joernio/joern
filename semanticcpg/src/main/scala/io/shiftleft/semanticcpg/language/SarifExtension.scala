package io.shiftleft.semanticcpg.language

import io.shiftleft.codepropertygraph.generated.help.Doc
import io.shiftleft.codepropertygraph.generated.nodes.Finding
import io.shiftleft.semanticcpg.sarif.SarifConfig
import io.shiftleft.semanticcpg.sarif.SarifConfig.SarifVersion
import io.shiftleft.semanticcpg.sarif.v2_1_0
import io.shiftleft.semanticcpg.sarif.SarifSchema.{Sarif, Sarif2_1_0}

/** Converts findings written to the CPG to the SARIF format.
  *
  * @param traversal
  *   the findings
  * @see
  *   https://docs.oasis-open.org/sarif/sarif/v2.1.0/sarif-v2.1.0.html
  */
class SarifExtension(val traversal: Iterator[Finding]) extends AnyVal {

  @Doc(info = "execute this traversal and convert findings to SARIF format")
  def toSarif(implicit config: SarifConfig = SarifConfig()): Sarif = {
    config.sarifVersion match {
      case SarifVersion.V2_1_0 =>
        Sarif2_1_0(runs =
          v2_1_0.Schema.Run(
            tool = v2_1_0.Schema.Tool(driver =
              v2_1_0.Schema.ToolComponent(
                name = config.toolName,
                organization = config.organization,
                semanticVersion = config.semanticVersion
              )
            ),
            results = traversal.map(config.resultConverter.convertFindingToResult).toList
          ) :: Nil
        )
    }
  }


  @Doc(info = "execute this traversal and convert findings to SARIF format as JSON")
  def toSarifJson(pretty: Boolean = false)(implicit config: SarifConfig = SarifConfig()): String = {
    // TODO: Implement JSON
    ???
  }

}
