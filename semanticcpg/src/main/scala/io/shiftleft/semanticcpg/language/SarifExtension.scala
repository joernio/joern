package io.shiftleft.semanticcpg.language

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.help.Doc
import io.shiftleft.codepropertygraph.generated.nodes.Finding
import io.shiftleft.semanticcpg.sarif.SarifConfig.SarifVersion
import io.shiftleft.semanticcpg.sarif.SarifSchema.{Sarif, Sarif2_1_0}
import io.shiftleft.semanticcpg.sarif.{SarifConfig, SarifSchema, v2_1_0}
import org.json4s.Formats
import org.json4s.native.Serialization.{write, writePretty}

import java.net.URI

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

    def generateSarif(results: List[SarifSchema.Result] = Nil, baseUri: Option[URI] = None): Sarif = {
      config.sarifVersion match {
        case SarifVersion.V2_1_0 =>
          Sarif2_1_0(runs =
            v2_1_0.Schema.Run(
              tool = v2_1_0.Schema.Tool(driver =
                v2_1_0.Schema.ToolComponent(
                  name = config.toolName,
                  fullName = config.toolFullName,
                  organization = config.organization,
                  semanticVersion = config.semanticVersion,
                  informationUri = config.toolInformationUri
                )
              ),
              originalUriBaseId = baseUri,
              results = results
            ) :: Nil
          )
      }
    }

    traversal.l match {
      case Nil => generateSarif(Nil)
      case findings @ head :: _ =>
        val baseUri = Cpg(head.graph).metaData.root.headOption.map(java.io.File(_).toURI)
        generateSarif(findings.map(config.resultConverter.convertFindingToResult), baseUri)
    }

  }

  @Doc(info = "execute this traversal and convert findings to SARIF format as JSON")
  def toSarifJson(pretty: Boolean = false)(implicit config: SarifConfig = SarifConfig()): String = {
    implicit val formats: Formats = org.json4s.DefaultFormats ++ config.customSerializers

    val results = toSarif
    if (pretty) writePretty(results)
    else write(results)
  }

}
