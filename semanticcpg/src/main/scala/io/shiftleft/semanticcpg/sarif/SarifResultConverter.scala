package io.shiftleft.semanticcpg.sarif

import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.sarif.SarifSchema.Result

/** A component that converts a CPG finding to some version of SARIF.
  */
trait SarifResultConverter {

  /** Given a finding, will convert it to the SARIF specified result.
    * @param finding
    *   the finding to convert.
    * @return
    *   a SARIF compliant result object.
    */
  def convertFindingToResult(finding: Finding): Result

}

/** Due to module dependencies, the following code is lifted from `io.joern.console.scan`.
  */
object SarifResultConverter {

  private object FindingKeys {
    val name        = "name"
    val title       = "title"
    val description = "description"
    val score       = "score"
  }

  implicit class FindingExtension(val node: Finding) extends AnyRef {

    def name: String = getValue(FindingKeys.name)

    def title: String = getValue(FindingKeys.title)

    def description: String = getValue(FindingKeys.description)

    def score: Double = getValue(FindingKeys.score).toDouble

    protected def getValue(key: String, default: String = ""): String =
      node.keyValuePairs.find(_.key == key).map(_.value).getOrElse(default)

  }
}
