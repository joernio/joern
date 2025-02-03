package io.shiftleft.semanticcpg.sarif

import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.sarif.SarifSchema.{ReportingDescriptor, Result}

/** A component that converts a CPG finding to some version of SARIF.
  */
trait ScanResultToSarifConverter {

  /** Given a finding, will extract any rule data and create a SARIF ReportingDescriptor
    * @param finding
    *   the finding to convert.
    * @return
    *   a SARIF compliant reporting descriptor object if possible.
    */
  def convertFindingToReportingDescriptor(finding: Finding): Option[ReportingDescriptor]

  /** Given a finding, will convert it to the SARIF specified result.
    * @param finding
    *   the finding to convert.
    * @return
    *   a SARIF compliant result object.
    */
  def convertFindingToResult(finding: Finding): Result

}
