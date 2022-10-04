package io.joern.jssrc2cpg.passes

import io.joern.jssrc2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.shiftleft.semanticcpg.language._
import io.joern.dataflowengineoss.language._

class RequirePassTests extends DataFlowCodeToCpgSuite {

  "methods imported via `require` should be resolved correctly" in {
    val cpg = code(
      """
         | const externalfunc = require('./sampleone')
         | function testone() {
         | var name = "foo";
         | console.log(name);
         | externalfunc(name);
         | }
         | testone();
      """.stripMargin,
      "sample.js"
    )

    cpg.moreCode(
      """
        | module.exports = function (nameparam){
        | console.log( "external func" + nameparam);
        | }
        |""".stripMargin,
      "sampleone.js"
    )

    cpg.call("externalfunc").methodFullName.l shouldBe List("sampleone.js::program:anonymous")
    implicit val callResolver: NoResolve.type = NoResolve
    cpg.call("externalfunc").callee.fullName.l shouldBe List("sampleone.js::program:anonymous")

    val sink   = cpg.call("log").argument(1)
    val source = cpg.literal.codeExact("\"foo\"")
    sink.reachableByFlows(source).size shouldBe 2
  }

}
