package io.joern.scanners.android

import io.joern.console.*
import io.joern.dataflowengineoss.language.toExtendedCfgNode
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.dataflowengineoss.semanticsloader.NoSemantics
import io.joern.macros.QueryMacros.*
import io.joern.scanners.*
import io.shiftleft.semanticcpg.language.*

object UnprotectedAppParts extends QueryBundle {
  implicit val engineContext: EngineContext = EngineContext(NoSemantics)
  implicit val resolver: ICallResolver      = NoResolve

  @q
  def intentRedirection(): Query =
    Query.make(
      name = "intent-redirection",
      author = Crew.claudiu,
      title = "Intent redirected without validation",
      description = "-",
      score = 4,
      withStrRep { cpg =>
        cpg.method
          .nameExact("getParcelableExtra")
          .callIn
          .code(".*Intent.*")
          .filter { c =>
            def startActivityCalls = cpg.method.nameExact("startActivity").callIn
            def sink               = startActivityCalls.whereNot(_.controlledBy.astParent.isControlStructure).argument
            sink.reachableByFlows(c).nonEmpty
          }
      },
      tags = List(QueryTags.android),
      codeExamples = CodeExamples(
        List("""
            |fun matchingExample1() {
            |    val forwardIntent = intent.getParcelableExtra<Intent>("very_forward_of_you")
            |    startActivity(forwardIntent)
            |}
            |""".stripMargin),
        List(
          """
            |fun nonMatchingExample1() {
            |    val forwardIntent = intent.getParcelableExtra<Intent>("very_forward_of_you")
            |    val destinationComponent = forwardIntent!!.resolveActivity(packageManager)
            |    if (destinationComponent.packageName.equals("org.emotet.botnet.safe") &&
            |        destinationComponent.className.equals("TotallySafeClass")) {
            |        startActivity(forwardIntent)
            |    }
            |}
            |""".stripMargin,
          """
            |fun nonMatchingExample2() {
            |    val forwardIntent = intent.getParcelableExtra<Intent>("very_forward_of_you")
            |    val destinationComponent = forwardIntent!!.resolveActivity(packageManager)
            |    if (destinationComponent.packageName.equals("org.emotet.botnet.safe") &&
            |        destinationComponent.className.equals("TotallySafeClass")) {
            |        startActivity(forwardIntent)
            |    }
            |}
            |""".stripMargin
        )
      )
    )
}
