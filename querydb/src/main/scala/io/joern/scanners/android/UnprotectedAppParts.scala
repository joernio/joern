package io.joern.scanners.android

import io.joern.scanners._
import io.joern.console._
import io.joern.dataflowengineoss.language.toExtendedCfgNode
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.joern.macros.QueryMacros._
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.Traversal

object UnprotectedAppParts extends QueryBundle {
  implicit val engineContext: EngineContext = EngineContext(Semantics.empty)
  implicit val resolver: ICallResolver      = NoResolve

  @q
  def intentRedirection(): Query =
    Query.make(
      name = "intent-redirection",
      author = Crew.claudiu,
      title = "Intent redirected without validation",
      description = "-",
      score = 4,
      withStrRep({ cpg =>
        cpg.method
          .nameExact("getParcelableExtra")
          .callIn
          .code(".*Intent.*")
          .filter { c =>
            def startActivityCalls = cpg.method.nameExact("startActivity").callIn
            def sink               = startActivityCalls.whereNot(_.controlledBy.astParent.isControlStructure).argument
            sink.reachableByFlows(c).nonEmpty
          }
          .l
          .iterator
      }),
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
