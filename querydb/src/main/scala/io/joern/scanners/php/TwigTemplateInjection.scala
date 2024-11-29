package io.joern.scanners.php

import io.joern.console.*
import io.joern.dataflowengineoss.language.*
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.macros.QueryMacros.*
import io.joern.scanners.*
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*

object TwigTemplateInjection extends QueryBundle {

  implicit val resolver: ICallResolver = NoResolve

  @q
  def TwigTemplateInjection()(implicit context: EngineContext): Query =
    Query.make(
      name = "twig-template-injection",
      author = Crew.SJ1iu,
      title = "Twig-Template-Injection: A parameter controlled by the user is rendered within a Twig template.",
      description = """
          |An attacker controlled parameter is used in an twig template.
          |
          |This doesn't necessarily indicate a Twig template injection, but if the input is not sanitized and the escape settings are disabled in the application, it could potentially lead to a template injection vulnerability.
          |""".stripMargin,
      score = 5,
      withStrRep({ cpg =>

        def source =
          cpg.call.name(Operators.assignment).argument.code("(?i).*request.*").l

        def sink =
          cpg.call.name("createTemplate").l.filter(call => call.methodFullName.toLowerCase.contains("twig")).argument

        sink.reachableBy(source).l.iterator

      }),
      tags = List(QueryTags.remoteCodeExecution, QueryTags.default)
    )
}
