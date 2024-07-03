package io.joern.scanners.php

import io.joern.console.*
import io.joern.dataflowengineoss.language.*
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.macros.QueryMacros.*
import io.joern.scanners.*
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*

object SQLInjection extends QueryBundle {

  implicit val resolver: ICallResolver = NoResolve

  @q
  def shellExec()(implicit context: EngineContext): Query =
    Query.make(
      name = "php-sql-injection",
      author = Crew.niko,
      title = "SQL injection: A parameter is used in an insecure database API call.",
      description = """
          |An attacker controlled parameter is used in an insecure database API call.
          |
          |If the parameter is not validated and sanitized, this is a SQL injection.
          |""".stripMargin,
      score = 5,
      withStrRep({ cpg =>
        // $_REQUEST["foo"], $_GET["foo"], $_POST["foo"]
        // are identifier (at the moment)
        def source =
          cpg.call.name(Operators.assignment).argument.code(".*_(REQUEST|GET|POST).*")

        def sink = cpg.call.name("query").filter(_.receiver.nonEmpty).argument

        sink.reachableBy(source).l.iterator
      }),
      tags = List(QueryTags.remoteCodeExecution, QueryTags.default)
    )
}
