package io.joern.scanners.java

import io.joern.scanners._
import io.shiftleft.semanticcpg.language._
import io.joern.console._
import io.shiftleft.console._
import io.shiftleft.macros.QueryMacros._

object DangerousFunctions extends QueryBundle {

  implicit val resolver: ICallResolver = NoResolve

  @q
  def execUsed(): Query =
    Query.make(
      name = "call-to-exec",
      author = Crew.niko,
      title = "Dangerous function 'java.lang.Runtime.exec:java.lang.Process(java.lang.String)' used",
      description = """
        | A call to the function `java.lang.Runtime.exec:java.lang.Process(java.lang.String)` 
        | could result in a potential remote code execution.
        |""".stripMargin,
      score = 8,
      withStrRep({ cpg =>
        cpg.method("java.lang.Runtime.exec").callIn
      }),
      tags = List(QueryTags.badfn, QueryTags.default)
    )
}
