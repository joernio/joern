package io.joern.scanners.ghidra

import io.joern.scanners.*
import io.joern.console.*
import io.joern.macros.QueryMacros.*
import io.shiftleft.semanticcpg.language.*

object DangerousFunctions extends QueryBundle {

  implicit val resolver: ICallResolver = NoResolve

  @q
  def strcpyUsed(): Query =
    Query.make(
      name = "call-to-strcpy-ghidra",
      author = Crew.suchakra,
      title = "Dangerous functions `strcpy` or `strncpy` used",
      description = """
        | Avoid `strcpy` or `strncpy` function. `strcpy` does not check buffer
        | lengths.
        | A possible mitigation could be `strncpy` which could prevent
        | buffer overflows but does not null-terminate strings leading to
        | memory corruption. A secure alternative (on BSD) is `strlcpy`.
        |""".stripMargin,
      score = 4,
      withStrRep({ cpg =>
        cpg.method("(?i)(strcpy|strncpy)").callIn
      }),
      tags = List(QueryTags.badfn)
    )
}
