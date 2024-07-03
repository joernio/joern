package io.joern.scanners.c

import io.joern.scanners.{Crew, QueryTags}
import io.joern.console.*
import io.joern.macros.QueryMacros.*
import io.shiftleft.semanticcpg.language.*
import QueryLangExtensions.*

object RetvalChecks extends QueryBundle {

  @q
  def uncheckedReadRecvMalloc(): Query =
    Query.make(
      name = "unchecked-read-recv-malloc",
      author = Crew.fabs,
      title = "Unchecked read/recv/malloc",
      description = """
      |The return value of a read/recv/malloc call is not checked directly and
      |the variable it has been assigned to (if any) does not
      |occur in any check within the caller.
      |""".stripMargin,
      score = 3.0,
      withStrRep({ cpg =>
        implicit val noResolve: NoResolve.type = NoResolve
        cpg
          .method("(?i)(read|recv|malloc)")
          .callIn
          .returnValueNotChecked
      }),
      tags = List(QueryTags.default),
      codeExamples = CodeExamples(
        List("""
          |
          |void unchecked_read() {
          |  read(fd, buf, sizeof(buf));
          |}
          |
          |void checks_something_else() {
          |  int nbytes = read(fd, buf, sizeof(buf));
          |  if( foo != sizeof(buf)) {
          |
          |  }
          |}
          |
          |""".stripMargin),
        List("""
          |
          |void checked_after_assignment() {
          |  int nbytes = read(fd, buf, sizeof(buf));
          |  if( nbytes != sizeof(buf)) {
          |
          |  }
          |}
          |
          |void immediately_checked() {
          |  if ( (read(fd, buf, sizeof(buf))) != sizeof(buf)) {
          |
          |  }
          |}
          |
          |int notCheckedButDirectlyReturned() {
          |  return read(fd, buf, sizeof(buf));
          |}
          |
          |""".stripMargin)
      )
    )
}
