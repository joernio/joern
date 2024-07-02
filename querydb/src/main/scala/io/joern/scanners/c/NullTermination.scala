package io.joern.scanners.c

import io.joern.scanners.{Crew, QueryTags}
import io.shiftleft.semanticcpg.language.*
import io.joern.dataflowengineoss.language.*
import io.joern.console.*
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.joern.macros.QueryMacros.*

object NullTermination extends QueryBundle {

  implicit val engineContext: EngineContext = EngineContext(Semantics.empty)
  implicit val resolver: ICallResolver      = NoResolve

  @q
  def strncpyNoNullTerm(): Query =
    Query.make(
      name = "strncpy-no-null-term",
      author = Crew.fabs,
      title = "strncpy is used and no null termination is nearby",
      description = """
        | Upon calling `strncpy` with a source string that is larger
        | than the destination buffer, the destination buffer is not
        | null-terminated by `strncpy` and there is no explicit
        | null termination nearby. This is unproblematic if the
        | buffer size is at least 1 larger than the size passed
        | to `strncpy`.
        |""".stripMargin,
      score = 4,
      withStrRep({ cpg =>
        val allocations = cpg.method(".*malloc$").callIn.argument(1)
        cpg
          .method("(?i)strncpy")
          .callIn
          .map { c =>
            (c.method, c.argument(1), c.argument(3))
          }
          .filter { case (method, dst, size) =>
            dst.reachableBy(allocations).codeExact(size.code).nonEmpty &&
            method.assignment
              .where(_.target.arrayAccess.code(s"${dst.code}.*\\[.*"))
              .source
              .isLiteral
              .code(".*0.*")
              .isEmpty
          }
          .map(_._2)
      }),
      tags = List(QueryTags.strings, QueryTags.default),
      codeExamples = CodeExamples(
        List("""
          |
          |// If src points to a string that is at least `asize` long,
          |// then `ptr` will not be null-terminated after the `strncpy`
          |// call.
          |int bad() {
          |  char *ptr = malloc(asize);
          |  strncpy(ptr, src, asize);
          |}
          |
          |""".stripMargin),
        List("""
          |
          |// Null-termination is ensured if we can only copy
          |// less than `asize + 1` into the buffer
          |int good() {
          |  char *ptr = malloc(asize + 1);
          |  strncpy(ptr, src, asize);
          |}
          |
          | // Null-termination is also ensured if it is performed
          | // explicitly
          |int alsogood() {
          |  char *ptr = malloc(asize);
          |  strncpy(ptr, src, asize);
          |  ptr[asize -1] = '\0';
          |}
          |
          |""".stripMargin)
      )
    )
}
