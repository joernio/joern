package io.joern.scanners.c

import io.joern.scanners.*
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.shiftleft.semanticcpg.language.*
import io.joern.dataflowengineoss.language.*
import io.joern.console.*
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.joern.macros.QueryMacros.*

object HeapBasedOverflow extends QueryBundle {

  implicit val engineContext: EngineContext = EngineContext(Semantics.empty)
  implicit val resolver: ICallResolver      = NoResolve

  /** Find calls to malloc where the first argument contains an arithmetic expression, the allocated buffer flows into
    * memcpy as the first argument, and the third argument of that memcpy is unequal to the first argument of malloc.
    * This is an adaption of the old-joern query first shown at 31C3 that found a buffer overflow in VLC's MP4 demuxer
    * (CVE-2014-9626).
    */
  @q
  def mallocMemcpyIntOverflow(): Query =
    Query.make(
      name = "malloc-memcpy-int-overflow",
      author = Crew.fabs,
      title = "Dangerous copy-operation into heap-allocated buffer",
      description = "-",
      score = 4,
      withStrRep({ cpg =>
        val src =
          cpg.method(".*malloc$").callIn.where(_.argument(1).arithmetic)

        cpg.method("(?i)memcpy").callIn.filter { memcpyCall =>
          memcpyCall
            .argument(1)
            .reachableBy(src)
            .where(_.inAssignment.target.codeExact(memcpyCall.argument(1).code))
            .whereNot(_.argument(1).codeExact(memcpyCall.argument(3).code))
            .hasNext
        }
      }),
      tags = List(QueryTags.integers, QueryTags.default),
      codeExamples = CodeExamples(
        List("""
          |
          |int vulnerable(size_t len, char *src) {
          |  char *dst = malloc(len + 8);
          |  memcpy(dst, src, len + 7);
          |}
          |
          |""".stripMargin),
        List("""
          |
          |int non_vulnerable(size_t len, char *src) {
          | char *dst = malloc(len + 8);
          | memcpy(dst, src,len + 8);
          |}
          |
          |int non_vulnerable2(size_t len, char *src) {
          | char *dst = malloc( some_size );
          | assert(dst);
          | memcpy(dst, src, some_size );
          |}
          |
          |""".stripMargin)
      )
    )
}
