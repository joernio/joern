package io.joern.scanners.c

import io.joern.scanners.*
import io.joern.console.*
import io.joern.macros.QueryMacros.*
import io.shiftleft.semanticcpg.language.*

object CopyLoops extends QueryBundle {

  @q
  def isCopyLoop(): Query =
    Query.make(
      name = "copy-loop",
      author = Crew.fabs,
      title = "Copy loop detected",
      description = """
        |For (buf, indices) pairs, determine those inside control structures (for, while, if ...)
        |where any of the calls made outside of the body (block) are Inc operations. Determine
        |the first argument of that Inc operation and check if they are used as indices for
        |the write operation into the buffer.
        |""".stripMargin,
      score = 2,
      withStrRep({ cpg =>
        cpg.assignment.target.arrayAccess
          .map { access =>
            (access.array, access.subscript.code.toSet)
          }
          .filter { case (buf, subscripts) =>
            val incIdentifiers = buf.inAst.isControlStructure.isFor.astChildren
              .filterNot(_.isBlock)
              .assignment
              .target
              .code
              .toSet
            (incIdentifiers & subscripts).nonEmpty
          }
          .map(_._1)
      }),
      tags = List(QueryTags.default),
      codeExamples = CodeExamples(
        List("""
          |
          |int index_into_dst_array (char *dst, char *src, int offset) {
          |  for(i = 0; i < strlen(src); i++) {
          |    dst[i + + j*8 + offset] = src[i];
          |  }
          |}
          |
          |""".stripMargin),
        List("""
          |
          |// We do not want to detect this one because the
          |// index only specifies where to read from
          |int index_into_src_array() {
          |  for(i = 0; i < strlen(src); i++) {
          |    dst[k] = src[i];
          |  }
          |}
          |
          |""".stripMargin)
      )
    )
}
