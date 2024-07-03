package io.joern.scanners.c

import io.joern.scanners.*
import io.shiftleft.codepropertygraph.generated.Operators
import io.joern.console.*
import io.joern.macros.QueryMacros.*
import io.shiftleft.semanticcpg.language.*

object SignedLeftShift extends QueryBundle {

  @q
  def signedLeftShift(): Query =
    Query.make(
      name = "signed-left-shift",
      author = Crew.malte,
      title = "Signed Shift May Cause Undefined Behavior",
      description = """
        |Signed integer overflow is undefined behavior. Shifts of signed values to the
        |left are very prone to overflow.
        |""".stripMargin,
      score = 2,
      withStrRep({ cpg =>
        cpg.call
          .nameExact(Operators.shiftLeft, Operators.assignmentShiftLeft)
          .where(_.argument(1).typ.fullNameExact("int", "long"))
          .filterNot(_.argument.isLiteral.size == 2)
      }),
      tags = List(QueryTags.integers, QueryTags.default),
      codeExamples = CodeExamples(
        List("""
          |
          |void bad1(int val) {
          |  val <<= 24;
          |}
          |
          |void bad2(int val) {
          |  255 << val;
          |}
          |
          |void bad3(int val) {
          |  val << val;
          |}
          |""".stripMargin),
        List("""
          |
          |void good(unsigned int val) {
          |  255 << 24; // we ignore signed shift with two literals
          |  val <<= 24;
          |  val << val;
          |}
          |
          |""".stripMargin)
      )
    )
}
