package io.joern.scanners.c

import io.joern.scanners.*
import io.joern.console.*
import io.shiftleft.semanticcpg.language.*
import io.joern.macros.QueryMacros.*

object Metrics extends QueryBundle {

  @q
  def tooManyParameters(n: Int = 4): Query =
    Query.make(
      name = "too-many-params",
      author = Crew.fabs,
      title = s"Number of parameters larger than $n",
      description = s"This query identifies functions with more than $n formal parameters",
      score = 1.0,
      withStrRep({ cpg =>
        cpg.method.internal.filter(_.parameter.size > n).nameNot("<global>")
      }),
      tags = List(QueryTags.metrics),
      codeExamples = CodeExamples(
        List("""
          |
          |int too_many_params(int a, int b, int c, int d, int e) {
          |
          |}
          |
          |""".stripMargin),
        List("""
          |
          |void good(int a, int b, int c, int d) {
          |
          |}
          |
          |""".stripMargin)
      )
    )

  @q
  def tooHighComplexity(n: Int = 4): Query =
    Query.make(
      name = "too-high-complexity",
      author = Crew.fabs,
      title = s"Cyclomatic complexity higher than $n",
      description = s"This query identifies functions with a cyclomatic complexity higher than $n",
      score = 1.0,
      withStrRep({ cpg =>
        cpg.method.internal.filter(_.controlStructure.size > n).nameNot("<global>")
      }),
      tags = List(QueryTags.metrics),
      codeExamples = CodeExamples(
        List("""
          |
          |int high_cyclomatic_complexity(int x) {
          |  while(true) {
          |    for(int i = 0; i < 10; i++) {
          |    }
          |    if(foo()) {}
          |  }
          |  if (x > 10) {
          |    for(int i = 0; i < 10; i++) {
          |
          |     }
          |  }
          |}
          |
          |""".stripMargin),
        List("""
          |
          |void good(int x, int y) {
          |    if (x > 0) {/* Stuff */ } else { /* Stuff */ }
          |    if (y > 0) {/* Stuff */ } else { /* Stuff */ }
          |}
          |
          |""".stripMargin)
      )
    )

  @q
  def tooLong(n: Int = 1000): Query =
    Query.make(
      name = "too-long",
      author = Crew.fabs,
      title = s"More than $n lines",
      description = s"This query identifies functions that are more than $n lines long",
      score = 1.0,
      withStrRep({ cpg =>
        cpg.method.internal.filter(_.numberOfLines > n).nameNot("<global>")
      }),
      tags = List(QueryTags.metrics),
      codeExamples = CodeExamples(
        List("""
          |
          |int func_with_many_lines(int x) {
          |  x++;
          |  x++;
          |  x++;
          |  x++;
          |  x++;
          |  x++;
          |  x++;
          |  x++;
          |  x++;
          |  x++;
          |  x++;
          |  x++;
          |}
          |
          |""".stripMargin),
        List("""
          |
          |int func_with_few_lines(int x) {
          |  x++;
          |}
          |
          |""".stripMargin)
      )
    )

  @q
  def multipleReturns(): Query =
    Query.make(
      name = "multiple-returns",
      author = Crew.fabs,
      title = s"Multiple returns",
      description = "This query identifies functions with more than one return",
      score = 1.0,
      withStrRep({ cpg =>
        cpg.method.internal.filter(_.ast.isReturn.l.size > 1).nameNot("<global>")
      }),
      tags = List(QueryTags.metrics),
      codeExamples = CodeExamples(
        List("""
          |
          |int func_with_multiple_returns (int x) {
          |  if (x > 10) {
          |    return 0;
          |  } else {
          |    return 1;
          |  }
          |}
          |
          |""".stripMargin),
        List("""
          |
          |int func_without_multiple_returns() {
          |  return 0;
          |}
          |
          |""".stripMargin)
      )
    )

  @q
  def tooManyLoops(n: Int = 4): Query =
    Query.make(
      name = "too-many-loops",
      author = Crew.fabs,
      title = s"More than $n loops",
      description = s"This query identifies functions with more than $n loops",
      score = 1.0,
      withStrRep({ cpg =>
        cpg.method.internal
          .filter(
            _.ast.isControlStructure
              .controlStructureType("(FOR|DO|WHILE)")
              .size > n
          )
          .nameNot("<global>")
      }),
      tags = List(QueryTags.metrics),
      codeExamples = CodeExamples(
        List("""
          |
          |int high_number_of_loops () {
          |  for(int i = 0; i < 10; i++){
          |  }
          |  int j = 0;
          |  do {
          |    j++
          |  } while(j < 10);
          |  while(foo()) {}
          |  while(bar()){}
          |}
          |""".stripMargin),
        List("""
          |int not_many_loops() {
          |  while (true) {
          |    // Do something
          |  }
          |}
          |""".stripMargin)
      )
    )

  @q
  def tooNested(n: Int = 3): Query =
    Query.make(
      name = "too-nested",
      author = Crew.fabs,
      title = s"Nesting level higher than $n",
      description = s"This query identifies functions with a nesting level higher than $n",
      score = 1.0,
      withStrRep({ cpg =>
        cpg.method.internal.filter(_.depth(_.isControlStructure) > n).nameNot("<global>")
      }),
      tags = List(QueryTags.metrics),
      codeExamples = CodeExamples(
        List("""
          |
          |int func_with_nesting_level_of_3(int foo, int bar) {
          |  if (foo > 10) {
          |    if (bar > foo) {
          |      for(int i = 0; i < bar ;i++) {
          |
          |      }
          |    }
          |  }
          |}
          |
          |""".stripMargin),
        List("""
          |
          |int func_with_nesting_level_of_1(int foo) {
          |  if (foo > 10) {
          |    // Do something
          |  }
          |}
          |
          |""".stripMargin)
      )
    )
}
