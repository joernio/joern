package io.joern.scanners.java;

import io.joern.scanners.*
import io.shiftleft.semanticcpg.language.*
import io.joern.console.*
import io.joern.macros.QueryMacros.*
import io.joern.dataflowengineoss.language.*
import io.joern.dataflowengineoss.queryengine.EngineContext

object SpringExpressionLanguageInjection extends QueryBundle {

  implicit val resolver: ICallResolver = NoResolve

  @q
  def SpelInject()(implicit context: EngineContext): Query =
    Query.make(
      name = "Spring-Expression-Language-Injection",
      author = Crew.SJ1iu,
      title =
        "Spring-Expression-Language-Injection: The value is taken from user input and passed to ExpressionParser!!",
      description = """
        | In a SpEL injection, if user-controlled input is directly parsed and evaluated as a SpEL expression without validation, attackers can execute arbitrary expressions.
        |""".stripMargin,
      score = 8,
      withStrRep({ cpg =>

        def source =
          cpg.parameter.where(_.annotation.name("RequestParam")).where(_.name("expression"))

        def sink =
          cpg.call.name("parseExpression").argument.order(2).l

        sink.reachableBy(source).l

      }),
      tags = List(QueryTags.badfn, QueryTags.default),
      multiFileCodeExamples = MultiFileCodeExamples(
        positive = List(
          List(
            CodeSnippet(
              """
      |import org.springframework.expression.ExpressionParser;
      |import org.springframework.expression.spel.standard.SpelExpressionParser;
      |import org.springframework.web.bind.annotation.GetMapping;
      |import org.springframework.web.bind.annotation.RequestParam;
      |import org.springframework.web.bind.annotation.RestController;
      |@RestController
      |public class SpelInjectionController {
      |private final ExpressionParser parser = new SpelExpressionParser();

      |@GetMapping("/evaluate")
      |public String evaluateExpression(@RequestParam String expression) {
      |// This line is vulnerable to SpEL injection as it directly evaluates user input
      |Object result = parser.parseExpression(expression).getValue();
      |return "Evaluation result: " + result;
      |}
      |}
      |""".stripMargin,
              "Positive.kt"
            )
          )
        ),
        negative = List(
          List(
            CodeSnippet(
              """
      |import org.springframework.expression.ExpressionParser;
      |import org.springframework.expression.spel.standard.SpelExpressionParser;
      |import org.springframework.web.bind.annotation.GetMapping;
      |import org.springframework.web.bind.annotation.RequestParam;
      |import org.springframework.web.bind.annotation.RestController;
      |@RestController
      |public class SpelInjectionController {
      |private final ExpressionParser parser = new SpelExpressionParser();

      |@GetMapping("/evaluate")
      |public String evaluateExpression(@RequestParam String expression) {
      |return "NOT VULNERABLE";
      |}
      |}
      |""".stripMargin,
              "Negative.kt"
            )
          )
        )
      )
    )
}
