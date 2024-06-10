package io.joern.taggers.java

import io.joern.scanners.java.JavaxTagger
import io.joern.suites.JavaQueryTestSuite

class JavaxServletTaggerTests extends JavaQueryTestSuite(JavaxTagger) {

  "the `javax-servlet` queries" when {

    "a basic request can write directly to the response" in {
      val cpg = code("""import java.io.IOException;
                       |import java.io.PrintWriter;
                       |import javax.servlet.http.HttpServletRequest;
                       |import javax.servlet.http.HttpServletResponse;
                       |
                       |public class Basic1  {
                       |    protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws IOException {
                       |        String str = req.getParameter("name");
                       |        resp.sendRedirect(str);    /* BAD */
                       |    }
                       |
                       |}
          |""".stripMargin)

      val sources = findMatchingParameters(cpg, queryBundle.javaxSources())
      sources shouldBe List("req")

      val sinks = findMatchingArgumentCalls(cpg, queryBundle.javaxSinks())
      sinks shouldBe List("sendRedirect(str)")
    }

  }

}
