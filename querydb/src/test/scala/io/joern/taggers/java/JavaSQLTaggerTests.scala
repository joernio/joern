package io.joern.taggers.java

import io.joern.scanners.java.JavaBuiltinsTagger
import io.joern.suites.JavaQueryTestSuite

class JavaSQLTaggerTests extends JavaQueryTestSuite(JavaBuiltinsTagger) {

  "the `javax-sql` queries" when {

    "a basic request can write directly to a SQL query" in {
      val cpg = code("""import java.io.IOException;
          |import java.sql.Connection;
          |import java.sql.DriverManager;
          |import java.sql.SQLException;
          |import javax.servlet.http.HttpServletRequest;
          |import javax.servlet.http.HttpServletResponse;
          |
          |public class Basic19 {
          |    private static final String FIELD_NAME = "name";
          |
          |    protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws IOException {
          |        String name = req.getParameter(FIELD_NAME);
          |
          |        Connection con = null;
          |        try {
          |            con = DriverManager.getConnection(MicroTestCase.CONNECTION_STRING);
          |            con.prepareStatement("select * from Users where name=" + name); /* BAD */
          |        } catch (SQLException e) {
          |            System.err.println("An error occurred");
          |        } finally {
          |            try {
          |                if(con != null) con.close();
          |            } catch (SQLException e) {
          |                e.printStackTrace();
          |            }
          |        }
          |    }
          |
          |}
          |""".stripMargin)

      val sources = findMatchingArgumentCalls(cpg, queryBundle.javaSqlSinks())
      sources shouldBe List("con.prepareStatement(\"select * from Users where name=\" + name)")
    }

  }

}
