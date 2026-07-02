package io.joern.jssrc2cpg.preprocessing

import io.joern.jssrc2cpg.testfixtures.JsSrc2CpgSuite
import io.shiftleft.semanticcpg.language.*

class EjsPassTests extends JsSrc2CpgSuite {

  "ejs files" should {

    "be renamed correctly" in {
      val cpg = code(
        """
        |<body>
        |<h1>Welcome <%= user.name %></h1>
        |</body>
        |""".stripMargin,
        "index.js.ejs"
      )
      cpg.file.name.l shouldBe List("index.js.ejs")
      cpg.call.name("escapeFn").argument(1).code.l shouldBe List("user.name")
    }

    "be ignored at folders excluded by default" in {
      val codeString = """
        |<body>
        |<h1>Welcome <%= user.name %></h1>
        |</body>
        |""".stripMargin
      val cpg = code(codeString, "index.js.ejs")
        .moreCode(codeString, "node_modules/foo.js.ejs")
        .moreCode(codeString, "vendor/bar.js.ejs")
        .moreCode(codeString, "www/baz.js.ejs")
      cpg.file.name.l shouldBe List("index.js.ejs")
      cpg.call.name("escapeFn").argument(1).code.l shouldBe List("user.name")
    }

    "be handled correctly" in {
      val cpg = code(
        """
        |<body>
        |
        |<h1>Welcome <%= user.name %></h1>
        |
        |<b><%_ foo.callWithWhitespaces() _%></b>
        |
        |<b><%- foo.callUnescaped() -%></b>
        |
        |<%# Just a comment here #%>
        |<% if (admin) { %>
        |    <a href="/admin">Admin</a>
        |<% } %>
        |
        |<ul>
        |    <% friends.forEach(function(friend, index) { %>
        |        <li class="<%= index === 0 ? "first" : "" %> <%= friend.name === selected ? "selected" : "" %>"><%= friend.name %></li>
        |    <% }); %>
        |</ul>
        |
        |<%
        |console.log(user);
        |exampleWrite = 'some value';
        |%>
        |
        |</body>
        |""".stripMargin,
        "index.ejs"
      )
      cpg.file.name.l shouldBe List("index.ejs")
      // unescaped output <%- %> -> __append wrapping the raw expression
      cpg.call.name("__append").argument(1).code.l shouldBe List("foo.callUnescaped()")
      // escaped output <%= %> -> escapeFn, one per output expression
      cpg.call.name("escapeFn").argument(1).code.sorted.l shouldBe List(
        "friend.name",
        "friend.name === selected ? \"selected\" : \"\"",
        "index === 0 ? \"first\" : \"\"",
        "user.name"
      )
      // the throwaway `ap` identifier never surfaces as a call name OR an identifier;
      // scriptlet/slurp blocks keep their real call names
      val callNames = cpg.call.name.toSet
      callNames should contain allOf ("callWithWhitespaces", "forEach", "log", "callUnescaped")
      callNames should not contain "ap"
      cpg.identifier.name.toSet should not contain "ap"
    }

    "invalid EJS file test" in {
      val cpg = code(
        """
        |<body>
        |<h1>Welcome <%@#$= user.name %></h1>
        |</body>
        |""".stripMargin,
        "index.js.ejs"
      )
      cpg.file.l.size shouldBe 0
    }
  }

}
