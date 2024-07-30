package io.joern.jssrc2cpg.preprocessing

import io.joern.jssrc2cpg.testfixtures.AstJsSrc2CpgSuite
import io.shiftleft.semanticcpg.language.*

class EjsPassTests extends AstJsSrc2CpgSuite {

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
      cpg.call.code.l.sorted shouldBe List("user.name")
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
      cpg.call.code.l.sorted shouldBe List("user.name")
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
      cpg.call.code.l.sorted shouldBe
        List(
          "console.log",
          "console.log(user)",
          "exampleWrite = 'some value'",
          "foo.callUnescaped",
          "foo.callUnescaped()",
          "foo.callWithWhitespaces",
          "foo.callWithWhitespaces()",
          "friend.name",
          "friend.name",
          "friend.name === selected",
          "friend.name === selected ? \"selected\" : \"\"",
          "friends.forEach",
          "friends.forEach(function(friend, index) { %>\n        <li class=\"<%= index === 0 ? \"first\" : \"\" %> <%= friend.name === selected ? \"selected\" : \"\" %>\"><%= friend.name %></li>\n    <% })",
          "index === 0",
          "index === 0 ? \"first\" : \"\"",
          "user.name"
        )
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
