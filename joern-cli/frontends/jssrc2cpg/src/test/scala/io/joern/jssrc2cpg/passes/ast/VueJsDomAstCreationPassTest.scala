package io.joern.jssrc2cpg.passes.ast

import io.joern.jssrc2cpg.passes.AbstractDomPassTest
import io.shiftleft.semanticcpg.language._

class VueJsDomAstCreationPassTest extends AbstractDomPassTest {

  "AST generation for vue.js DOM" should {

    "have correct structure for simple vue.js Single-File Component" in AstFixture(
      """
        |<template>
        |  <div id="app">
        |    <div id="nav">
        |      <router-link to="/">Home</router-link> |
        |      <router-link to="/about">About</router-link>
        |    </div>
        |    <router-view/>
        |  </div>
        |</template>
        |
        |<style>
        |#app {
        |  font-family: Avenir, Helvetica, Arial, sans-serif;
        |  -webkit-font-smoothing: antialiased;
        |  -moz-osx-font-smoothing: grayscale;
        |  text-align: center;
        |  color: #2c3e50;
        |}
        |
        |#nav {
        |  padding: 30px;
        |}
        |
        |#nav a {
        |  font-weight: bold;
        |  color: #2c3e50;
        |}
        |
        |#nav a.router-link-exact-active {
        |  color: #42b983;
        |}
        |
        |</style>
        |
        |""".stripMargin,
      "test.vue"
    ) { cpg =>
      cpg.file.name.l shouldBe List("test.vue")
      cpg.call.size shouldBe 0
      cpg.identifier.size shouldBe 0
      templateDomName(cpg) shouldBe Set(
        "JSXElement",
        "JSXOpeningElement",
        "JSXAttribute",
        "JSXClosingElement",
        "JSXText"
      )
      templateDomCode(cpg) shouldBe List(
        """<div id="app"> <div id="nav"> <router-link to="/">Home</router-link> | <router-link to="/about">About</router-link> </div> <router-view/> </div>""",
        """<div id="app">""",
        """id="app"""",
        "",
        """<div id="nav"> <router-link to="/">Home</router-link> | <router-link to="/about">About</router-link> </div>""",
        """<div id="nav">""",
        """id="nav"""",
        "",
        """<router-link to="/">Home</router-link>""",
        """<router-link to="/">""",
        """to="/"""",
        "Home",
        "</router-link>",
        "|",
        """<router-link to="/about">About</router-link>""",
        """<router-link to="/about">""",
        """to="/about"""",
        "About",
        "</router-link>",
        "",
        "</div>",
        "",
        "<router-view/>",
        "<router-view/>",
        "",
        "</div>"
      )
    }

    "have correct structure for full vue.js Single-File Component" in AstFixture(
      """
        |<template>
        |  <div class="hello">
        |    <h1>{{ msg }}</h1>
        |    <p>
        |      For a guide ...
        |    </p>
        |    <h3>Installed CLI Plugins</h3>
        |    <ul>
        |      <li><a href="link" target="_blank">babel</a></li>
        |    </ul>
        |    <h3>Essential Links</h3>
        |    <ul>
        |      <li><a href="link" target="_blank">Core Docs</a></li>
        |    </ul>
        |    <h3>Ecosystem</h3>
        |    <ul>
        |      <li><a href="link" target="_blank">vue-router</a></li>
        |    </ul>
        |  </div>
        |</template>
        |
        |<script lang="ts">
        |import { Component, Prop, Vue } from 'vue-property-decorator';
        |
        |@Component
        |export default class HelloWorld extends Vue {
        |  @Prop() private msg!: string;
        |}
        |</script>
        |
        |<!-- Add "scoped" attribute to limit CSS to this component only -->
        |<style scoped>
        |h3 {
        |  margin: 40px 0 0;
        |}
        |ul {
        |  list-style-type: none;
        |  padding: 0;
        |}
        |li {
        |  display: inline-block;
        |  margin: 0 10px;
        |}
        |a {
        |  color: #42b983;
        |}
        |</style>
        |
        |""".stripMargin,
      "test.vue"
    ) { cpg =>
      cpg.file.name.l shouldBe List("test.vue")
      cpg.call.code.l shouldBe List("exports[\"default\"]", "exports[\"default\"] = HelloWorld")

      inside(cpg.identifier.l) { case List(exports, msg, helloWorld) =>
        exports.name shouldBe "exports"
        exports.code shouldBe "exports"
        msg.name shouldBe "msg"
        msg.code shouldBe "msg"
        parentTemplateDom(msg).name shouldBe "JSXExpressionContainer"
        parentTemplateDom(msg).code shouldBe "{{ msg }}"
        parentTemplateDom(parentTemplateDom(msg)).name shouldBe "JSXElement"
        parentTemplateDom(parentTemplateDom(msg)).code shouldBe "<h1>{{ msg }}</h1>"
        helloWorld.name shouldBe "HelloWorld"
        helloWorld.code shouldBe "HelloWorld"
      }

      inside(cpg.staticImport.l) { case List(component, prop, vue) =>
        component.importedAs shouldBe Some("Component")
        component.importedEntity shouldBe Some("vue-property-decorator")
        component.code shouldBe "import { Component, Prop, Vue } from 'vue-property-decorator'"
        prop.importedAs shouldBe Some("Prop")
        prop.importedEntity shouldBe Some("vue-property-decorator")
        prop.code shouldBe "import { Component, Prop, Vue } from 'vue-property-decorator'"
        vue.importedAs shouldBe Some("Vue")
        vue.importedEntity shouldBe Some("vue-property-decorator")
        vue.code shouldBe "import { Component, Prop, Vue } from 'vue-property-decorator'"
      }
      inside(cpg.typeDecl("HelloWorld").l) { case List(helloWorld) =>
        helloWorld.code shouldBe "class HelloWorld"
        helloWorld.inheritsFromTypeFullName shouldBe Seq("Vue")
        helloWorld.member.name.l shouldBe List("msg")
        helloWorld.member.code.l shouldBe List("@Prop() private msg!: string;")
      }
      templateDomName(cpg) shouldBe Set(
        "JSXElement",
        "JSXExpressionContainer",
        "JSXOpeningElement",
        "JSXAttribute",
        "JSXClosingElement",
        "JSXText"
      )
      templateDomCode(cpg) shouldBe List(
        """<div class="hello"> <h1>{{ msg }}</h1> <p> For a guide ... </p> <h3>Installed CLI Plugins</h3> <ul> <li><a href="link" target="_blank">babel</a></li> </ul> <h3>Essential Links</h3> <ul> <li><a href="link" target="_blank">Core Docs</a></li> </ul> <h3>Ecosystem</h3> <ul> <li><a href="link" target="_blank">vue-router</a></li> </ul> </div>""",
        """<div class="hello">""",
        """class="hello"""",
        "",
        """<h1>{{ msg }}</h1>""",
        """<h1>""",
        """{{ msg }}""",
        """</h1>""",
        "",
        """<p> For a guide ... </p>""",
        """<p>""",
        """For a guide ...""",
        """</p>""",
        "",
        """<h3>Installed CLI Plugins</h3>""",
        """<h3>""",
        """Installed CLI Plugins""",
        """</h3>""",
        "",
        """<ul> <li><a href="link" target="_blank">babel</a></li> </ul>""",
        """<ul>""",
        "",
        """<li><a href="link" target="_blank">babel</a></li>""",
        """<li>""",
        """<a href="link" target="_blank">babel</a>""",
        """<a href="link" target="_blank">""",
        """href="link"""",
        """target="_blank"""",
        """babel""",
        """</a>""",
        """</li>""",
        "",
        """</ul>""",
        "",
        """<h3>Essential Links</h3>""",
        """<h3>""",
        """Essential Links""",
        """</h3>""",
        "",
        """<ul> <li><a href="link" target="_blank">Core Docs</a></li> </ul>""",
        """<ul>""",
        "",
        """<li><a href="link" target="_blank">Core Docs</a></li>""",
        """<li>""",
        """<a href="link" target="_blank">Core Docs</a>""",
        """<a href="link" target="_blank">""",
        """href="link"""",
        """target="_blank"""",
        """Core Docs""",
        """</a>""",
        """</li>""",
        "",
        """</ul>""",
        "",
        """<h3>Ecosystem</h3>""",
        """<h3>""",
        """Ecosystem""",
        """</h3>""",
        "",
        """<ul> <li><a href="link" target="_blank">vue-router</a></li> </ul>""",
        """<ul>""",
        "",
        """<li><a href="link" target="_blank">vue-router</a></li>""",
        """<li>""",
        """<a href="link" target="_blank">vue-router</a>""",
        """<a href="link" target="_blank">""",
        """href="link"""",
        """target="_blank"""",
        """vue-router""",
        """</a>""",
        """</li>""",
        "",
        """</ul>""",
        "",
        """</div>"""
      )
    }

  }

}
