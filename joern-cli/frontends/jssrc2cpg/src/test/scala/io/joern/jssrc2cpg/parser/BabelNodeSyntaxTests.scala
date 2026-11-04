package io.joern.jssrc2cpg.parser

import io.joern.jssrc2cpg.Config
import io.joern.jssrc2cpg.astcreation.AstCreator
import io.joern.jssrc2cpg.parser.BabelJsonParser.ParseResult
import io.joern.jssrc2cpg.parser.BabelNodeSyntax.*
import io.joern.x2cpg.ValidationMode
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class BabelNodeSyntaxTests extends AnyWordSpec with Matchers {

  "createBabelNode" should {

    "decode a node into its generated type" in {
      val node = createBabelNode(ujson.read("""{"type":"Identifier","name":"foo","start":0,"end":3}"""))
      node shouldBe a[Identifier]
      node.asInstanceOf[Identifier].name shouldBe "foo"
      node.nodeType shouldBe "Identifier"
      node.start shouldBe Some(0)
      node.end shouldBe Some(3)
    }

    "model optional fields and arrays" in {
      val node = createBabelNode(ujson.read("""{"type":"TemplateLiteral","expressions":[],"quasis":[
          |  {"type":"TemplateElement","value":{"raw":"a","cooked":"a"},"tail":true}
          |]}""".stripMargin))
      val template = node.asInstanceOf[TemplateLiteral]
      template.expressions shouldBe empty
      template.quasis.map(_.value("raw").str) shouldBe Seq("a")
      template.quasis.head.tail shouldBe true
    }

    "decode unknown node types to UnknownNode" in {
      val node = createBabelNode(ujson.read("""{"type":"NotABabelNode"}"""))
      node shouldBe a[UnknownNode]
      node.nodeType shouldBe "NotABabelNode"
    }
  }

  "BabelAstFile" should {

    "expose the program of a file" in {
      val envelope = BabelAstFile(ujson.read("""{"fullName":"/a/foo.js","relativeName":"foo.js",
            |"ast":{"type":"File","program":{"type":"Program","body":[]}}}""".stripMargin))
      envelope.relativeName shouldBe "foo.js"
      envelope.ast.program.body shouldBe empty
    }
  }

  "AstCreator" should {

    "turn unknown node types into UNKNOWN nodes instead of failing the file" in {
      val code = "a;  b;"
      val json = ujson.read("""{"type":"File","program":{"type":"Program","sourceType":"module","body":[
          |  {"type":"ExpressionStatement","start":0,"end":2,
          |   "expression":{"type":"Identifier","name":"a","start":0,"end":1}},
          |  {"type":"FromTheFuture","start":4,"end":6}
          |]}}""".stripMargin)
      val parseResult = ParseResult("test.js", "/test.js", createBabelNode(json).asInstanceOf[File], code, Map.empty, 1)
      val diff = new AstCreator(Config(), scala.collection.mutable.HashSet.empty, parseResult)(ValidationMode.Disabled)
        .createAst()
      val cpg = Cpg.empty
      flatgraph.DiffGraphApplier.applyDiff(cpg.graph, diff)

      cpg.identifier.name.l shouldBe List("a")
      cpg.all.collectAll[io.shiftleft.codepropertygraph.generated.nodes.Unknown].code.l shouldBe List("b;")
    }
  }
}
