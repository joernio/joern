package io.joern.console

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class QueryJsonSerializationTests extends AnyWordSpec with should.Matchers {

  "QueryJsonSerialization" should {
    "serialize queries to json" in {
      val query = Query(
        name = "a-name",
        author = "an-author",
        title = "a-title",
        description = "a-description",
        score = 2.5,
        traversal = { cpg =>
          cpg.method
        },
        traversalAsString = "cpg.method",
        tags = List("tag1", "tag2"),
        language = "c",
        codeExamples = CodeExamples(List("pos"), List("neg")),
        multiFileCodeExamples = MultiFileCodeExamples(List(List(CodeSnippet("content", "file.c"))), List())
      )
      // the `traversal` function cannot be serialized and is rendered as an empty object
      QueryJsonSerialization.write(List(query)) shouldBe
        """[{"name":"a-name","author":"an-author","title":"a-title","description":"a-description","score":2.5,"traversal":{},"traversalAsString":"cpg.method","tags":["tag1","tag2"],"language":"c","codeExamples":{"positive":["pos"],"negative":["neg"]},"multiFileCodeExamples":{"positive":[[{"content":"content","filename":"file.c"}]],"negative":[]}}]"""
    }
  }

}
