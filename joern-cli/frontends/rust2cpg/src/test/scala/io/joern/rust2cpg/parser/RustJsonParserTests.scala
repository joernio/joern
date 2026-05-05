package io.joern.rust2cpg.parser

import io.joern.rust2cpg.parser.RustNodeSyntax.SourceFile
import org.scalatest.Inside.inside
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.util.{Failure, Success}

class RustJsonParserTests extends AnyWordSpec with Matchers {

  "should parse a minimal JSON string" in {
    val json =
      """{
        |  "relativeFilePath": "src/main.rs",
        |  "fullFilePath": "/tmp/project/src/main.rs",
        |  "content": "fn main() {}",
        |  "loc": 1,
        |  "children": [
        |    {
        |      "nodeKind": "SOURCE_FILE",
        |      "range": { "startOffset": 0, "endOffset": 12, "startLine": 0, "startColumn": 0 },
        |      "children": []
        |    }
        |  ]
        |}""".stripMargin

    inside(RustJsonParser.readJsonString(json)) {
      case Success(result) => {
        result.filename shouldBe "src/main.rs"
        result.fullPath shouldBe "/tmp/project/src/main.rs"
        result.fileContent shouldBe "fn main() {}"
        result.loc shouldBe 1
        result.contentBytes shouldBe result.fileContent.getBytes("UTF-8")
        result.ast shouldBe a[SourceFile]
      }
    }
  }

  "should fail for malformed JSON" in {
    RustJsonParser.readJsonString("{ not valid json }") shouldBe a[Failure[?]]
  }

  "should fail for missing header field" in {
    val json =
      """{
        |  "fullFilePath": "/tmp/project/src/main.rs",
        |  "content": "",
        |  "loc": 1,
        |  "children": []
        |}""".stripMargin

    RustJsonParser.readJsonString(json) shouldBe a[Failure[?]]
  }

}
