package io.joern.swiftsrc2cpg.utils

import io.joern.swiftsrc2cpg.utils.SwiftTypesProvider.TypeInfo
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.io.StringReader

class GsonTypeInfoReaderTests extends AnyWordSpec with Matchers {

  "collectTypeInfo" should {
    "extract decl/type info for call, return, and member ref expressions" in {
      val json =
        """[
          |  {
          |    "_kind": "call_expr",
          |    "filename": "/tmp/F.swift",
          |    "range": { "start": 10, "end": 20 },
          |    "type": "Swift.Int",
          |    "fn": {
          |      "_kind": "declref_expr",
          |      "decl": {
          |        "_kind": "decl_ref",
          |        "decl_usr": "usr.fn"
          |      }
          |    }
          |  },
          |  {
          |    "_kind": "return_stmt",
          |    "filename": "/tmp/F.swift",
          |    "range": { "start": 30, "end": 40 },
          |    "result": {
          |      "_kind": "integer_literal_expr",
          |      "type": "Swift.String"
          |    }
          |  },
          |  {
          |    "_kind": "member_ref_expr",
          |    "filename": "/tmp/F.swift",
          |    "range": { "start": 50, "end": 60 },
          |    "decl": {
          |      "_kind": "decl_ref",
          |      "decl_usr": "usr.member"
          |    }
          |  }
          |]""".stripMargin

      val result = GsonTypeInfoReader.collectTypeInfo(new StringReader(json))
      // Expected ranges preserve existing behavior: end offsets are +1 for most node kinds.
      result should contain(TypeInfo("/tmp/F.swift", (10, 21), Some("Swift.Int"), Some("usr.fn"), Seq.empty, "call_expr"))
      result should contain(
        TypeInfo("/tmp/F.swift", (30, 41), Some("Swift.String"), None, Seq.empty, "return_stmt")
      )
      result should contain(
        TypeInfo("/tmp/F.swift", (50, 61), None, Some("usr.member"), Seq.empty, "member_ref_expr")
      )
    }

    "apply attrs-adjusted ranges for parameters and collect inheritances while filtering build folder files" in {
      val json =
        """[
          |  {
          |    "_kind": "function_decl",
          |    "filename": "/tmp/F.swift",
          |    "range": { "start": 100, "end": 120 },
          |    "attrs": [{ "_kind": "attribute", "range": { "start": 95, "end": 99 } }],
          |    "type": "Swift.Void"
          |  },
          |  {
          |    "_kind": "parameter",
          |    "filename": "/tmp/F.swift",
          |    "type": "Swift.Int"
          |  },
          |  {
          |    "_kind": "class_decl",
          |    "filename": "/tmp/F.swift",
          |    "range": { "start": 1, "end": 2 },
          |    "type": "Swift.C",
          |    "inherits": ["Swift.Base"]
          |  },
          |  {
          |    "_kind": "class_decl",
          |    "filename": "/tmp/F.swift",
          |    "range": { "start": 3, "end": 4 },
          |    "type": "Swift.D",
          |    "inherits": {
          |      "_kind": "inheritance_clause",
          |      "superclass_type": "Swift.Parent",
          |      "conformances": [{ "_kind": "type_expr", "protocol": "Swift.P" }]
          |    }
          |  },
          |  {
          |    "_kind": "class_decl",
          |    "filename": "/tmp/.build/generated.swift",
          |    "range": { "start": 5, "end": 6 },
          |    "type": "Swift.Skip"
          |  }
          |]""".stripMargin

      val result = GsonTypeInfoReader.collectTypeInfo(new StringReader(json))

      result should contain(TypeInfo("/tmp/F.swift", (95, 121), Some("Swift.Void"), None, Seq.empty, "function_decl"))
      result should contain(TypeInfo("/tmp/F.swift", (95, 121), Some("Swift.Int"), None, Seq.empty, "parameter"))
      result should contain(TypeInfo("/tmp/F.swift", (1, 3), Some("Swift.C"), None, Seq("Swift.Base"), "class_decl"))
      result should contain(
        TypeInfo("/tmp/F.swift", (3, 5), Some("Swift.D"), None, Seq("Swift.Parent", "Swift.P"), "class_decl")
      )
      result.exists(_.filename.contains("/.build/")) shouldBe false
    }
  }
}
