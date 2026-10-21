package io.joern.swiftsrc2cpg.utils

import io.joern.swiftsrc2cpg.utils.SwiftTypesProvider.TypeInfo
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.io.StringReader
import scala.collection.mutable

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

      val result = mutable.HashSet.empty[TypeInfo]
      GsonTypeInfoReader.collectTypeInfo(new StringReader(json), result.add)
      result should contain(
        TypeInfo("/tmp/F.swift", (10, 21), Some("Swift.Int"), Some("usr.fn"), Seq.empty, "call_expr")
      )
      result should contain(TypeInfo("/tmp/F.swift", (30, 41), Some("Swift.String"), None, Seq.empty, "return_stmt"))
      result should contain(TypeInfo("/tmp/F.swift", (50, 61), None, Some("usr.member"), Seq.empty, "member_ref_expr"))
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

      val result = mutable.HashSet.empty[TypeInfo]
      GsonTypeInfoReader.collectTypeInfo(new StringReader(json), result.add)

      result should contain(TypeInfo("/tmp/F.swift", (95, 121), Some("Swift.Void"), None, Seq.empty, "function_decl"))
      result should contain(TypeInfo("/tmp/F.swift", (95, 121), Some("Swift.Int"), None, Seq.empty, "parameter"))
      result should contain(TypeInfo("/tmp/F.swift", (1, 3), Some("Swift.C"), None, Seq("Swift.Base"), "class_decl"))
      result should contain(
        TypeInfo("/tmp/F.swift", (3, 5), Some("Swift.D"), None, Seq("Swift.Parent", "Swift.P"), "class_decl")
      )
      result.exists(_.filename.contains("/.build/")) shouldBe false
    }

    "handle deeply nested structures with multiple levels of child objects" in {
      val json =
        """[
          |  {
          |    "_kind": "call_expr",
          |    "filename": "/tmp/Nested.swift",
          |    "range": { "start": 200, "end": 250 },
          |    "type": "Swift.Result",
          |    "fn": {
          |      "_kind": "function_conversion_expr",
          |      "sub_expr": {
          |        "_kind": "declref_expr",
          |        "type": "Swift.Function",
          |        "decl": {
          |          "_kind": "decl_ref",
          |          "decl_usr": "usr.deeply.nested.function"
          |        }
          |      }
          |    }
          |  },
          |  {
          |    "_kind": "class_decl",
          |    "filename": "/tmp/Nested.swift",
          |    "range": { "start": 300, "end": 400 },
          |    "type": "MyModule.ComplexClass",
          |    "usr": "usr.MyModule.ComplexClass",
          |    "attrs": [
          |      { "_kind": "attribute", "range": { "start": 290, "end": 295 } },
          |      { "_kind": "attribute", "range": { "start": 285, "end": 289 } }
          |    ],
          |    "inherits": {
          |      "_kind": "inheritance_clause",
          |      "superclass_type": "MyModule.BaseClass",
          |      "conformances": [
          |        { "_kind": "type_expr", "protocol": "Swift.Codable" },
          |        { "_kind": "type_expr", "protocol": "Swift.Equatable" },
          |        { "_kind": "type_expr", "protocol": "MyModule.CustomProtocol" }
          |      ]
          |    }
          |  },
          |  {
          |    "_kind": "call_expr",
          |    "filename": "/tmp/Nested.swift",
          |    "range": { "start": 500, "end": 550 },
          |    "type": "Swift.Void",
          |    "fn": {
          |      "_kind": "declref_expr",
          |      "decl": {
          |        "_kind": "decl_ref",
          |        "decl_usr": "usr.method.chain"
          |      }
          |    },
          |    "irrelevant_field": {
          |      "_kind": "some_node",
          |      "deeply": {
          |        "nested": {
          |          "structure": {
          |            "that": {
          |              "should": {
          |                "be": {
          |                  "skipped": "because it's not in RelevantChildFieldNames"
          |                }
          |              }
          |            }
          |          }
          |        }
          |      }
          |    }
          |  }
          |]""".stripMargin

      val result = mutable.HashSet.empty[TypeInfo]
      GsonTypeInfoReader.collectTypeInfo(new StringReader(json), result.add)

      // Verify deeply nested call_expr with function_conversion_expr -> sub_expr -> decl chain
      result should contain(
        TypeInfo(
          "/tmp/Nested.swift",
          (200, 251),
          Some("Swift.Result"),
          Some("usr.deeply.nested.function"),
          Seq.empty,
          "call_expr"
        )
      )

      // Verify class with multiple attributes (uses first attribute's start) and multiple conformances
      result should contain(
        TypeInfo(
          "/tmp/Nested.swift",
          (290, 401),
          Some("MyModule.ComplexClass"),
          Some("usr.MyModule.ComplexClass"),
          Seq("MyModule.BaseClass", "Swift.Codable", "Swift.Equatable", "MyModule.CustomProtocol"),
          "class_decl"
        )
      )

      // Verify nested fn -> declref_expr -> decl chain (tests RelevantChildFieldNames filtering)
      result should contain(
        TypeInfo("/tmp/Nested.swift", (500, 551), Some("Swift.Void"), Some("usr.method.chain"), Seq.empty, "call_expr")
      )

      // Verify we extracted exactly 3 TypeInfo entries (irrelevant nested structure should be skipped)
      result.size shouldBe 3
    }
  }
}
