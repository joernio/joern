package io.joern.swiftsrc2cpg.utils

import com.google.gson.JsonObject
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.io.StringReader

class GsonUtilsTests extends AnyFunSuite with Matchers {

  private def collectNodes(json: String): Seq[JsonObject] =
    GsonUtils.collectJsonNodesWithProperty(new StringReader(json), "usrStr")

  test("finds single root level usrStr") {
    val json  = """{"usrStr":"hello"}"""
    val nodes = collectNodes(json)
    nodes should have size 1
    nodes.head.get("usrStr").getAsString shouldBe "hello"
  }

  test("finds multiple usrStr on root and nested objects") {
    val json =
      """
        {
          "usrStr": "root",
          "other": {"usrStr": "nested"},
          "array": [
            {"usrStr": "inArray"},
            {"foo": "bar"},
            [
              {"usrStr": "deepArray"}
            ]
          ]
        }
      """
    val nodes = collectNodes(json)
    nodes.map(_.get("usrStr").getAsString).toSet shouldBe Set("root", "nested", "inArray", "deepArray")
  }

  test("ignores objects without usrStr") {
    val json =
      """
        {
          "a": {"foo": "bar"},
          "b": [
            {"notUsrStr": "no"},
            {"usrStr": "yes"}
          ],
          "c": {}
        }
      """
    val nodes = collectNodes(json)
    nodes.map(_.get("usrStr").getAsString) should contain only "yes"
  }

  test("works with empty JSON object") {
    val json = "{}"
    collectNodes(json) shouldBe empty
  }

  test("works with empty array") {
    val json = "[]"
    collectNodes(json) shouldBe empty
  }

  test("works with nested arrays and objects") {
    val json =
      """
        [
          {"usrStr": "first"},
          [
            {"usrStr": "second"}
          ],
          {
            "inner": {
              "usrStr": "third",
              "another": {"usrStr": "fourth"}
            }
          }
        ]
      """
    val nodes = collectNodes(json)
    nodes.map(_.get("usrStr").getAsString).toSet shouldBe Set("first", "second", "third", "fourth")
  }

  test("handles objects with usrStr plus other keys") {
    val json =
      """
        {"usrStr":"val", "foo": "bar", "baz": {"usrStr": "nestedVal"}}
      """
    val nodes = collectNodes(json)
    nodes.map(_.get("usrStr").getAsString).toSet shouldBe Set("val", "nestedVal")
    nodes.exists(_.has("foo")) shouldBe true
  }

  test("does not fail on primitives") {
    val jsons = Seq("42", "\"string\"", "null", "true", "false")
    for (json <- jsons) {
      noException should be thrownBy collectNodes(json)
      collectNodes(json) shouldBe empty
    }
  }

  test("large nested structure") {
    val json =
      """
        {
          "level1": {
            "level2": {
              "usrStr": "deep",
              "level3": {
                "usrStr": "deeper"
              }
            }
          },
          "usrStr": "shallow"
        }
      """
    val nodes = collectNodes(json)
    nodes.map(_.get("usrStr").getAsString).toSet shouldBe Set("shallow", "deep", "deeper")
  }

  test("usrStr is not at first object property") {
    val json =
      """
        {
          "foo": "bar",
          "usrStr": "present"
        }
      """
    val nodes = collectNodes(json)
    nodes should have size 1
    nodes.head.get("usrStr").getAsString shouldBe "present"
  }

  test("usrStr property with different value types") {
    val json =
      """
        {
          "usrStr": 123,
          "array": [{"usrStr": true}, {"usrStr": null}]
        }
      """
    val nodes = collectNodes(json)
    nodes should have size 3
    nodes.map(_.get("usrStr")).map(n => n.isJsonPrimitive || n.isJsonNull) should contain only true
  }
}
