package io.joern.swiftsrc2cpg.utils

import com.google.gson.stream.JsonReader
import com.google.gson.{JsonArray, JsonObject, JsonParser}
import io.joern.swiftsrc2cpg.utils.SwiftTypesProvider.TypeInfo

import java.io.Reader
import scala.annotation.tailrec
import scala.collection.mutable

object GsonTypeInfoReader {

  private val FullnameFieldNames = Set("usr", "type", "result", "decl_usr")

  private def safePropertyValue(obj: JsonObject, propertyName: String): Option[String] = {
    Option.when(obj.has(propertyName) && obj.get(propertyName).isJsonPrimitive)(obj.get(propertyName).getAsString)
  }

  private def range(obj: JsonObject): (Int, Int) = {
    val rangeObj = obj.get("range").getAsJsonObject
    (rangeObj.get("start").getAsInt, rangeObj.get("end").getAsInt)
  }

  private def qualifies(obj: JsonObject): Boolean = {
    FullnameFieldNames.exists(n => safePropertyValue(obj, n).isDefined) && obj.has("range")
  }

  @tailrec
  private def declFromCallExpr(obj: JsonObject): JsonObject = {
    if (obj.has("fn") && obj.get("fn").isJsonObject) {
      val fn = obj.getAsJsonObject("fn")
      fn.get("_kind").getAsString match {
        // TODO: there are maybe more AST node kinds that need special handling
        case "declref_expr"                   => fn.getAsJsonObject("decl")
        case "function_conversion_expr"       => fn.getAsJsonObject("sub_expr").getAsJsonObject("decl")
        case other if other.endsWith("_expr") => declFromCallExpr(fn)
        case _                                => obj
      }
    } else {
      obj
    }
  }

  def collectTypeInfo(reader: Reader): Set[TypeInfo] = {
    val found      = mutable.HashSet.empty[TypeInfo]
    val jsonReader = new JsonReader(reader)
    var filename   = ""

    def parseElement(): Unit = {
      jsonReader.peek() match {
        case com.google.gson.stream.JsonToken.BEGIN_OBJECT => parseObject()
        case com.google.gson.stream.JsonToken.BEGIN_ARRAY  => parseArray()
        case _                                             => jsonReader.skipValue()
      }
    }

    def parseObject(): JsonObject = {
      val obj         = new JsonObject
      var hasProperty = false

      jsonReader.beginObject()
      while (jsonReader.hasNext) {
        val name = jsonReader.nextName()
        jsonReader.peek() match {
          case com.google.gson.stream.JsonToken.BEGIN_OBJECT if obj.has("_kind") =>
            val value = parseObject()
            obj.add(name, value)
          case com.google.gson.stream.JsonToken.BEGIN_OBJECT => // don't descend
          case com.google.gson.stream.JsonToken.BEGIN_ARRAY =>
            val value = parseArray()
            obj.add(name, value)
          case _ =>
            val value = JsonParser.parseReader(jsonReader)
            obj.add(name, value)
        }
        if (qualifies(obj)) hasProperty = true
        if (name == "filename") filename = obj.get("filename").getAsString
      }
      jsonReader.endObject()

      if (hasProperty) {
        val nodeKind = obj.get("_kind").getAsString
        val range_   = range(obj)

        lazy val declFullName = nodeKind match {
          case kind if kind.endsWith("call_expr") =>
            val fn = declFromCallExpr(obj)
            safePropertyValue(fn, "decl_usr")
          case _ =>
            safePropertyValue(obj, "decl_usr")
        }
        val typeName    = safePropertyValue(obj, "type").orElse(safePropertyValue(obj, "result"))
        val usrFullName = safePropertyValue(obj, "usr").orElse(declFullName)
        found.addOne(TypeInfo(filename, range_, typeName, usrFullName, nodeKind))
      }
      obj
    }

    def parseArray(): JsonArray = {
      val arr = new JsonArray
      jsonReader.beginArray()
      while (jsonReader.hasNext) {
        jsonReader.peek() match {
          case com.google.gson.stream.JsonToken.BEGIN_OBJECT =>
            arr.add(parseObject())
          case com.google.gson.stream.JsonToken.BEGIN_ARRAY =>
            arr.add(parseArray())
          case _ =>
            arr.add(JsonParser.parseReader(jsonReader))
        }
      }
      jsonReader.endArray()
      arr
    }

    parseElement()
    found.toSet
  }
}
