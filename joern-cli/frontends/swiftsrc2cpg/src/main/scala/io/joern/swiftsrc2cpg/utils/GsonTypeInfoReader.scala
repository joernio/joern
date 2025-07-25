package io.joern.swiftsrc2cpg.utils

import com.google.gson.stream.JsonReader
import com.google.gson.{JsonArray, JsonObject, JsonParser}
import io.joern.swiftsrc2cpg.utils.SwiftTypesProvider.TypeInfo

import java.io.Reader
import scala.collection.mutable.ListBuffer

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

  def collectTypeInfo(reader: Reader): Seq[(String, TypeInfo)] = {
    val found      = ListBuffer.empty[(String, TypeInfo)]
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
        val nodeKind       = obj.get("_kind").getAsString
        val range_         = range(obj)
        val typeName       = safePropertyValue(obj, "type")
        val resultTypeName = safePropertyValue(obj, "result")
        val usrFullName    = safePropertyValue(obj, "usr")
        val declFullName   = safePropertyValue(obj, "decl_usr")
        found.addOne(
          (filename, TypeInfo(range_, typeName.orElse(resultTypeName), usrFullName.orElse(declFullName), nodeKind))
        )
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
    found.toList
  }
}
