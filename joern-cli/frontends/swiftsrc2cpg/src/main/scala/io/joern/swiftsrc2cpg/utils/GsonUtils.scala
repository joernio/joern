package io.joern.swiftsrc2cpg.utils

import com.google.gson.stream.JsonReader
import com.google.gson.{JsonArray, JsonObject, JsonParser}
import io.joern.swiftsrc2cpg.utils.SwiftTypesProvider.TypeInfo

import java.io.Reader
import scala.collection.mutable.ListBuffer

object GsonUtils {

  /** Collects all JsonObjects containing the type and fullName specific properties from a very large JSON file using a
    * JsonReader.
    *
    * @param reader
    *   A java.io.Reader for the JSON input.
    * @param properties
    *   The properties as Set of String to look for.
    * @return
    *   A sequence of [[TypeInfo]] with their filename.
    */
  def collectJsonNodes(reader: Reader, properties: Set[String]): Seq[(String, TypeInfo)] = {
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
        if (properties.exists(p => obj.has(p) && obj.get(p).isJsonPrimitive) && obj.has("range")) hasProperty = true
        if (name == "filename") filename = obj.get("filename").getAsString
      }
      jsonReader.endObject()

      if (hasProperty) {
        val rangeObj        = obj.get("range").getAsJsonObject
        val pos             = (rangeObj.get("start").getAsInt, rangeObj.get("end").getAsInt)
        val mangledTypeName = Option.when(obj.has("type")) { obj.get("type").getAsString }
        val mangledResultTypeName = Option.when(obj.has("result") && obj.get("result").isJsonPrimitive) {
          obj.get("result").getAsString
        }
        val mangledDeclFullName = Option.when(obj.has("usr")) { obj.get("usr").getAsString }
        val mangledFullName     = Option.when(obj.has("decl_usr")) { obj.get("decl_usr").getAsString }
        found.addOne(
          (
            filename,
            TypeInfo(
              pos,
              mangledTypeName.orElse(mangledResultTypeName),
              mangledDeclFullName.orElse(mangledFullName),
              obj
            )
          )
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
