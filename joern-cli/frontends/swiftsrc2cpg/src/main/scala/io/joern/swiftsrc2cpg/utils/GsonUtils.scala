package io.joern.swiftsrc2cpg.utils

import com.google.gson.stream.JsonReader
import com.google.gson.{JsonArray, JsonObject, JsonParser}

import java.io.Reader
import scala.collection.mutable.ListBuffer

object GsonUtils {

  /** Collects all JsonObjects containing the given property from a very large JSON file using a JsonReader.
    *
    * @param reader
    *   A java.io.Reader for the JSON input.
    * @param property
    *   The property as String to look for.
    * @return
    *   A sequence of JsonObjects each containing given property.
    */
  def collectJsonNodesWithProperty(reader: Reader, property: String): Seq[JsonObject] = {
    val found      = ListBuffer.empty[JsonObject]
    val jsonReader = new JsonReader(reader)

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
        if (name == property) hasProperty = true
      }
      jsonReader.endObject()

      if (hasProperty) found += obj
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
