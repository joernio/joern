package io.joern.swiftsrc2cpg.utils

import com.google.gson.stream.{JsonReader, JsonToken}
import com.google.gson.{JsonArray, JsonObject, JsonParser}
import io.joern.swiftsrc2cpg.utils.SwiftTypesProvider.TypeInfo

import java.io.Reader
import scala.annotation.tailrec
import scala.collection.mutable

/** Utility for reading and extracting type information from Swift AST in JSON format. Parses Swift's JSON
  * representation to collect type information for CPG generation.
  */
object GsonTypeInfoReader {

  /** Field names that can contain type and decl fullnames in the Swift AST */
  private val FullnameFieldNames = Set("usr", "type", "result", "decl_usr", "interface_type")

  /** AST node kinds that require special handling.
    *
    * TODO: there are maybe more AST node kinds that need special handling
    */
  private object NodeKinds {
    val DeclRefExpr            = "declref_expr"
    val FunctionConversionExpr = "function_conversion_expr"
    val MemberRefExpr          = "member_ref_expr"
    val DeclRef                = "decl_ref"
    val CallExpr               = "call_expr"
  }

  /** Safely retrieves a string property from a JsonObject.
    *
    * @param obj
    *   The JSON object to extract from
    * @param propertyName
    *   The name of the property to extract
    * @return
    *   Some(value) if property exists and is a primitive, None otherwise
    */
  private def safePropertyValue(obj: JsonObject, propertyName: String): Option[String] = {
    Option.when(obj.has(propertyName) && obj.get(propertyName).isJsonPrimitive)(obj.get(propertyName).getAsString)
  }

  /** Safely retrieves a JsonObject property.
    *
    * @param obj
    *   The JSON object to extract from
    * @param propertyName
    *   The name of the property to extract
    * @return
    *   Some(jsonObject) if property exists and is an object, None otherwise
    */
  private def safePropertyObject(obj: JsonObject, propertyName: String): Option[JsonObject] = {
    Option.when(obj.has(propertyName) && obj.get(propertyName).isJsonObject)(obj.get(propertyName).getAsJsonObject)
  }

  /** Extracts the source range from a JSON object.
    *
    * @param obj
    *   The JSON object containing a range property
    * @return
    *   A tuple of (start, end) positions
    */
  private def range(obj: JsonObject): (Int, Int) = {
    val rangeObj = obj.get("range").getAsJsonObject
    (rangeObj.get("start").getAsInt, rangeObj.get("end").getAsInt)
  }

  /** Determines if a JSON object contains useful type information.
    *
    * @param obj
    *   The JSON object to check
    * @return
    *   true if the object contains type information, false otherwise
    */
  private def qualifies(obj: JsonObject): Boolean = {
    FullnameFieldNames.exists(n => safePropertyValue(obj, n).isDefined) && obj.has("range")
  }

  /** Gets the AST node kind from a JSON object.
    *
    * @param obj
    *   The JSON object representing an AST node
    * @return
    *   The kind of the AST node as a string
    */
  private def astNodeKind(obj: JsonObject): String = {
    obj.get("_kind").getAsString
  }

  /** Determines if a JSON object represents an AST node.
    *
    * @param obj
    *   The JSON object to check
    * @return
    *   true if the object is an AST node, false otherwise
    */
  private def isAstNode(obj: JsonObject): Boolean = {
    obj.has("_kind")
  }

  /** Recursively traverses a call expression to find its declaration.
    *
    * @param obj
    *   The JSON object representing a call expression
    * @return
    *   The JSON object representing the declaration
    */
  @tailrec
  private def declFromCallExpr(obj: JsonObject): JsonObject = {
    safePropertyObject(obj, "fn") match {
      case Some(fn) =>
        astNodeKind(fn) match {
          case NodeKinds.DeclRefExpr            => fn.getAsJsonObject("decl")
          case NodeKinds.FunctionConversionExpr => fn.getAsJsonObject("sub_expr").getAsJsonObject("decl")
          case other if other.endsWith("_expr") => declFromCallExpr(fn)
          case _                                => obj
        }
      case None => obj
    }
  }

  /** Extracts declaration from a member reference expression.
    *
    * @param obj
    *   The JSON object representing a member reference expression
    * @return
    *   The JSON object representing the declaration
    */
  private def declFromMemberRefExpr(obj: JsonObject): JsonObject = {
    safePropertyObject(obj, "decl") match {
      case Some(decl) if astNodeKind(decl) == NodeKinds.DeclRef => decl
      case _                                                    => obj
    }
  }

  /** Collects type information from Swift AST JSON.
    *
    * @param reader
    *   The reader providing the JSON input
    * @return
    *   A set of TypeInfo objects extracted from the JSON
    */
  def collectTypeInfo(reader: Reader): Set[TypeInfo] = {
    val found      = mutable.HashSet.empty[TypeInfo]
    val jsonReader = new JsonReader(reader)
    var filename   = ""

    def parseObject(): JsonObject = {
      val obj         = new JsonObject
      var hasTypeInfo = false

      jsonReader.beginObject()
      while (jsonReader.hasNext) {
        val name = jsonReader.nextName()
        jsonReader.peek() match {
          case JsonToken.BEGIN_OBJECT if isAstNode(obj) =>
            val value = parseObject()
            obj.add(name, value)
          case JsonToken.BEGIN_OBJECT =>
            // don't descend
            jsonReader.skipValue()
          case JsonToken.BEGIN_ARRAY =>
            val value = parseArray()
            obj.add(name, value)
          case _ =>
            val value = JsonParser.parseReader(jsonReader)
            obj.add(name, value)
        }
        if (qualifies(obj)) hasTypeInfo = true
        if (name == "filename") filename = obj.get("filename").getAsString
      }
      jsonReader.endObject()

      if (hasTypeInfo && !filename.contains(".build")) {
        extractTypeInfo(obj, filename)
      }
      obj
    }

    /** Extracts type information from an AST node and adds it to the result set.
      *
      * @param obj
      *   The JSON object representing an AST node
      * @param filename
      *   The source file name
      */
    def extractTypeInfo(obj: JsonObject, filename: String): Unit = {
      val nodeKind = astNodeKind(obj)
      val range_   = range(obj)

      lazy val declFullName = nodeKind match {
        case kind if kind.endsWith(NodeKinds.CallExpr) => safePropertyValue(declFromCallExpr(obj), "decl_usr")
        case NodeKinds.MemberRefExpr                   => safePropertyValue(declFromMemberRefExpr(obj), "decl_usr")
        case _                                         => safePropertyValue(obj, "decl_usr")
      }

      val typeName = safePropertyValue(obj, "type")
        .orElse(safePropertyValue(obj, "result"))
        .orElse(safePropertyValue(obj, "interface_type"))
      val usrFullName = safePropertyValue(obj, "usr").orElse(declFullName)

      found.addOne(TypeInfo(filename, range_, typeName, usrFullName, nodeKind))
    }

    /** Parses a JSON array.
      *
      * @return
      *   The parsed JsonArray
      */
    def parseArray(): JsonArray = {
      val arr = new JsonArray
      jsonReader.beginArray()
      while (jsonReader.hasNext) {
        jsonReader.peek() match {
          case JsonToken.BEGIN_OBJECT => arr.add(parseObject())
          case JsonToken.BEGIN_ARRAY  => arr.add(parseArray())
          case _                      => jsonReader.skipValue()
        }
      }
      jsonReader.endArray()
      arr
    }

    // Start parsing based on the root element type
    jsonReader.peek() match {
      case JsonToken.BEGIN_OBJECT => parseObject()
      case JsonToken.BEGIN_ARRAY  => parseArray()
      case _                      => jsonReader.skipValue()
    }

    found.toSet
  }
}
