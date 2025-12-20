package io.joern.swiftsrc2cpg.utils

import com.google.gson.stream.{JsonReader, JsonToken}
import com.google.gson.{JsonArray, JsonObject, JsonParser, Strictness}
import io.joern.swiftsrc2cpg.utils.SwiftTypesProvider.TypeInfo

import java.io.Reader
import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*
import scala.collection.mutable

/** Utility for reading and extracting type information from Swift AST in JSON format. Parses Swift's JSON
  * representation to collect type information for CPG generation.
  */
object GsonTypeInfoReader {

  /** Field names that can contain decl fullNames in the Swift AST */
  private val DeclFullNameFieldNames = Set("usr", "decl_usr", "protocol", "superclass_type")

  /** Field names that can contain type fullNames in the Swift AST */
  private val TypeFullNameFieldNames = Set("type", "type_usr", "result", "interface_type", "extended_type")

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
    val ReturnStmt             = "return_stmt"
    val DotCallExpr            = "dot_syntax_call_expr"
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

  /** Safely retrieves a JsonArray property.
    *
    * @param obj
    *   The JSON object to extract from
    * @param propertyName
    *   The name of the property to extract
    * @return
    *   Some(jsonArray) if property exists and is an array, None otherwise
    */
  private def safePropertyArray(obj: JsonObject, propertyName: String): Option[JsonArray] = {
    Option.when(obj.has(propertyName) && obj.get(propertyName).isJsonArray)(obj.get(propertyName).getAsJsonArray)
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
    val start    = rangeObj.get("start").getAsInt
    val end      = rangeObj.get("end").getAsInt

    // no adjustment needed for zero-length ranges, e.g., for member accesses:
    if (start == end) return (start, end)

    val endOffset = astNodeKind(obj) match {
      case NodeKinds.DotCallExpr => 3 // offsets are off by 2 from SwiftParser for simple dot calls
      case _                     => 1
    }
    val startWithModifiers = obj match {
      case _ if obj.has("attrs") =>
        /** Handles cases where attributes modify the start position of the range. For example, in the presence of
          * attributes like `@escaping` or for access modifiers like `static` the start position may need to be adjusted
          * to account for the attribute's position because swift-parser does include attributes in the range of the
          * associated declaration while swiftc does not.
          */
        val elemWithRange = safePropertyArray(obj, "attrs").flatMap { arr =>
          arr.asList().asScala.collectFirst {
            case attrObj if safeRange(attrObj.getAsJsonObject).isDefined => attrObj.getAsJsonObject
          }
        }
        elemWithRange.map(range) match {
          case Some((s, e)) if s < start => s
          case _                         => start
        }
      case _ => start
    }
    (startWithModifiers, end + endOffset)
  }

  /** Safely extracts the source range from a JSON object.
    *
    * @param obj
    *   The JSON object to extract range information from
    * @return
    *   Some((start, end)) if the range property exists, None otherwise
    */
  private def safeRange(obj: JsonObject): Option[(Int, Int)] = {
    Option.when(obj.has("range"))(range(obj))
  }

  /** Determines if a JSON object contains useful type or decl fullName information.
    *
    * @param obj
    *   The JSON object to check
    * @return
    *   true if the object contains type or decl fullName information, false otherwise
    */
  private def qualifies(obj: JsonObject): Boolean = {
    obj.has("range") && (TypeFullNameFieldNames.exists(obj.has) || DeclFullNameFieldNames.exists(obj.has))
  }

  /** Determines if a JSON object represents a parameter in the Swift AST.
    *
    * @param obj
    *   The JSON object to check
    * @return
    *   true if the object is a parameter with type or declaration information, false otherwise
    */
  private def isParameter(obj: JsonObject): Boolean = {
    safePropertyValue(obj, "_kind").contains("parameter") &&
    (TypeFullNameFieldNames.exists(obj.has) || DeclFullNameFieldNames.exists(obj.has))
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

  /** Extracts result object from a return statement.
    *
    * @param obj
    *   The JSON object representing a return statement
    * @return
    *   The JSON object representing the result or the original object if result not found
    */
  private def resultFromReturnStmt(obj: JsonObject): JsonObject = {
    safePropertyObject(obj, "result") match {
      case Some(resultObj) => resultObj
      case None            => obj
    }
  }

  /** Extracts declaration from a member reference expression.
    *
    * @param obj
    *   The JSON object representing a member reference expression
    * @return
    *   The JSON object representing the declaration or the original object if result not found
    */
  private def declFromMemberRefExpr(obj: JsonObject): JsonObject = {
    safePropertyObject(obj, "decl") match {
      case Some(decl) if astNodeKind(decl) == NodeKinds.DeclRef => decl
      case _                                                    => obj
    }
  }

  /** Until swiftc 6.1.x `inherits` is a plain JSON array storing the mangled fullNames directly */
  private def superClassesFromNodeLegacy(obj: JsonObject): Seq[String] = {
    safePropertyArray(obj, "inherits")
      .filter(_ != null)
      .map(_.asList().asScala.toSeq.map(_.getAsString))
      .getOrElse(Seq.empty)
  }

  /** Starting with swiftc 6.2.x `inherits` is an actual JSON object */
  private def superClassesFromNode(obj: JsonObject): Seq[String] = {
    safePropertyObject(obj, "inherits").flatMap(safePropertyValue(_, "superclass_type")).toSeq
  }

  /** Starting with swiftc 6.2.x `inherits` is an actual JSON object */
  private def conformancesFromNode(obj: JsonObject): Seq[String] = {
    safePropertyObject(obj, "inherits")
      .flatMap(safePropertyArray(_, "conformances"))
      .filter(_ != null)
      .map(_.asList().asScala.toSeq.flatMap(r => safePropertyValue(r.getAsJsonObject, "protocol")))
      .getOrElse(Seq.empty)
  }

  private def declFullNameFromNode(obj: JsonObject, declObj: JsonObject): Option[String] = {
    safePropertyValue(obj, "usr").orElse(safePropertyValue(declObj, "decl_usr"))
  }

  private def typeFullNameFromNode(obj: JsonObject): Option[String] = {
    safePropertyValue(obj, "type")
      .orElse(safePropertyValue(obj, "type_usr"))
      .orElse(safePropertyValue(obj, "result"))
      .orElse(safePropertyValue(obj, "interface_type"))
      .orElse(safePropertyValue(obj, "extended_type"))
  }

  private def isInBuildFolder(filename: String): Boolean = {
    filename.contains("/.build/") || filename.contains("\\.build\\") || filename.contains("/Build/")
  }

  /** Collects type information from Swift AST JSON.
    *
    * @param reader
    *   The reader providing the JSON input
    * @return
    *   A set of TypeInfo objects extracted from the JSON
    */
  def collectTypeInfo(reader: Reader): Set[TypeInfo] = {
    val found        = mutable.HashSet.empty[TypeInfo]
    val jsonReader   = new JsonReader(reader)
    var filename     = ""
    var currentRange = Option.empty[(Int, Int)]

    // Configure reader for better performance
    jsonReader.setStrictness(Strictness.LENIENT)

    def parseObject(): JsonObject = {
      val obj         = new JsonObject
      var hasKind     = false
      var isFromBuild = false

      jsonReader.beginObject()
      while (jsonReader.hasNext) {
        val name = jsonReader.nextName()

        if (name == "_kind") {
          hasKind = true
          val value = JsonParser.parseReader(jsonReader)
          obj.add(name, value)
        } else if (name == "filename") {
          filename = JsonParser.parseReader(jsonReader).getAsString
          isFromBuild = isInBuildFolder(filename)
        } else {
          jsonReader.peek() match {
            case JsonToken.BEGIN_OBJECT if hasKind && !isFromBuild =>
              obj.add(name, parseObject())
            case JsonToken.BEGIN_OBJECT =>
              // don't descend
              jsonReader.skipValue()
            case JsonToken.BEGIN_ARRAY if hasKind && !isFromBuild =>
              obj.add(name, parseArray())
            case JsonToken.BEGIN_ARRAY =>
              // don't descend
              jsonReader.skipValue()
            case _ =>
              if (
                name == "start" || name == "end" ||
                TypeFullNameFieldNames.contains(name) ||
                DeclFullNameFieldNames.contains(name)
              ) {
                val value = JsonParser.parseReader(jsonReader)
                obj.add(name, value)
              } else jsonReader.skipValue()
          }
          currentRange = safeRange(obj).orElse(currentRange)
        }
      }
      jsonReader.endObject()

      if (qualifies(obj)) {
        extractTypeInfo(obj, filename, None)
      } else if (isParameter(obj)) {
        extractTypeInfo(obj, filename, currentRange)
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
    def extractTypeInfo(obj: JsonObject, filename: String, rangeOpt: Option[(Int, Int)]): Unit = {
      val nodeKind = astNodeKind(obj)
      val range_   = rangeOpt.getOrElse(range(obj))

      lazy val declObj = nodeKind match {
        case kind if kind.endsWith(NodeKinds.CallExpr) => declFromCallExpr(obj)
        case NodeKinds.MemberRefExpr                   => declFromMemberRefExpr(obj)
        case _                                         => obj
      }

      val typeObj = nodeKind match {
        case NodeKinds.ReturnStmt => resultFromReturnStmt(obj)
        case _                    => obj
      }

      val typeFullName    = typeFullNameFromNode(typeObj)
      val declFullName    = declFullNameFromNode(obj, declObj)
      val conformances    = conformancesFromNode(obj)
      val superClassTypes = superClassesFromNode(obj) ++ superClassesFromNodeLegacy(obj)

      found.add(TypeInfo(filename, range_, typeFullName, declFullName, superClassTypes ++ conformances, nodeKind))
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
          case JsonToken.STRING       => arr.add(jsonReader.nextString())
          case _                      => jsonReader.skipValue()
        }
      }
      jsonReader.endArray()
      arr
    }

    var shouldTerminate = false
    while (!shouldTerminate && jsonReader.hasNext) {
      // Start parsing based on the root element type
      jsonReader.peek() match {
        case JsonToken.BEGIN_OBJECT => parseObject()
        case JsonToken.BEGIN_ARRAY  => parseArray()
        case _                      => shouldTerminate = true
      }
    }

    jsonReader.close()
    found.toSet
  }
}
