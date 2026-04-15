package io.joern.swiftsrc2cpg.utils

import com.google.gson.stream.{JsonReader, JsonToken}
import com.google.gson.Strictness
import io.joern.swiftsrc2cpg.utils.SwiftTypesProvider.TypeInfo

import java.io.Reader
import scala.annotation.tailrec
import scala.collection.mutable

/** Utility for reading and extracting type information from Swift AST in JSON format. Parses Swift's JSON
  * representation to collect type information for CPG generation.
  */
object GsonTypeInfoReader {

  /** Child object/array field names actually accessed by `extractTypeInfo` and its helpers (`range`,
    * `declFromCallExpr`, `resultFromReturnStmt`, `declFromMemberRefExpr`, `conformancesFromNode`,
    * `superClassesFromNode`). Children with names outside this set are still recursed for side effects but not retained
    * on the parent node, avoiding retention of large irrelevant subtrees.
    */
  private val RelevantChildFieldNames =
    Set("range", "fn", "decl", "sub_expr", "result", "inherits", "conformances", "attrs")

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

  /** Minimal mutable node model used while token-streaming JSON with JsonReader.
    *
    * This avoids Gson DOM allocations while retaining only fields needed for TypeInfo extraction.
    *
    * '''Performance note on nullable fields:''' This class intentionally uses `null` instead of `Option[T]` for
    * performance reasons. Swift compiler JSON output can be large (hundreds of MB), creating thousands of AstNode
    * instances during streaming parsing. Using `Option[T]` for ~20 fields per node would:
    *   - Add ~20 object allocations per AstNode (one `Some`/`None` per field)
    *   - Increase GC pressure significantly during bulk parsing
    *   - Add pattern-matching overhead on every field access
    *
    * Safety is maintained by:
    *   - This class is `private` to this object - no external access
    *   - All field access goes through null-safe wrappers (`Option(field)`) or explicit null checks
    *   - Fields are only set during parsing and read once during `extractTypeInfo`
    *
    * This is a conscious trade-off: performance optimization in a hot path at the cost of idiomatic Scala.
    */
  private final class AstNode {
    var kind: String                                = null
    var start: Int                                  = -1
    var end: Int                                    = -1
    var rangeNode: AstNode                          = null
    var attrs: mutable.ArrayBuffer[AstNode]         = null
    var fn: AstNode                                 = null
    var decl: AstNode                               = null
    var subExpr: AstNode                            = null
    var resultObj: AstNode                          = null
    var inheritsObj: AstNode                        = null
    var inheritsLegacy: mutable.ArrayBuffer[String] = null
    var conformances: mutable.ArrayBuffer[AstNode]  = null

    var typeValue: String      = null
    var typeUsr: String        = null
    var resultValue: String    = null
    var interfaceType: String  = null
    var extendedType: String   = null
    var usr: String            = null
    var declUsr: String        = null
    var protocol: String       = null
    var superclassType: String = null

    def hasTypeOrDeclInfo: Boolean = {
      typeValue != null || typeUsr != null || resultValue != null || interfaceType != null || extendedType != null ||
      usr != null || declUsr != null || protocol != null || superclassType != null ||
      // Also check for child objects that contain type/decl info (e.g., resultObj in return_stmt, fn/decl in call_expr)
      resultObj != null || fn != null || decl != null
    }
  }

  private def safeRange(node: AstNode): Option[(Int, Int)] = {
    Option(node.rangeNode)
      .filter(r => r.start > -1 && r.end > -1)
      .map { rangeObj =>
        val start = rangeObj.start
        val end   = rangeObj.end

        // no adjustment needed for zero-length ranges, e.g., for member accesses:
        if (start == end) {
          (start, end)
        } else {
          val endOffset = node.kind match {
            case NodeKinds.DotCallExpr => 3 // offsets are off by 2 from SwiftParser for simple dot calls
            case _                     => 1
          }

          /** Handles cases where attributes modify the start position of the range. For example, in the presence of
            * attributes like `@escaping` or for access modifiers like `static` the start position may need to be
            * adjusted to account for the attribute's position because swift-parser does include attributes in the range
            * of the associated declaration while swiftc does not.
            */
          val startWithModifiers = Option(node.attrs)
            .flatMap(_.collectFirst(Function.unlift(attr => safeRange(attr))))
            .map {
              case (s, _) if s < start => s
              case _                   => start
            }
            .getOrElse(start)

          (startWithModifiers, end + endOffset)
        }
      }
  }

  private def qualifies(node: AstNode): Boolean = {
    safeRange(node).isDefined && node.hasTypeOrDeclInfo
  }

  private def isParameter(node: AstNode): Boolean = {
    node.kind == "parameter" && node.hasTypeOrDeclInfo
  }

  @tailrec
  private def declFromCallExpr(node: AstNode): AstNode = {
    val fn = node.fn
    if (fn == null) {
      node
    } else {
      fn.kind match {
        case NodeKinds.DeclRefExpr            => Option(fn.decl).getOrElse(node)
        case NodeKinds.FunctionConversionExpr => Option(fn.subExpr).flatMap(s => Option(s.decl)).getOrElse(node)
        case other if other != null && other.endsWith("_expr") =>
          declFromCallExpr(fn)
        case _ => node
      }
    }
  }

  private def resultFromReturnStmt(node: AstNode): AstNode = Option(node.resultObj).getOrElse(node)

  private def declFromMemberRefExpr(node: AstNode): AstNode = {
    Option(node.decl).filter(_.kind == NodeKinds.DeclRef).getOrElse(node)
  }

  /** Until swiftc 6.1.x `inherits` is a plain JSON array storing the mangled fullNames directly */
  private def superClassesFromNodeLegacy(node: AstNode): Seq[String] = {
    Option(node.inheritsLegacy).map(_.toSeq).getOrElse(Seq.empty)
  }

  /** Starting with swiftc 6.2.x `inherits` is an actual JSON object */
  private def superClassesFromNode(node: AstNode): Seq[String] = {
    Option(node.inheritsObj).flatMap(n => Option(n.superclassType)).toSeq
  }

  /** Starting with swiftc 6.2.x `inherits` is an actual JSON object */
  private def conformancesFromNode(node: AstNode): Seq[String] = {
    Option(node.inheritsObj)
      .flatMap(inh => Option(inh.conformances))
      .map(_.flatMap(c => Option(c.protocol)).toSeq)
      .getOrElse(Seq.empty)
  }

  private def declFullNameFromNode(node: AstNode, declNode: AstNode): Option[String] = {
    Option(node.usr).orElse(Option(declNode.declUsr))
  }

  private def typeFullNameFromNode(node: AstNode): Option[String] = {
    Option(node.typeValue)
      .orElse(Option(node.typeUsr))
      .orElse(Option(node.resultValue))
      .orElse(Option(node.interfaceType))
      .orElse(Option(node.extendedType))
  }

  private def isInBuildFolder(filename: String): Boolean = {
    filename.contains("/.build/") || filename.contains("\\.build\\") || filename.contains("/Build/")
  }

  /** Collects type information from Swift AST JSON, emitting each [[TypeInfo]] via the provided callback as it is
    * discovered during streaming token-by-token parsing. No intermediate collection is built.
    *
    * @param reader
    *   The reader providing the JSON input
    * @param emit
    *   Called once for each [[TypeInfo]] extracted from the JSON stream
    */
  def collectTypeInfo(reader: Reader, emit: TypeInfo => Unit): Unit = {
    val jsonReader   = new JsonReader(reader)
    var filename     = ""
    var currentRange = Option.empty[(Int, Int)]

    // Configure reader for better performance
    jsonReader.setStrictness(Strictness.LENIENT)

    def readPrimitiveAsString(): Option[String] = {
      jsonReader.peek() match {
        case JsonToken.STRING  => Some(jsonReader.nextString())
        case JsonToken.NUMBER  => Some(jsonReader.nextString())
        case JsonToken.BOOLEAN => Some(jsonReader.nextBoolean().toString)
        case JsonToken.NULL =>
          jsonReader.nextNull()
          None
        case _ =>
          jsonReader.skipValue()
          None
      }
    }

    def readPrimitiveAsInt(): Option[Int] = {
      jsonReader.peek() match {
        case JsonToken.NUMBER =>
          Some(jsonReader.nextInt())
        case _ =>
          jsonReader.skipValue()
          None
      }
    }

    def addChild(parent: AstNode, name: String, child: AstNode): Unit = name match {
      case "range"    => parent.rangeNode = child
      case "fn"       => parent.fn = child
      case "decl"     => parent.decl = child
      case "sub_expr" => parent.subExpr = child
      case "result"   => parent.resultObj = child
      case "inherits" => parent.inheritsObj = child
      case _          =>
    }

    def addChildFromArray(parent: AstNode, name: String, child: AstNode): Unit = name match {
      case "attrs" =>
        if (parent.attrs == null) parent.attrs = mutable.ArrayBuffer.empty[AstNode]
        parent.attrs += child
      case "conformances" =>
        if (parent.conformances == null) parent.conformances = mutable.ArrayBuffer.empty[AstNode]
        parent.conformances += child
      case _ =>
    }

    def extractTypeInfo(node: AstNode, filename: String, rangeOpt: Option[(Int, Int)]): Unit = {
      val nodeKind   = node.kind
      val maybeRange = rangeOpt.orElse(safeRange(node))
      lazy val declObj = nodeKind match {
        case kind if kind.endsWith(NodeKinds.CallExpr) => declFromCallExpr(node)
        case NodeKinds.MemberRefExpr                   => declFromMemberRefExpr(node)
        case _                                         => node
      }

      val typeObj = nodeKind match {
        case NodeKinds.ReturnStmt => resultFromReturnStmt(node)
        case _                    => node
      }

      val typeFullName    = typeFullNameFromNode(typeObj)
      val declFullName    = declFullNameFromNode(node, declObj)
      val conformances    = conformancesFromNode(node)
      val superClassTypes = superClassesFromNode(node) ++ superClassesFromNodeLegacy(node)

      maybeRange.foreach { range_ =>
        emit(TypeInfo(filename, range_, typeFullName, declFullName, superClassTypes ++ conformances, nodeKind))
      }
    }

    def parseArray(parent: AstNode, fieldName: String): Unit = {
      jsonReader.beginArray()
      while (jsonReader.hasNext) {
        jsonReader.peek() match {
          case JsonToken.BEGIN_OBJECT =>
            val child = parseObject()
            if (RelevantChildFieldNames.contains(fieldName)) {
              addChildFromArray(parent, fieldName, child)
            }
          case JsonToken.BEGIN_ARRAY =>
            parseArray(parent, fieldName)
          case JsonToken.STRING if fieldName == "inherits" =>
            val value = jsonReader.nextString()
            if (parent.inheritsLegacy == null) parent.inheritsLegacy = mutable.ArrayBuffer.empty[String]
            parent.inheritsLegacy += value
          case _ =>
            jsonReader.skipValue()
        }
      }
      jsonReader.endArray()
    }

    def parseObject(): AstNode = {
      val node        = new AstNode
      var hasKind     = false
      var isFromBuild = false

      jsonReader.beginObject()
      while (jsonReader.hasNext) {
        val name = jsonReader.nextName()

        if (name == "_kind") {
          hasKind = true
          node.kind = readPrimitiveAsString().orNull
        } else if (name == "filename") {
          filename = readPrimitiveAsString().orNull
          isFromBuild = filename != null && isInBuildFolder(filename)
        } else {
          jsonReader.peek() match {
            case JsonToken.BEGIN_OBJECT if hasKind && !isFromBuild =>
              val child = parseObject()
              if (RelevantChildFieldNames.contains(name)) {
                addChild(node, name, child)
              }
            case JsonToken.BEGIN_OBJECT =>
              // don't descend
              jsonReader.skipValue()
            case JsonToken.BEGIN_ARRAY if hasKind && !isFromBuild =>
              parseArray(node, name)
            case JsonToken.BEGIN_ARRAY =>
              // don't descend
              jsonReader.skipValue()
            case _ =>
              name match {
                case "start" =>
                  node.start = readPrimitiveAsInt().getOrElse(-1)
                case "end" =>
                  node.end = readPrimitiveAsInt().getOrElse(-1)
                case "type" =>
                  node.typeValue = readPrimitiveAsString().orNull
                case "type_usr" =>
                  node.typeUsr = readPrimitiveAsString().orNull
                case "result" =>
                  node.resultValue = readPrimitiveAsString().orNull
                case "interface_type" =>
                  node.interfaceType = readPrimitiveAsString().orNull
                case "extended_type" =>
                  node.extendedType = readPrimitiveAsString().orNull
                case "usr" =>
                  node.usr = readPrimitiveAsString().orNull
                case "decl_usr" =>
                  node.declUsr = readPrimitiveAsString().orNull
                case "protocol" =>
                  node.protocol = readPrimitiveAsString().orNull
                case "superclass_type" =>
                  node.superclassType = readPrimitiveAsString().orNull
                case _ =>
                  jsonReader.skipValue()
              }
          }
          currentRange = safeRange(node).orElse(currentRange)
        }
      }
      jsonReader.endObject()

      if (!isFromBuild && node.kind != null) {
        if (qualifies(node)) {
          extractTypeInfo(node, filename, None)
        } else if (isParameter(node)) {
          extractTypeInfo(node, filename, currentRange)
        }
      }
      node
    }

    var shouldTerminate = false
    while (!shouldTerminate && jsonReader.hasNext) {
      // Start parsing based on the root element type
      jsonReader.peek() match {
        case JsonToken.BEGIN_OBJECT => parseObject()
        case JsonToken.BEGIN_ARRAY =>
          val sinkNode = new AstNode
          parseArray(sinkNode, "")
        case _ => shouldTerminate = true
      }
    }

    jsonReader.close()
  }
}
