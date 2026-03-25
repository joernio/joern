package io.joern.abap2cpg.parser

import io.joern.abap2cpg.parser.AbapIntermediateAst.*
import ujson.*

import java.nio.file.{Files, Path, Paths}
import scala.util.{Try, Success, Failure}

/** Parses JSON output from parse-abap.js into ABAP intermediate AST */
class AbapJsonParser {

  /** Read and parse a JSON file produced by parse-abap.js */
  def parseFile(jsonPath: Path): Try[ProgramRoot] = {
    Try {
      val jsonContent = new String(Files.readAllBytes(jsonPath))
      val json = ujson.read(jsonContent)
      parseProgram(json)
    }
  }

  /** Parse the top-level program structure */
  private def parseProgram(json: Value): ProgramRoot = {
    val fileName = json("file").str
    val objectType = json("objectType").str
    val methodImpls = json("methods").arr.map(parseMethod).toSeq

    // Parse method signatures from "Unknown" statements (METHOD definitions)
    val methodSigs = json("statements").arr.collect {
      case stmt if stmt("type").str == "Unknown" &&
                   stmt("tokens").arr.headOption.exists(_.obj("str").str == "METHODS") =>
        parseMethodSignature(stmt)
    }.toSeq

    // Combine implementations with signatures
    val methods = methodImpls ++ methodSigs

    // Extract class name from ClassDefinition statement if present
    val classOpt = json("statements").arr.collectFirst {
      case stmt if stmt("type").str == "ClassDefinition" =>
        parseClassDefinition(stmt, methods)
    }

    val interfaceOpt = json("statements").arr.collectFirst {
      case stmt if stmt("type").str == "Interface" =>
        parseInterfaceDefinition(stmt, methods)
    }

    ProgramRoot(
      fileName = fileName,
      objectType = objectType,
      classes = classOpt.toSeq,
      interfaces = interfaceOpt.toSeq,
      methods = methods,
      statements = Seq.empty,
      span = TextSpan(code = fileName)
    )
  }

  /** Parse a class definition from statement */
  private def parseClassDefinition(stmt: Value, methods: Seq[MethodDef]): ClassDef = {
    val tokens = stmt("tokens").arr.map(_.obj("str").str)
    val className = tokens.find(t => t != "CLASS" && t != "DEFINITION" && t.head.isLetter)
      .getOrElse("Unknown")

    val isPublic = tokens.exists(_ == "PUBLIC")
    val visibility = if (isPublic) "PUBLIC" else "PRIVATE"

    // Merge method definitions with implementations
    val mergedMethods = mergeMethods(methods)

    ClassDef(
      name = className,
      visibility = visibility,
      isFinal = false,
      methods = mergedMethods,
      attributes = Seq.empty,
      span = parseTextSpan(stmt)
    )
  }

  /** Merge method definitions with their implementations */
  private def mergeMethods(methods: Seq[MethodDef]): Seq[MethodDef] = {
    // Group methods by name
    val methodsByName = methods.groupBy(_.name)

    methodsByName.values.map { methodsWithSameName =>
      if (methodsWithSameName.length == 1) {
        // Only one entry (either definition or implementation)
        methodsWithSameName.head
      } else {
        // Multiple entries - merge definition (has params) with implementation (has body)
        val withParams = methodsWithSameName.find(m =>
          m.parameters.importing.nonEmpty ||
          m.parameters.exporting.nonEmpty ||
          m.parameters.changing.nonEmpty ||
          m.parameters.returning.isDefined
        )
        val withBody = methodsWithSameName.find(_.body.isDefined)

        (withParams, withBody) match {
          case (Some(definition), Some(implementation)) =>
            // Merge: use definition's parameters and visibility, implementation's body
            definition.copy(body = implementation.body)

          case (Some(definition), None) =>
            // Only definition, no implementation
            definition

          case (None, Some(implementation)) =>
            // Only implementation, no definition
            implementation

          case (None, None) =>
            // No params and no body - use first one
            methodsWithSameName.head
        }
      }
    }.toSeq
  }

  /** Parse an interface definition */
  private def parseInterfaceDefinition(stmt: Value, methods: Seq[MethodDef]): InterfaceDef = {
    val tokens = stmt("tokens").arr.map(_.obj("str").str)
    val interfaceName = tokens.find(t => t != "INTERFACE" && t.head.isLetter)
      .getOrElse("Unknown")

    InterfaceDef(
      name = interfaceName,
      visibility = "PUBLIC",
      methods = methods,
      span = parseTextSpan(stmt)
    )
  }

  /** Parse method signature from Unknown statement with METHODS tokens */
  private def parseMethodSignature(stmt: Value): MethodDef = {
    val tokens = stmt("tokens").arr.map(_.obj("str").str).toSeq

    // Find method name (token after METHODS)
    val methodNameIdx = tokens.indexOf("METHODS") + 1
    val methodName = if (methodNameIdx < tokens.size) tokens(methodNameIdx) else "unknown"

    // Parse parameters from token stream
    var importing = Seq[Parameter]()
    var exporting = Seq[Parameter]()
    var changing = Seq[Parameter]()
    var returning: Option[Parameter] = None

    var i = methodNameIdx + 1
    var currentSection = ""

    while (i < tokens.size) {
      tokens(i) match {
        case "IMPORTING" =>
          currentSection = "IMPORTING"
          i += 1

        case "EXPORTING" =>
          currentSection = "EXPORTING"
          i += 1

        case "CHANGING" =>
          currentSection = "CHANGING"
          i += 1

        case "RETURNING" =>
          currentSection = "RETURNING"
          i += 1
          // Skip "VALUE" and "("
          if (i < tokens.size && tokens(i) == "VALUE") i += 1
          if (i < tokens.size && tokens(i) == "(") i += 1

        case paramName if paramName != "TYPE" && paramName != "LENGTH" && paramName != "DECIMALS" &&
                         paramName != "VALUE" && !paramName.forall(_.isDigit) && paramName != ")" =>
          // This is a parameter name
          val pname = paramName
          i += 1

          // Skip TYPE keyword
          if (i < tokens.size && tokens(i) == "TYPE") i += 1

          // Get type
          val ptype = if (i < tokens.size) tokens(i) else "ANY"
          i += 1

          // Skip LENGTH/DECIMALS modifiers
          while (i < tokens.size && (tokens(i) == "LENGTH" || tokens(i) == "DECIMALS")) {
            i += 2 // Skip keyword and value
          }

          // Skip closing paren for RETURNING
          if (i < tokens.size && tokens(i) == ")") i += 1

          val param = Parameter(name = pname, typeName = ptype, isValue = true)

          currentSection match {
            case "IMPORTING" => importing :+= param
            case "EXPORTING" => exporting :+= param
            case "CHANGING" => changing :+= param
            case "RETURNING" => returning = Some(param)
            case _ => ()
          }

        case _ =>
          i += 1
      }
    }

    MethodDef(
      name = methodName,
      isStatic = false,
      visibility = None,
      parameters = MethodParameters(
        importing = importing,
        exporting = exporting,
        changing = changing,
        returning = returning
      ),
      body = None, // Signature only, no body
      span = parseTextSpan(stmt)
    )
  }

  /** Parse a method definition from JSON */
  private def parseMethod(methodJson: Value): MethodDef = {
    val name = methodJson("name").str
    val isStatic = methodJson.obj.get("isStatic").exists(_.bool)
    val visibility = methodJson.obj.get("visibility").flatMap(_.strOpt)

    val methodParams = methodJson.obj.get("parameters") match {
      case Some(params) if params != Null =>
        // Handle CLASS methods (importing/exporting/changing/returning)
        if (params.obj.contains("importing")) {
          MethodParameters(
            importing = params("importing").arr.map(parseParameter).toSeq,
            exporting = params("exporting").arr.map(parseParameter).toSeq,
            changing = params("changing").arr.map(parseParameter).toSeq,
            returning = params("returning") match {
              case Null => None
              case ret => Some(parseParameter(ret))
            }
          )
        }
        // Handle FORMs (using/changing/tables)
        else if (params.obj.contains("using")) {
          MethodParameters(
            importing = params("using").arr.map(parseParameter).toSeq,
            exporting = Seq.empty,
            changing = params("changing").arr.map(parseParameter).toSeq,
            returning = None
          )
        }
        else {
          MethodParameters() // Empty parameters
        }
      case _ =>
        MethodParameters() // Empty parameters
    }

    // Parse method body if present
    val body = methodJson.obj.get("body") match {
      case Some(bodyArr) if bodyArr != Null =>
        val statements = bodyArr.arr.map(parseStatement).toSeq
        Some(StatementList(statements, parseTextSpan(methodJson)))
      case _ =>
        None
    }

    MethodDef(
      name = name,
      visibility = visibility,
      isStatic = isStatic,
      parameters = methodParams,
      body = body,
      span = parseTextSpan(methodJson)
    )
  }

  /** Parse a parameter definition */
  private def parseParameter(paramJson: Value): Parameter = {
    Parameter(
      name = paramJson("name").str,
      typeName = paramJson("type") match {
        case Null => "ANY"  // FORMs don't declare types, use ANY as placeholder
        case t => t.str
      },
      isValue = paramJson.obj.get("isValue").exists(_.bool),
      isOptional = paramJson.obj.get("isOptional").exists(_.bool)
    )
  }

  /** Parse text span (position information) */
  private def parseTextSpan(node: Value): TextSpan = {
    val start = node.obj.get("start").map { s =>
      Position(
        row = s("row").num.toInt,
        col = s("col").num.toInt
      )
    }

    val end = node.obj.get("end").map { e =>
      Position(
        row = e("row").num.toInt,
        col = e("col").num.toInt
      )
    }

    val code = node.obj.get("tokens").map { tokens =>
      tokens.arr.map {
        case s: Str => s.str
        case obj: Obj => obj("str").str
        case _ => ""
      }.mkString(" ")
    }.getOrElse("")

    TextSpan(start, end, code)
  }

  /** Parse a statement from method body */
  private def parseStatement(stmtJson: Value): AbapNode = {
    val stmtType = stmtJson("type").str
    val tokens = stmtJson("tokens").arr.map(_.str).toSeq
    val span = parseTextSpan(stmtJson)

    stmtType match {
      // Parse Call statements (method calls, object->method calls)
      case "Call" | "CallFunction" =>
        // Check if this is a chained call (has multiple arrows)
        val instanceArrows = tokens.zipWithIndex.filter(_._1 == "->").map(_._2)
        val staticArrows = tokens.zipWithIndex.filter(_._1 == "=>").map(_._2)
        val allArrows = (instanceArrows ++ staticArrows).sorted

        if (allArrows.length > 1) {
          // Chained call - create a StatementList with multiple CallExpr nodes
          val calls = parseChainedCalls(tokens, span)
          StatementList(calls, span)
        } else {
          parseCallStatement(tokens, stmtType, span)
        }

      // Parse Move statements (assignments)
      case "Move" =>
        parseMoveStatement(tokens, span)

      // Parse Assign statements (dynamic assignments)
      case "Assign" =>
        parseAssignStatement(tokens, span)

      // Parse DATA declarations (local variables)
      case "Data" =>
        parseDataDeclaration(tokens, span)

      // For now, treat other statements as unknown
      case _ =>
        UnknownNode(stmtType, span)
    }
  }

  /** Parse Assign statement (ASSIGN source TO target) */
  private def parseAssignStatement(tokens: Seq[String], span: TextSpan): AbapNode = {
    // Find TO keyword
    val toIndex = tokens.indexOf("TO")

    if (toIndex <= 1 || toIndex >= tokens.length - 1) {
      // Invalid ASSIGN, return unknown node
      return UnknownNode("Assign", span)
    }

    // Parse source (between ASSIGN and TO)
    val sourceTokens = tokens.slice(1, toIndex) // Skip "ASSIGN"
    val source = parseExpression(sourceTokens, span)

    // Parse target (after TO)
    val targetTokens = tokens.drop(toIndex + 1).filterNot(_ == ".")
    val target = if (targetTokens.isEmpty) {
      IdentifierExpr("", span)
    } else if (targetTokens.head.startsWith("<") && targetTokens.head.endsWith(">")) {
      // Field symbol: <name>
      IdentifierExpr(targetTokens.head, span)
    } else if (targetTokens.contains("-")) {
      parseFieldAccess(targetTokens, span)
    } else {
      IdentifierExpr(targetTokens.head, span)
    }

    AssignmentStmt(target, source, span)
  }

  /** Parse DATA declaration (local variable) */
  private def parseDataDeclaration(tokens: Seq[String], span: TextSpan): AbapNode = {
    // DATA: lv_name TYPE i.
    // OR: DATA: lv_name TYPE string VALUE 'initial'.
    // Tokens: ["DATA", "lv_name", "TYPE", "i", "."]
    // OR: ["DATA", "lv_name", "TYPE", "string", "VALUE", "'initial'", "."]

    if (tokens.length < 4) {
      // Invalid DATA statement
      return UnknownNode("Data", span)
    }

    // Variable name is after DATA (may have colon)
    val nameIndex = if (tokens(1) == ":") 2 else 1
    val varName = tokens(nameIndex)

    // Type is after TYPE keyword
    val typeIndex = tokens.indexOf("TYPE")
    if (typeIndex < 0 || typeIndex >= tokens.length - 1) {
      return UnknownNode("Data", span)
    }

    val typeName = tokens(typeIndex + 1)

    // Check for VALUE keyword (initial value)
    val valueIndex = tokens.indexOf("VALUE")
    val initialValue = if (valueIndex > 0 && valueIndex < tokens.length - 1) {
      // Get tokens after VALUE until end or period
      val valueTokens = tokens.drop(valueIndex + 1).takeWhile(_ != ".")
      if (valueTokens.nonEmpty) {
        Some(parseExpression(valueTokens, span))
      } else {
        None
      }
    } else {
      None
    }

    DataDeclaration(varName, typeName, initialValue, span)
  }

  /** Parse Move statement (assignment) */
  private def parseMoveStatement(tokens: Seq[String], span: TextSpan): AbapNode = {
    // Find the assignment operator
    val assignIndex = tokens.indexOf("=")

    if (assignIndex <= 0 || assignIndex >= tokens.length - 1) {
      // Invalid assignment, return unknown node
      return UnknownNode("Move", span)
    }

    // Parse left side (target) - typically just an identifier
    val targetTokens = tokens.take(assignIndex)
    val target = if (targetTokens.contains("-")) {
      // Field access: structure-field
      parseFieldAccess(targetTokens, span)
    } else {
      // Simple identifier
      IdentifierExpr(targetTokens.head, span)
    }

    // Parse right side (value expression)
    val valueTokens = tokens.drop(assignIndex + 1).filterNot(_ == ".")
    val value = parseExpression(valueTokens, span)

    AssignmentStmt(target, value, span)
  }

  /** Parse an expression from tokens */
  private def parseExpression(tokens: Seq[String], span: TextSpan): AbapNode = {
    if (tokens.isEmpty) {
      return IdentifierExpr("", span)
    }

    // Check for indirection: object->*
    if (tokens.contains("->") && tokens.contains("*")) {
      val arrowIndex = tokens.indexOf("->")
      if (arrowIndex + 1 < tokens.length && tokens(arrowIndex + 1) == "*") {
        // Indirection operation
        val targetTokens = tokens.take(arrowIndex)
        val target = if (targetTokens.isEmpty) {
          IdentifierExpr("UNKNOWN", span)
        } else {
          IdentifierExpr(targetTokens.mkString(" "), span)
        }
        return OperatorCall(
          operatorName = "<operator>.indirection",
          arguments = Seq(target),
          span = span
        )
      }
    }

    // Check for method call (has => or ->)
    val hasStaticArrow = tokens.contains("=>")
    val hasInstanceArrow = tokens.contains("->")

    if (hasStaticArrow || hasInstanceArrow) {
      // This is a method call or field access expression
      parseCallStatement(tokens, "Call", span) match {
        case call: CallExpr => call
        case opCall: OperatorCall => opCall  // indirection or indirectFieldAccess
        case stmtList: StatementList =>
          // Chained call - for now, return the statement list
          stmtList
        case other => other
      }
    }
    // Check for function call: name(args)
    else if (tokens.contains("(") && tokens.contains(")")) {
      parseFunctionCall(tokens, span)
    }
    // Check for arithmetic operators
    else if (tokens.contains("+")) {
      parseArithmeticExpression(tokens, "+", "<operator>.addition", span)
    }
    else if (tokens.contains("-")) {
      parseArithmeticExpression(tokens, "-", "<operator>.subtraction", span)
    }
    else if (tokens.contains("*")) {
      parseArithmeticExpression(tokens, "*", "<operator>.multiplication", span)
    }
    else if (tokens.contains("/")) {
      parseArithmeticExpression(tokens, "/", "<operator>.division", span)
    }
    else if (tokens.contains("&&")) {
      parseArithmeticExpression(tokens, "&&", "<operator>.addition", span) // String concatenation
    }
    // Simple literal or identifier
    else if (tokens.length == 1) {
      val token = tokens.head
      // Check if it's a number
      if (token.matches("-?\\d+(\\.\\d+)?")) {
        LiteralExpr(token, "NUMBER", span)
      }
      // Check if it's a string literal
      else if (token.startsWith("'") || token.startsWith("`")) {
        LiteralExpr(token, "STRING", span)
      }
      // Otherwise it's an identifier
      else {
        IdentifierExpr(token, span)
      }
    }
    else {
      // Complex expression - for now, treat as unknown
      IdentifierExpr(tokens.mkString(" "), span)
    }
  }

  /** Parse function call: name(args) */
  private def parseFunctionCall(tokens: Seq[String], span: TextSpan): AbapNode = {
    // Find opening parenthesis
    val openParen = tokens.indexOf("(")
    if (openParen <= 0) {
      // Invalid function call
      return IdentifierExpr(tokens.mkString(" "), span)
    }

    // Function name is everything before (
    val functionName = tokens.take(openParen).mkString("")

    // Find matching closing parenthesis
    val closeParen = tokens.lastIndexOf(")")
    if (closeParen <= openParen) {
      // Invalid function call
      return IdentifierExpr(tokens.mkString(" "), span)
    }

    // Arguments are between ( and )
    val argTokens = tokens.slice(openParen + 1, closeParen)

    // Parse arguments (split by comma)
    val arguments = if (argTokens.isEmpty) {
      Seq.empty[Argument]
    } else {
      // For now, treat each comma-separated part as an unnamed argument
      val argParts = splitByComma(argTokens)
      argParts.map { argToks =>
        Argument(None, parseExpression(argToks, span))
      }
    }

    // For simple function calls (no object/class qualifier), use empty targetName
    // This way methodFullName will be just the function name
    CallExpr(
      targetName = "",  // No target object/class for simple calls
      methodName = Some(functionName),
      arguments = arguments,
      isStatic = false, // Could be static or instance, hard to tell without type info
      span = span
    )
  }

  /** Split tokens by comma (for argument parsing) */
  private def splitByComma(tokens: Seq[String]): Seq[Seq[String]] = {
    if (!tokens.contains(",")) {
      if (tokens.nonEmpty) Seq(tokens) else Seq.empty
    } else {
      val result = scala.collection.mutable.ArrayBuffer[Seq[String]]()
      var current = scala.collection.mutable.ArrayBuffer[String]()

      tokens.foreach { token =>
        if (token == ",") {
          if (current.nonEmpty) {
            result += current.toSeq
            current = scala.collection.mutable.ArrayBuffer[String]()
          }
        } else {
          current += token
        }
      }

      if (current.nonEmpty) {
        result += current.toSeq
      }

      result.toSeq
    }
  }

  /** Parse arithmetic expression (binary operator) */
  private def parseArithmeticExpression(tokens: Seq[String], operator: String, operatorName: String, span: TextSpan): AbapNode = {
    val opIndex = tokens.indexOf(operator)
    if (opIndex <= 0 || opIndex >= tokens.length - 1) {
      return IdentifierExpr(tokens.mkString(" "), span)
    }

    val leftTokens = tokens.take(opIndex)
    val rightTokens = tokens.drop(opIndex + 1)

    val leftExpr = parseExpression(leftTokens, span)
    val rightExpr = parseExpression(rightTokens, span)

    OperatorCall(operatorName, Seq(leftExpr, rightExpr), span)
  }

  /** Parse field access expression */
  private def parseFieldAccess(tokens: Seq[String], span: TextSpan): AbapNode = {
    val dashIndex = tokens.indexOf("-")
    if (dashIndex > 0 && dashIndex < tokens.length - 1) {
      val targetExpr = IdentifierExpr(tokens.take(dashIndex).mkString(" "), span)
      val fieldName = tokens.drop(dashIndex + 1).mkString(" ")
      FieldAccessExpr(targetExpr, fieldName, span)
    } else {
      IdentifierExpr(tokens.mkString(" "), span)
    }
  }

  /** Parse chained calls into multiple CallExpr nodes */
  private def parseChainedCalls(tokens: Seq[String], span: TextSpan): Seq[CallExpr] = {
    // Find all call operators (=> and ->)
    val arrowIndices = tokens.zipWithIndex.collect {
      case ("=>", idx) => (idx, true)  // static
      case ("->", idx) => (idx, false) // instance
    }

    arrowIndices.map { case (arrowIdx, isStatic) =>
      // Get the target (token before arrow)
      val targetName = if (arrowIdx > 0) {
        tokens(arrowIdx - 1)
      } else {
        "UNKNOWN"
      }

      // Get the method name (token after arrow)
      val methodName = if (arrowIdx + 1 < tokens.length) {
        tokens(arrowIdx + 1)
      } else {
        "UNKNOWN"
      }

      // For chained calls, if target is ")", use a placeholder
      val actualTarget = if (targetName == ")") {
        "<chained-result>"
      } else {
        targetName
      }

      CallExpr(
        targetName = actualTarget,
        methodName = Some(methodName),
        arguments = Seq.empty, // Simplified - not parsing args in chained calls for now
        isStatic = isStatic,
        span = span
      )
    }
  }

  /** Parse Call statement into CallExpr */
  private def parseCallStatement(tokens: Seq[String], stmtType: String, span: TextSpan): AbapNode = {
    // ABAP has two call operators:
    // => for static calls (class=>method)
    // -> for instance calls (object->method)
    // Calls can be chained: class=>get_instance()->method()

    // For now, take the LAST call in a chain (rightmost arrow)
    val instanceArrowIndex = tokens.lastIndexOf("->")
    val staticArrowIndex = tokens.lastIndexOf("=>")

    // Determine which arrow to use (prefer the rightmost one)
    val (arrowIndex, isStatic) = if (instanceArrowIndex > staticArrowIndex) {
      (instanceArrowIndex, false)
    } else if (staticArrowIndex >= 0) {
      (staticArrowIndex, true)
    } else {
      (-1, false)
    }

    if (arrowIndex > 0 && arrowIndex + 1 < tokens.length) {
      val targetName = tokens(arrowIndex - 1)

      // If target is ), this is actually a chained call (method()->next())
      // Use a placeholder for the chained result
      val actualTarget = if (targetName == ")") {
        "<chained-result>"
      } else {
        targetName
      }

      val nextToken = tokens(arrowIndex + 1)

      // Check if this is a dereference operation: object->*
      if (nextToken == "*") {
        // This is indirection (dereferencing)
        return OperatorCall(
          operatorName = "<operator>.indirection",
          arguments = Seq(IdentifierExpr(actualTarget, span)),
          span = span
        )
      }

      val methodName = nextToken

      // Check if this is a method call (has parentheses) or field access
      val parenIndex = tokens.indexWhere(_ == "(", arrowIndex + 1)
      val isMethodCall = parenIndex >= 0 && parenIndex == arrowIndex + 2

      if (!isMethodCall) {
        // This is field/attribute access through reference: object->attribute
        // Use indirectFieldAccess operator
        return OperatorCall(
          operatorName = "<operator>.indirectFieldAccess",
          arguments = Seq(
            IdentifierExpr(actualTarget, span),
            IdentifierExpr(methodName, span)
          ),
          span = span
        )
      }

      // Extract arguments (simple version - just collect tokens between parentheses)
      // Find parentheses AFTER the method name
      val methodNameIndex = arrowIndex + 1
      val parenStart = tokens.indexWhere(_ == "(", methodNameIndex)
      val parenEnd = if (parenStart >= 0) {
        tokens.indexWhere(_ == ")", parenStart)
      } else {
        -1
      }

      val args = if (parenStart >= 0 && parenEnd > parenStart) {
        val argTokens = tokens.slice(parenStart + 1, parenEnd)
        argTokens.filterNot(t => t.isEmpty || t == ",").map(argToken =>
          Argument(None, IdentifierExpr(argToken, span))
        )
      } else {
        Seq.empty
      }

      CallExpr(
        targetName = actualTarget,
        methodName = Some(methodName),
        arguments = args,
        isStatic = isStatic,
        span = span
      )
    }
    // Pattern 2: CALL FUNCTION 'name'
    else if (stmtType == "CallFunction") {
      // CALL FUNCTION 'FUNCTION_NAME'
      val functionName = tokens.find(t => t.startsWith("'") || t.startsWith("`")).getOrElse("UNKNOWN")
      CallExpr(
        targetName = functionName.stripPrefix("'").stripSuffix("'").stripPrefix("`").stripSuffix("`"),
        methodName = None,
        arguments = Seq.empty,
        isStatic = true,
        span = span
      )
    }
    // Pattern 3: Simple function call without arrows
    else {
      // Simple method call: method_name()
      val methodName = tokens.headOption.getOrElse("UNKNOWN")
      CallExpr(
        targetName = methodName,
        methodName = None,
        arguments = Seq.empty,
        isStatic = false,
        span = span
      )
    }
  }

}

object AbapJsonParser {
  def apply(): AbapJsonParser = new AbapJsonParser()
}
