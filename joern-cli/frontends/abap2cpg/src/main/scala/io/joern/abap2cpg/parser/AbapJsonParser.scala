package io.joern.abap2cpg.parser

import io.joern.abap2cpg.parser.AbapIntermediateAst.*
import io.shiftleft.codepropertygraph.generated.Operators
import ujson.*

import java.nio.file.{Files, Path}
import scala.collection.mutable
import scala.util.Try

/** Parses JSON output from parse-abap.js into the ABAP intermediate AST.
  *
  * Input format: { file, objectType, statements: [{type, tokens:[{str}], start, end}] } All AST interpretation
  * (grouping into classes/methods, expression parsing) is done here in Scala. The JS parser is a thin wrapper that only
  * invokes @abaplint/core and dumps raw tokens.
  */
class AbapJsonParser {

  def parseFile(jsonPath: Path): Try[ProgramRoot] =
    Try {
      val json = ujson.read(new String(Files.readAllBytes(jsonPath)))
      parseProgram(json)
    }

  // ---------------------------------------------------------------------------
  // Top-level grouping
  // ---------------------------------------------------------------------------

  private def parseProgram(json: Value): ProgramRoot = {
    val fileName   = json("file").str
    val objectType = json("objectType").str
    val allStmts   = json("statements").arr.toSeq

    // --- state machine variables ---
    val classDefs = mutable.LinkedHashMap.empty[String, ClassDef] // name -> def (sigs only)
    val classSigs = mutable.ArrayBuffer.empty[MethodDef]          // sigs in current CLASS DEF
    val bodies    = mutable.HashMap.empty[String, StatementList]  // UPPER(name) -> body

    var currentClass: Option[String] = None
    var inClassDef                   = false

    var inMethod                   = false
    var methodName: Option[String] = None
    var methodSpan: TextSpan       = TextSpan()
    val methodBody                 = mutable.ArrayBuffer.empty[AbapNode]

    for (stmt <- allStmts) {
      val stmtType = stmt("type").str
      val tokens   = stmt("tokens").arr.map(_.obj("str").str).toSeq
      val span     = parseTextSpan(stmt)

      if (inMethod) {
        stmtType match {
          case "EndMethod" | "EndForm" | "EndFunction" =>
            methodName.foreach { name =>
              bodies(name.toUpperCase) = StatementList(methodBody.toSeq, methodSpan)
            }
            inMethod = false; methodName = None; methodBody.clear()
          case _ =>
            methodBody += parseBodyStatement(stmtType, tokens, span)
        }
      } else {
        stmtType match {
          case "ClassDefinition" =>
            val idx = tokens.indexWhere(_.equalsIgnoreCase("CLASS"))
            currentClass = if (idx + 1 < tokens.size) Some(tokens(idx + 1)) else None
            classSigs.clear()
            inClassDef = true

          case "ClassImplementation" =>
            // Seal the current definition if we were in one
            if (inClassDef) currentClass.foreach(sealClassDef(_, span, classSigs, classDefs))
            val idx = tokens.indexWhere(_.equalsIgnoreCase("CLASS"))
            currentClass = if (idx + 1 < tokens.size) Some(tokens(idx + 1)) else None
            classSigs.clear()
            inClassDef = false

          case "EndClass" =>
            if (inClassDef) currentClass.foreach(sealClassDef(_, span, classSigs, classDefs))
            // If we're in an implementation section the class def was already sealed above
            inClassDef = false
            currentClass = None
            classSigs.clear()

          case "MethodDef" | "ClassMethod" if inClassDef =>
            classSigs += parseMethodSignature(tokens, span)

          case "MethodImplementation" =>
            val idx = tokens.indexWhere(_.equalsIgnoreCase("METHOD"))
            methodName = if (idx + 1 < tokens.size) Some(tokens(idx + 1)) else None
            methodSpan = span; inMethod = true

          case "Form" =>
            val idx = tokens.indexWhere(_.equalsIgnoreCase("FORM"))
            methodName = if (idx + 1 < tokens.size) Some(tokens(idx + 1)) else None
            methodSpan = span; inMethod = true

          case "Function" =>
            val idx = tokens.indexWhere(_.equalsIgnoreCase("FUNCTION"))
            methodName = if (idx + 1 < tokens.size) Some(tokens(idx + 1)) else None
            methodSpan = span; inMethod = true

          case _ => // top-level declarations (ALIASES, INTERFACES, etc.) — ignore
        }
      }
    }

    // Merge method bodies into class definitions
    val mergedClasses = classDefs.values.map { cls =>
      val merged = cls.methods.map { sig =>
        bodies.get(sig.name.toUpperCase) match {
          case Some(body) => sig.copy(body = Some(body))
          case None       => sig
        }
      }
      cls.copy(methods = merged)
    }.toSeq

    // Standalone methods (FORMs / functions not inside a class)
    val classMethodNames = classDefs.values.flatMap(_.methods.map(_.name.toUpperCase)).toSet
    val standaloneNames  = bodies.keySet.filterNot(classMethodNames.contains)
    val standaloneMethods = standaloneNames.toSeq.map { name =>
      val body = bodies(name)
      MethodDef(
        name = name,
        visibility = None,
        isStatic = false,
        parameters = MethodParameters(),
        body = Some(body),
        span = body.span
      )
    }

    ProgramRoot(
      fileName,
      objectType,
      classes = mergedClasses,
      methods = standaloneMethods,
      span = TextSpan(code = fileName)
    )
  }

  private def sealClassDef(
    name: String,
    span: TextSpan,
    sigs: mutable.ArrayBuffer[MethodDef],
    out: mutable.LinkedHashMap[String, ClassDef]
  ): Unit = {
    val existing = out.get(name.toUpperCase)
    existing match {
      case Some(cls) =>
        // Merge additional sigs into existing class (e.g. PUBLIC then PRIVATE sections)
        out(name.toUpperCase) = cls.copy(methods = cls.methods ++ sigs.toSeq)
      case None =>
        out(name.toUpperCase) = ClassDef(name = name, visibility = "PUBLIC", methods = sigs.toSeq, span = span)
    }
  }

  // ---------------------------------------------------------------------------
  // Method signature parsing (from METHODS / CLASS-METHODS token stream)
  // ---------------------------------------------------------------------------

  private val parameterKeywords = Set(
    "IMPORTING",
    "EXPORTING",
    "CHANGING",
    "RETURNING",
    "RAISING",
    "EXCEPTIONS",
    "TYPE",
    "REF",
    "TO",
    "VALUE",
    "DEFAULT",
    "OPTIONAL",
    "LENGTH",
    "DECIMALS",
    "LIKE",
    "STRUCTURE",
    "TABLE"
  )

  private def isParameterKeyword(t: String): Boolean = parameterKeywords.contains(t.toUpperCase)

  private def parseMethodSignature(tokens: Seq[String], span: TextSpan): MethodDef = {
    val upper = tokens.map(_.toUpperCase)

    // Method name is the token after METHODS or CLASS-METHODS
    val kwIdx      = upper.indexWhere(t => t == "METHODS" || t == "CLASS-METHODS")
    val isStatic   = kwIdx >= 0 && upper(kwIdx) == "CLASS-METHODS"
    val methodName = if (kwIdx + 1 < tokens.size) tokens(kwIdx + 1) else "unknown"

    var importing                    = Seq[Parameter]()
    var exporting                    = Seq[Parameter]()
    var changing                     = Seq[Parameter]()
    var returning: Option[Parameter] = None
    var section                      = ""
    var i                            = kwIdx + 2

    while (i < upper.size) {
      upper(i) match {
        case "IMPORTING" => section = "IMPORTING"; i += 1
        case "EXPORTING" => section = "EXPORTING"; i += 1
        case "CHANGING"  => section = "CHANGING"; i += 1
        case "RETURNING" =>
          section = "RETURNING"; i += 1
          if (i < upper.size && upper(i) == "VALUE") i += 1
          if (i < upper.size && upper(i) == "(") i += 1
        case t if isParameterKeyword(t) || t == "." || t == "(" || t == ")" || t == ":" =>
          i += 1
        case _ if section.nonEmpty =>
          val pname = tokens(i); i += 1
          // Skip optional VALUE( wrapper (already consumed for RETURNING by the RETURNING case above,
          // but present here for CHANGING VALUE(p) / IMPORTING VALUE(p) patterns)
          if (i < upper.size && upper(i) == "VALUE") i += 1
          if (i < upper.size && upper(i) == "(") i += 1
          // Read TYPE typename — may appear before or after closing paren
          if (i < upper.size && upper(i) == "TYPE") i += 1
          def readType(): String =
            if (i < upper.size && upper(i) == "REF") {
              i += 1
              if (i < upper.size && upper(i) == "TO") i += 1
              if (i < upper.size) { val t = tokens(i); i += 1; t }
              else "ANY"
            } else if (
              i < upper.size && !isParameterKeyword(upper(i)) &&
              upper(i) != "." && upper(i) != ")"
            ) {
              val t = tokens(i); i += 1; t
            } else "ANY"
          var ptype = readType()
          while (
            i < upper.size && (upper(i) == "OPTIONAL" || upper(i) == "DEFAULT" ||
              upper(i) == "LENGTH" || upper(i) == "DECIMALS")
          ) {
            i += 1
            if (i < upper.size && !isParameterKeyword(upper(i)) && upper(i) != ".") i += 1
          }
          // Consume closing paren, then read TYPE typename that follows it.
          // e.g. RETURNING VALUE(rv) TYPE t  or  IMPORTING VALUE(p) TYPE t
          if (i < upper.size && upper(i) == ")") i += 1
          if (i < upper.size && upper(i) == "TYPE") {
            i += 1
            val t = readType()
            if (ptype == "ANY") ptype = t
          }
          val param = Parameter(name = pname, typeName = ptype, isValue = section == "RETURNING")
          section match {
            case "IMPORTING" => importing :+= param
            case "EXPORTING" => exporting :+= param
            case "CHANGING"  => changing :+= param
            case "RETURNING" => returning = Some(param)
            case _           => ()
          }
        case _ => i += 1
      }
    }

    MethodDef(
      name = methodName,
      isStatic = isStatic,
      visibility = None,
      parameters = MethodParameters(importing, exporting, changing, returning),
      body = None,
      span = span
    )
  }

  // ---------------------------------------------------------------------------
  // Body statement dispatch
  // ---------------------------------------------------------------------------

  private def parseBodyStatement(stmtType: String, tokens: Seq[String], span: TextSpan): AbapNode =
    stmtType match {
      case "Move"         => parseMoveStatement(tokens, span)
      case "Assign"       => parseAssignStatement(tokens, span)
      case "Call"         => parseCallStatement(tokens, "Call", span)
      case "CallFunction" => parseCallStatement(tokens, "CallFunction", span)
      case "Data"         => parseDataDeclaration(tokens, span)

      // ABAP-specific statements mapped to named CallExpr nodes so they are
      // visible as CALL nodes in the CPG and reachable by dataflow queries.

      case "OpenDataset" =>
        // OPEN DATASET <file> [FOR ...] [FILTER <cmd>]
        val fileArg   = tokens.lift(2).map(f => Argument(Some("FILENAME"), IdentifierExpr(f, span)))
        val filterIdx = tokens.indexWhere(_.equalsIgnoreCase("FILTER"))
        val filterArg =
          if (filterIdx >= 0)
            tokens
              .lift(filterIdx + 1)
              .map(c => Argument(Some("FILTER"), IdentifierExpr(c, span)))
          else None
        CallExpr("OPEN_DATASET", None, fileArg.toSeq ++ filterArg.toSeq, isStatic = true, span = span)

      case "ReadDataset" =>
        // READ DATASET <file> INTO <var>
        val fileArg = tokens.lift(2).map(f => Argument(Some("FILENAME"), IdentifierExpr(f, span)))
        CallExpr("READ_DATASET", None, fileArg.toSeq, isStatic = true, span = span)

      case "DeleteDataset" =>
        // DELETE DATASET <file>
        val fileArg = tokens.lift(2).map(f => Argument(Some("FILENAME"), IdentifierExpr(f, span)))
        CallExpr("DELETE_DATASET", None, fileArg.toSeq, isStatic = true, span = span)

      case "Transfer" =>
        // TRANSFER <data> TO <file>
        val toIdx = tokens.indexWhere(_.equalsIgnoreCase("TO"))
        val fileArg =
          if (toIdx >= 0)
            tokens
              .lift(toIdx + 1)
              .map(f => Argument(Some("TO"), IdentifierExpr(f, span)))
          else None
        val dataArg = tokens.lift(1).map(d => Argument(Some("DATA"), IdentifierExpr(d, span)))
        CallExpr("TRANSFER", None, dataArg.toSeq ++ fileArg.toSeq, isStatic = true, span = span)

      case "AuthorityCheck" =>
        // AUTHORITY-CHECK OBJECT '<obj>' ID '<field>' FIELD <var> ...
        // Tokens: ["AUTHORITY", "-", "CHECK", "OBJECT", "'obj'", "ID", ...]
        val objIdx = tokens.indexWhere(_.equalsIgnoreCase("OBJECT"))
        val objArg =
          if (objIdx >= 0)
            tokens
              .lift(objIdx + 1)
              .map(o => Argument(Some("OBJECT"), LiteralExpr(o, "STRING", span)))
          else None
        // Collect all FIELD <var> pairs
        val fieldArgs = tokens.zipWithIndex.collect {
          case ("FIELD", i) if i + 1 < tokens.size =>
            Argument(Some("FIELD"), IdentifierExpr(tokens(i + 1), span))
        }
        CallExpr("AUTHORITY_CHECK", None, objArg.toSeq ++ fieldArgs, isStatic = true, span = span)

      case "GenerateSubroutine" =>
        // GENERATE SUBROUTINE POOL <code_var> NAME <prog_var>
        val codeArg = tokens.lift(3).map(v => Argument(Some("POOL"), IdentifierExpr(v, span)))
        val nameIdx = tokens.indexWhere(_.equalsIgnoreCase("NAME"))
        val nameArg =
          if (nameIdx >= 0)
            tokens
              .lift(nameIdx + 1)
              .map(v => Argument(Some("NAME"), IdentifierExpr(v, span)))
          else None
        CallExpr("GENERATE_SUBROUTINE_POOL", None, codeArg.toSeq ++ nameArg.toSeq, isStatic = true, span = span)

      case "CallTransformation" =>
        // CALL TRANSFORMATION <name> SOURCE ... RESULT ...
        val nameArg = tokens.lift(2).map(n => Argument(None, LiteralExpr(n, "STRING", span)))
        CallExpr("CALL_TRANSFORMATION", None, nameArg.toSeq, isStatic = true, span = span)

      case "EditorCall" =>
        // EDITOR-CALL FOR REPORT <prog>  — tokens: ["EDITOR", "-", "CALL", "FOR", "REPORT", <prog>]
        val progArg = tokens.lift(5).map(p => Argument(Some("REPORT"), IdentifierExpr(p, span)))
        CallExpr("EDITOR_CALL", None, progArg.toSeq, isStatic = true, span = span)

      case "Do" =>
        // DO <n> TIMES  — <n> can be a literal or variable
        val timesIdx = tokens.indexWhere(_.equalsIgnoreCase("TIMES"))
        val countArg = if (timesIdx > 0) {
          val raw = tokens(timesIdx - 1)
          val expr =
            if (raw.matches("\\d+")) LiteralExpr(raw, "NUMBER", span)
            else IdentifierExpr(raw, span)
          Some(Argument(Some("TIMES"), expr))
        } else None
        CallExpr("DO_TIMES", None, countArg.toSeq, isStatic = true, span = span)

      case "Unknown" =>
        // abaplint emits Unknown for e.g. CALL FUNCTION 'SYSTEM' ID 'COMMAND' FIELD <var>
        // which is syntactically invalid ABAP but still used. Recover as a CallFunction.
        if (
          tokens.headOption.exists(_.equalsIgnoreCase("CALL")) &&
          tokens.lift(1).exists(_.equalsIgnoreCase("FUNCTION"))
        )
          parseCallStatement(tokens, "CallFunction", span)
        else
          UnknownNode(stmtType, span)

      case _ => UnknownNode(stmtType, span)
    }

  // ---------------------------------------------------------------------------
  // Text span from a statement JSON object
  // ---------------------------------------------------------------------------

  private def parseTextSpan(stmt: Value): TextSpan = {
    val start = stmt.obj.get("start").map(s => Position(row = s("row").num.toInt, col = s("col").num.toInt))
    val end   = stmt.obj.get("end").map(e => Position(row = e("row").num.toInt, col = e("col").num.toInt))
    val code  = stmt.obj.get("tokens").map(_.arr.map(_.obj("str").str).mkString(" ")).getOrElse("")
    TextSpan(start, end, code)
  }

  // ---------------------------------------------------------------------------
  // Token-stream expression parsers (unchanged from original)
  // ---------------------------------------------------------------------------

  private val abapConstructorOperators: Map[String, String] = Map(
    "NEW"           -> "<operator>.new",
    "VALUE"         -> "<operator>.valueConstruct",
    "REDUCE"        -> "<operator>.reduce",
    "COND"          -> "<operator>.conditional",
    "FILTER"        -> "<operator>.filter",
    "CORRESPONDING" -> "<operator>.corresponding",
    "CAST"          -> "<operator>.cast",
    "CONV"          -> "<operator>.conv",
    "SWITCH"        -> "<operator>.switch",
    "EXACT"         -> "<operator>.exact"
  )

  private val abapConstructorKeywords: Set[String] = abapConstructorOperators.keySet

  /** Normalize a JSON call target that starts with a constructor keyword. e.g. "NEW memoryfile" → ("",
    * Some("<operator>.new")) "NEW lexer( )" + method "run" → ("<chained-result>", Some("run"))
    */
  private def normalizeCallTarget(target: String, method: Option[String]): (String, Option[String]) = {
    val firstWord = target.takeWhile(c => c.isLetter || c == '_')
    if (firstWord.nonEmpty && abapConstructorKeywords.contains(firstWord) && target.length > firstWord.length) {
      method match {
        case None =>
          val op = abapConstructorOperators.getOrElse(firstWord, s"<operator>.${firstWord.toLowerCase}")
          ("", Some(op))
        case Some(_) =>
          ("<chained-result>", method)
      }
    } else {
      (target, method)
    }
  }

  private val abapConstructorBodyKeywords = Set(
    "LET",
    "IN",
    "INIT",
    "NEXT",
    "WHEN",
    "FOR",
    "WHILE",
    "UNTIL",
    "FROM",
    "STEP",
    "WHERE",
    "GROUP",
    "BASE",
    "THEN",
    "ELSE",
    "USING"
  )

  private def parseAssignStatement(tokens: Seq[String], span: TextSpan): AbapNode = {
    val toIndex = tokens.indexWhere(_.equalsIgnoreCase("TO"))
    if (toIndex <= 1 || toIndex >= tokens.length - 1) return UnknownNode("Assign", span)
    val source     = parseExpression(tokens.slice(1, toIndex), span)
    val targetToks = tokens.drop(toIndex + 1).filterNot(_ == ".")
    val target =
      if (targetToks.isEmpty) IdentifierExpr("", span)
      else if (targetToks.head.startsWith("<") && targetToks.head.endsWith(">"))
        IdentifierExpr(targetToks.head, span)
      else if (targetToks.contains("-")) parseFieldAccess(targetToks, span)
      else IdentifierExpr(targetToks.head, span)
    AssignmentStmt(target, source, span)
  }

  private def parseDataDeclaration(tokens: Seq[String], span: TextSpan): AbapNode = {
    if (tokens.length < 4) return UnknownNode("Data", span)
    val nameIdx = if (tokens.length > 1 && tokens(1) == ":") 2 else 1
    val varName = tokens(nameIdx)
    val typeIdx = tokens.indexWhere(_.equalsIgnoreCase("TYPE"))
    if (typeIdx < 0 || typeIdx >= tokens.length - 1) return UnknownNode("Data", span)
    val typeName = tokens(typeIdx + 1)
    val valueIdx = tokens.indexWhere(_.equalsIgnoreCase("VALUE"))
    val initialValue = if (valueIdx > 0 && valueIdx < tokens.length - 1) {
      val vToks = tokens.drop(valueIdx + 1).takeWhile(_ != ".")
      if (vToks.nonEmpty) Some(parseExpression(vToks, span)) else None
    } else None
    DataDeclaration(varName, typeName, initialValue, span)
  }

  private def parseMoveStatement(tokens: Seq[String], span: TextSpan): AbapNode = {
    val assignIdx = tokens.indexOf("=")
    if (assignIdx <= 0 || assignIdx >= tokens.length - 1) return UnknownNode("Move", span)
    val targetToks = tokens.take(assignIdx)
    val value      = parseExpression(tokens.drop(assignIdx + 1).filterNot(_ == "."), span)

    // Inline DATA declaration: DATA(varName) = expr
    // Represents as DataDeclaration with initialValue so astForStatements creates LOCAL + assignment.
    if (targetToks.headOption.exists(_.equalsIgnoreCase("DATA")) && targetToks.lift(1).contains("(")) {
      val varName = targetToks.drop(2).takeWhile(_ != ")").headOption.getOrElse("UNKNOWN")
      return DataDeclaration(varName, "ANY", Some(value), span)
    }

    val target =
      if (targetToks.contains("->") || targetToks.contains("=>"))
        parseCallStatement(targetToks, "Call", span)
      else if (targetToks.contains("-")) parseFieldAccess(targetToks, span)
      else IdentifierExpr(targetToks.head, span)
    AssignmentStmt(target, value, span)
  }

  private def parseExpression(tokens: Seq[String], span: TextSpan): AbapNode = {
    if (tokens.isEmpty) return IdentifierExpr("", span)

    if (tokens.contains("->") && tokens.contains("*")) {
      val arrowIdx = tokens.indexOf("->")
      if (arrowIdx + 1 < tokens.length && tokens(arrowIdx + 1) == "*") {
        val target = IdentifierExpr(tokens.take(arrowIdx).mkString(" "), span)
        return OperatorCall(Operators.indirection, Seq(target), span)
      }
    }

    if (tokens.contains("=>") || tokens.contains("->"))
      return parseCallStatement(tokens, "Call", span) match {
        case c: CallExpr     => c
        case o: OperatorCall => o
        case other           => other
      }

    if (tokens.contains("(") && tokens.contains(")"))
      return parseFunctionCall(tokens, span)

    if (tokens.contains("-") && !tokens.contains("+") && !tokens.contains("*") && !tokens.contains("/")) {
      val di = tokens.indexOf("-")
      val lt = tokens.take(di); val rt = tokens.drop(di + 1)
      if (
        lt.size == 1 && rt.size == 1 &&
        lt.head.forall(c => c.isLetterOrDigit || c == '_') &&
        rt.head.forall(c => c.isLetterOrDigit || c == '_')
      )
        return FieldAccessExpr(IdentifierExpr(lt.head, span), rt.head, span)
      return parseArithmeticExpression(tokens, "-", Operators.subtraction, span)
    }

    if (tokens.contains("+")) return parseArithmeticExpression(tokens, "+", Operators.addition, span)
    if (tokens.contains("*")) return parseArithmeticExpression(tokens, "*", Operators.multiplication, span)
    if (tokens.contains("/")) return parseArithmeticExpression(tokens, "/", Operators.division, span)
    if (tokens.contains("&&")) return parseArithmeticExpression(tokens, "&&", Operators.addition, span)

    if (tokens.length == 1) {
      val t = tokens.head
      if (t.matches("-?\\d+(\\.\\d+)?")) return LiteralExpr(t, "NUMBER", span)
      if (t.startsWith("'") || t.startsWith("`") || t.startsWith("|")) return LiteralExpr(t, "STRING", span)
      return IdentifierExpr(t, span)
    }

    UnknownNode("complex-expression", span)
  }

  private def parseFunctionCall(tokens: Seq[String], span: TextSpan): AbapNode = {
    val openParen = tokens.indexOf("(")
    if (openParen <= 0) return IdentifierExpr(tokens.mkString(" "), span)
    val before = tokens.take(openParen)
    if (before.exists(abapConstructorBodyKeywords.contains)) return UnknownNode("constructor-body-expr", span)

    val functionName = if (before.length == 1) {
      val n = before.head; if (n == "#") return UnknownNode("constructor-expr", span); n.stripSuffix("#")
    } else if (before.headOption.exists(t => abapConstructorKeywords.contains(t.toUpperCase))) {
      abapConstructorOperators.getOrElse(before.head.toUpperCase, before.head)
    } else if (before.last == "#") {
      before.dropRight(1).lastOption.getOrElse("UNKNOWN")
    } else if (before.length <= 2) {
      val last = before.last
      if (last.forall(c => c.isLetterOrDigit || c == '_')) last
      else return UnknownNode("complex-expr", span)
    } else return UnknownNode("complex-expr", span)

    val closeParen = tokens.lastIndexOf(")")
    if (closeParen <= openParen) return IdentifierExpr(tokens.mkString(" "), span)
    val argToks = tokens.slice(openParen + 1, closeParen)
    val arguments =
      if (argToks.isEmpty) Seq.empty[Argument]
      else
        splitByComma(argToks).map { argChunk =>
          // Check for named parameter syntax: param = value
          val eqIdx = argChunk.indexOf("=")
          if (eqIdx > 0 && eqIdx < argChunk.length - 1) {
            // Named parameter
            val paramName = argChunk.take(eqIdx).mkString(" ").trim
            val valueExpr = parseExpression(argChunk.drop(eqIdx + 1), span)
            Argument(Some(paramName), valueExpr)
          } else {
            // Positional parameter
            Argument(None, parseExpression(argChunk, span))
          }
        }

    CallExpr(targetName = "", methodName = Some(functionName), arguments = arguments, isStatic = false, span = span)
  }

  private def splitByComma(tokens: Seq[String]): Seq[Seq[String]] = {
    if (!tokens.contains(",")) { if (tokens.nonEmpty) Seq(tokens) else Seq.empty }
    else {
      val result  = mutable.ArrayBuffer[Seq[String]]()
      val current = mutable.ArrayBuffer[String]()
      tokens.foreach {
        case "," => if (current.nonEmpty) { result += current.toSeq; current.clear() }
        case t   => current += t
      }
      if (current.nonEmpty) result += current.toSeq
      result.toSeq
    }
  }

  private def parseArithmeticExpression(tokens: Seq[String], op: String, name: String, span: TextSpan): AbapNode = {
    val idx = tokens.indexOf(op)
    if (idx <= 0 || idx >= tokens.length - 1) return IdentifierExpr(tokens.mkString(" "), span)
    OperatorCall(name, Seq(parseExpression(tokens.take(idx), span), parseExpression(tokens.drop(idx + 1), span)), span)
  }

  private def parseFieldAccess(tokens: Seq[String], span: TextSpan): AbapNode = {
    val di = tokens.indexOf("-")
    if (di > 0 && di < tokens.length - 1)
      FieldAccessExpr(IdentifierExpr(tokens.take(di).mkString(" "), span), tokens.drop(di + 1).mkString(" "), span)
    else IdentifierExpr(tokens.mkString(" "), span)
  }

  private def parseCallStatement(tokens: Seq[String], stmtType: String, span: TextSpan): AbapNode = {
    val instIdx = tokens.lastIndexOf("->")
    val statIdx = tokens.lastIndexOf("=>")
    val (arrowIdx, isStatic) =
      if (instIdx > statIdx) (instIdx, false)
      else if (statIdx >= 0) (statIdx, true)
      else (-1, false)

    if (arrowIdx > 0 && arrowIdx + 1 < tokens.length) {
      val rawTarget    = tokens(arrowIdx - 1)
      val actualTarget = if (rawTarget == ")") "<chained-result>" else rawTarget
      val nextTok      = tokens(arrowIdx + 1)

      if (nextTok == "*")
        return OperatorCall(Operators.indirection, Seq(IdentifierExpr(actualTarget, span)), span)

      val parenIdx     = tokens.indexWhere(_ == "(", arrowIdx + 1)
      val isMethodCall = parenIdx >= 0 && parenIdx == arrowIdx + 2

      if (!isMethodCall)
        return OperatorCall(
          Operators.indirectFieldAccess,
          Seq(IdentifierExpr(actualTarget, span), IdentifierExpr(nextTok, span)),
          span
        )

      val parenStart = tokens.indexWhere(_ == "(", arrowIdx + 1)
      val parenEnd   = if (parenStart >= 0) tokens.indexWhere(_ == ")", parenStart) else -1
      val args = if (parenStart >= 0 && parenEnd > parenStart) {
        val argToks = tokens.slice(parenStart + 1, parenEnd)
        // Split by comma and check for named parameter syntax (param = value)
        splitByComma(argToks).map { argChunk =>
          val eqIdx = argChunk.indexOf("=")
          if (eqIdx > 0 && eqIdx < argChunk.length - 1) {
            // Named parameter
            val paramName = argChunk.take(eqIdx).mkString(" ").trim
            val valueExpr = parseExpression(argChunk.drop(eqIdx + 1), span)
            Argument(Some(paramName), valueExpr)
          } else {
            // Positional parameter
            Argument(None, parseExpression(argChunk, span))
          }
        }
      } else Seq.empty

      val (target, method) = normalizeCallTarget(actualTarget, Some(nextTok))
      return CallExpr(targetName = target, methodName = method, arguments = args, isStatic = isStatic, span = span)
    }

    if (stmtType == "CallFunction") {
      val fn = tokens.find(t => t.startsWith("'") || t.startsWith("`")).getOrElse("UNKNOWN")
      return CallExpr(
        targetName = fn.stripPrefix("'").stripSuffix("'").stripPrefix("`").stripSuffix("`"),
        methodName = None,
        arguments = Seq.empty,
        isStatic = true,
        span = span
      )
    }

    val first = tokens.headOption.getOrElse("UNKNOWN")
    val name = abapConstructorOperators.getOrElse(
      first.toUpperCase,
      if (first.forall(c => c.isLetterOrDigit || c == '_' || c == '#' || c == '~')) first
      else "UNKNOWN"
    )
    CallExpr(targetName = name, methodName = None, arguments = Seq.empty, isStatic = false, span = span)
  }
}

object AbapJsonParser {
  def apply(): AbapJsonParser = new AbapJsonParser()
}
