package io.joern.php2cpg.astcreation

import io.joern.php2cpg.astcreation.AstCreator.{TypeConstants, operatorSymbols}
import io.joern.php2cpg.parser.Domain._
import io.joern.x2cpg.Ast.storeInDiffGraph
import io.joern.x2cpg.datastructures.Global
import io.joern.x2cpg.{Ast, AstCreatorBase}
import io.joern.x2cpg.utils.NodeBuilders.operatorCallNode
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes, EvaluationStrategies, Operators, PropertyNames}
import io.shiftleft.codepropertygraph.generated.nodes.{ExpressionNew, NewBlock, NewCall, NewControlStructure, NewIdentifier, NewJumpTarget, NewLiteral, NewMethod, NewMethodParameterIn, NewTypeRef}
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate

class AstCreator(filename: String, phpAst: PhpFile, global: Global) extends AstCreatorBase(filename) {

  private val logger = LoggerFactory.getLogger(AstCreator.getClass)

  override def createAst(): BatchedUpdate.DiffGraphBuilder = {
    val ast = astForPhpFile(phpAst)
    storeInDiffGraph(ast, diffGraph)
    diffGraph
  }

  private def registerType(typ: String): String = {
    global.usedTypes.putIfAbsent(typ, true)
    typ
  }

  private def astForPhpFile(file: PhpFile): Ast = {
    val namespaceBlock = globalNamespaceBlock().filename(absolutePath(filename))
    val children       = file.children.map(astForStmt)

    Ast(namespaceBlock).withChildren(children)
  }

  private def astForStmt(stmt: PhpStmt): Ast = {
    stmt match {
      case echoStmt: PhpEchoStmt     => astForEchoStmt(echoStmt)
      case methodDecl: PhpMethodDecl => astForMethodDecl(methodDecl)
      case expr: PhpExpr             => astForExpr(expr)
      case breakStmt: PhpBreakStmt   => astForBreakStmt(breakStmt)
      case contStmt: PhpContinueStmt => astForContinueStmt(contStmt)
      case whileStmt: PhpWhileStmt   => astForWhileStmt(whileStmt)
      case ifStmt: PhpIfStmt         => astForIfStmt(ifStmt)
      case switchStmt: PhpSwitchStmt => astForSwitchStmt(switchStmt)

      case unhandled =>
        logger.warn(s"Unhandled stmt: $unhandled")
        ???
    }
  }

  private def astForEchoStmt(echoStmt: PhpEchoStmt): Ast = {
    val callNode = operatorCallNode("<operator>.echo", code = "echo", line = line(echoStmt))
    val args     = echoStmt.exprs.map(astForExpr)
    callAst(callNode, args)
  }

  private def astForMethodDecl(decl: PhpMethodDecl): Ast = {
    val methodNode =
      NewMethod()
        .name(decl.name)
        .code(decl.name)
        .lineNumber(line(decl))
        .isExternal(false)

    val parameters   = decl.params.map(astForParam)
    val methodBody   = stmtBlockAst(decl.stmts, line(decl))
    val methodReturn = methodReturnNode(TypeConstants.Unresolved, line = line(decl), column = None)

    methodAstWithAnnotations(methodNode, parameters, methodBody, methodReturn)
  }

  private def stmtBlockAst(stmts: Seq[PhpStmt], lineNumber: Option[Integer]): Ast = {
    val bodyBlock    = NewBlock().lineNumber(lineNumber)
    val bodyStmtAsts = stmts.map(astForStmt)
    Ast(bodyBlock).withChildren(bodyStmtAsts)
  }

  private def astForParam(param: PhpParam): Ast = {
    val evaluationStrategy =
      if (param.byRef)
        EvaluationStrategies.BY_REFERENCE
      else
        EvaluationStrategies.BY_VALUE

    val paramNode = NewMethodParameterIn()
      .name(param.name)
      .code(param.name)
      .lineNumber(line(param))
      .isVariadic(param.isVariadic)
      .evaluationStrategy(evaluationStrategy)

    Ast(paramNode)
  }

  private def astForExpr(expr: PhpExpr): Ast = {
    expr match {
      case funcCallExpr: PhpFuncCall => astForFunctionCall(funcCallExpr)
      case variableExpr: PhpVariable => astForVariableExpr(variableExpr)
      case nameExpr: PhpNameExpr     => astForNameExpr(nameExpr)
      case assignExpr: PhpAssignment => astForAssignment(assignExpr)
      case scalarExpr: PhpScalar     => astForScalar(scalarExpr)
      case binaryOp: PhpBinaryOp     => astForBinOp(binaryOp)
      case unaryOp: PhpUnaryOp       => astForUnaryOp(unaryOp)
      case castExpr: PhpCast         => astForCastExpr(castExpr)
      case issetExpr: PhpIsset       => astForIssetExpr(issetExpr)
      case printExpr: PhpPrint       => astForPrintExpr(printExpr)
      case ternaryOp: PhpTernaryOp   => astForTernaryOp(ternaryOp)

      case unhandled =>
        logger.warn(s"Unhandled expr: $unhandled")
        ???
    }
  }

  private def intToLiteralAst(num: Int): Ast = {
    Ast(NewLiteral().code(num.toString).typeFullName(TypeConstants.Int))
  }

  private def astForBreakStmt(breakStmt: PhpBreakStmt): Ast = {
    val code = breakStmt.num.map(num => s"break($num)").getOrElse("break")
    val breakNode = NewControlStructure()
      .controlStructureType(ControlStructureTypes.BREAK)
      .code(code)
      .lineNumber(line(breakStmt))

    val argument = breakStmt.num.map(intToLiteralAst)

    controlStructureAst(breakNode, None, argument.toList)
  }

  private def astForContinueStmt(continueStmt: PhpContinueStmt): Ast = {
    val code = continueStmt.num.map(num => s"continue($num)").getOrElse("continue")
    val continueNode = NewControlStructure()
      .controlStructureType(ControlStructureTypes.CONTINUE)
      .code(code)
      .lineNumber(line(continueStmt))

    val argument = continueStmt.num.map(intToLiteralAst)

    controlStructureAst(continueNode, None, argument.toList)
  }

  private def astForWhileStmt(whileStmt: PhpWhileStmt): Ast = {
    val condition = astForExpr(whileStmt.cond)

    val whileNode = NewControlStructure()
      .controlStructureType(ControlStructureTypes.WHILE)
      .code(s"while (${rootCode(condition)})")
      .lineNumber(line(whileStmt))

    val body = stmtBlockAst(whileStmt.stmts, line(whileStmt))

    controlStructureAst(whileNode, Some(condition), List(body))
  }

  private def astForIfStmt(ifStmt: PhpIfStmt): Ast = {
    val condition = astForExpr(ifStmt.cond)

    val thenAst = stmtBlockAst(ifStmt.stmts, line(ifStmt))

    val elseAst = ifStmt.elseIfs match {
      case Nil => ifStmt.elseStmt.map(els => stmtBlockAst(els.stmts, line(els))).toList

      case elseIf :: rest =>
        val newIfStmt     = PhpIfStmt(elseIf.cond, elseIf.stmts, rest, ifStmt.elseStmt, elseIf.attributes)
        val wrappingBlock = Ast(NewBlock())
        wrappingBlock.withChild(astForIfStmt(newIfStmt)) :: Nil
    }

    val conditionCode = rootCode(condition)
    val ifNode = NewControlStructure()
      .controlStructureType(ControlStructureTypes.IF)
      .code(s"if ($conditionCode)")
      .lineNumber(line(ifStmt))

    controlStructureAst(ifNode, Some(condition), thenAst :: elseAst)
  }

  private def astForSwitchStmt(stmt: PhpSwitchStmt): Ast = {
    val conditionAst = astForExpr(stmt.condition)

    val switchNode = NewControlStructure()
      .controlStructureType(ControlStructureTypes.SWITCH)
      .code(s"switch (${rootCode(conditionAst)})")
      .lineNumber(line(stmt))

    val entryAsts = stmt.cases.flatMap(astsForSwitchCase)

    controlStructureAst(switchNode, Some(conditionAst), entryAsts)
  }

  private def astsForSwitchCase(caseStmt: PhpCaseStmt): List[Ast] = {
    val maybeConditionAst = caseStmt.condition.map(astForExpr)
    val jumpTarget = maybeConditionAst match {
      case Some(conditionAst) => NewJumpTarget().name("case").code(s"case ${rootCode(conditionAst)}")
      case None               => NewJumpTarget().name("default").code("default")
    }
    jumpTarget.lineNumber(line(caseStmt))

    val stmtAsts = caseStmt.stmts.map(astForStmt)

    Ast(jumpTarget) :: stmtAsts
  }

  private def astForFunctionCall(call: PhpFuncCall): Ast = {

    // TODO Static function calls?
    val targetAst = astForExpr(call.target)
    val arguments = call.args.map(astForCallArg)

    val targetCode = rootCode(targetAst)
    val argsCode = arguments.map(rootCode(_)).mkString(",")
    val code = s"$targetCode($argsCode)"

    // TODO Function name doesn't make sense in a lot of cases. Do we have some sort of
    //  `apply` operator? Use targetCode for now for best current results.
    val name = targetCode

    val callNode = NewCall()
      .name(name)
      .code(code)
      .dispatchType(DispatchTypes.DYNAMIC_DISPATCH /* TODO STATIC DISPATCH for Name targets? */)
      .lineNumber(line(call))

    callAst(callNode, arguments, receiver = Some(targetAst))
  }

  private def astForCallArg(arg: PhpArgument): Ast = {
    arg match {
      case PhpArg(expr, _, _, _, _) =>
        astForExpr(expr)

      case _: PhpVariadicPlaceholder =>
        val identifier =
          NewIdentifier()
            .name("...")
            .lineNumber(line(arg))
            .code("...")
            .typeFullName("PhpVariadicPlaceholder")
        Ast(identifier)
    }
  }

  private def astForVariableExpr(variable: PhpVariable): Ast = {
    // TODO Need to figure out variable variables. Maybe represent as some kind of call?
    val valueAst = astForExpr(variable.value)

    valueAst.root.collect { case root: ExpressionNew =>
      root.code = "$" + root.code
    }

    valueAst.root.collect { case root: NewIdentifier =>
      root.lineNumber = line(variable)
    }

    valueAst
  }

  private def astForNameExpr(expr: PhpNameExpr): Ast = {
    val identifier = NewIdentifier()
      .name(expr.name)
      .code(expr.name)
      .lineNumber(line(expr))

    Ast(identifier)
  }

  private def astForAssignment(assignment: PhpAssignment): Ast = {
    val operatorName = assignment.assignOp

    val targetAst = astForExpr(assignment.target)
    val sourceAst = astForExpr(assignment.source)

    // TODO Handle ref assigns properly (if needed).
    val refSymbol = if (assignment.isRefAssign) "&" else ""
    val symbol    = operatorSymbols.getOrElse(assignment.assignOp, assignment.assignOp)
    val code      = s"${rootCode(targetAst)} $symbol $refSymbol${rootCode(sourceAst)}"

    val callNode = operatorCallNode(operatorName, code, line = line(assignment))
    callAst(callNode, List(targetAst, sourceAst))
  }

  private def astForScalar(scalar: PhpScalar): Ast = {
    scalar match {
      case PhpString(value, _) =>
        Ast(NewLiteral().code(value).typeFullName(TypeConstants.String).lineNumber(line(scalar)))
      case PhpInt(value, _) =>
        Ast(NewLiteral().code(value).typeFullName(TypeConstants.Int).lineNumber(line(scalar)))
      case PhpFloat(value, _) =>
        Ast(NewLiteral().code(value).typeFullName(TypeConstants.Float).lineNumber(line(scalar)))
      case PhpEncapsed(parts, _) =>
        val callNode =
          operatorCallNode(PhpBuiltins.encaps, code = /* TODO */ PhpBuiltins.encaps, line = line(scalar))
        val args = parts.map(astForExpr)
        callAst(callNode, args)
      case PhpEncapsedPart(value, _) =>
        Ast(NewLiteral().code(value).typeFullName(TypeConstants.String).lineNumber(line(scalar)))

      case unhandled =>
        logger.warn(s"Unhandled scalar: $unhandled")
        ???
    }
  }

  private def astForBinOp(binOp: PhpBinaryOp): Ast = {
    val leftAst  = astForExpr(binOp.left)
    val rightAst = astForExpr(binOp.right)

    val symbol = operatorSymbols.getOrElse(binOp.operator, binOp.operator)
    val code   = s"${rootCode(leftAst)} ${symbol} ${rootCode(rightAst)}"

    val callNode = operatorCallNode(binOp.operator, code, line = line(binOp))

    callAst(callNode, List(leftAst, rightAst))
  }

  private def isPostfixOperator(operator: String): Boolean = {
    Set(Operators.postDecrement, Operators.postIncrement).contains(operator)
  }

  private def astForUnaryOp(unaryOp: PhpUnaryOp): Ast = {
    val exprAst = astForExpr(unaryOp.expr)

    val symbol = operatorSymbols.getOrElse(unaryOp.operator, unaryOp.operator)
    val code =
      if (isPostfixOperator(unaryOp.operator))
        s"${rootCode(exprAst)}$symbol"
      else
        s"$symbol${rootCode(exprAst)}"

    val callNode = operatorCallNode(unaryOp.operator, code, line = line(unaryOp))

    callAst(callNode, exprAst :: Nil)
  }

  // TODO Move to x2cpg
  private def rootCode(ast: Ast, default: String = ""): String = {
    ast.root.flatMap(_.properties.get(PropertyNames.CODE).map(_.toString)).getOrElse(default)
  }
  private def astForCastExpr(castExpr: PhpCast): Ast = {
    val typ = NewTypeRef()
      .typeFullName(registerType(castExpr.typ))
      .code(castExpr.typ)
      .lineNumber(line(castExpr))

    val expr    = astForExpr(castExpr.expr)
    val codeStr = s"(${castExpr.typ}) ${rootCode(expr)}"

    val callNode = operatorCallNode(name = Operators.cast, codeStr, Some(castExpr.typ), line(castExpr))

    callAst(callNode, Ast(typ) :: expr :: Nil)
  }

  private def astForIssetExpr(issetExpr: PhpIsset): Ast = {
    val args = issetExpr.vars.map(astForExpr)
    val code = s"${PhpBuiltins.issetFunc}(${args.map(rootCode(_).mkString(","))})"

    val callNode =
      operatorCallNode(PhpBuiltins.issetFunc, code, typeFullName = Some(TypeConstants.Bool), line = line(issetExpr))

    callAst(callNode, args)
  }
  private def astForPrintExpr(printExpr: PhpPrint): Ast = {
    val arg  = astForExpr(printExpr.expr)
    val code = s"${PhpBuiltins.printFunc}(${rootCode(arg)})"

    val callNode =
      operatorCallNode(PhpBuiltins.printFunc, code, typeFullName = Some(TypeConstants.Int), line = line(printExpr))

    callAst(callNode, arg :: Nil)
  }

  private def astForTernaryOp(ternaryOp: PhpTernaryOp): Ast = {
    val conditionAst = astForExpr(ternaryOp.condition)
    val maybeThenAst = ternaryOp.thenExpr.map(astForExpr)
    val elseAst      = astForExpr(ternaryOp.elseExpr)

    val operatorName = if (maybeThenAst.isDefined) Operators.conditional else PhpBuiltins.elvisOp
    val code = maybeThenAst match {
      case Some(thenAst) => s"${rootCode(conditionAst)} ? ${rootCode(thenAst)} : ${rootCode(elseAst)}"
      case None          => s"${rootCode(conditionAst)} ?: ${rootCode(elseAst)}"
    }

    val callNode = operatorCallNode(operatorName, code, line = line(ternaryOp))

    val args = List(Some(conditionAst), maybeThenAst, Some(elseAst)).flatten
    callAst(callNode, args)
  }

  private def line(phpNode: PhpNode): Option[Integer] = phpNode.attributes.lineNumber
}

object AstCreator {
  object TypeConstants {
    val String: String     = "string"
    val Int: String        = "int"
    val Float: String      = "float"
    val Bool: String       = "bool"
    val Unresolved: String = "codepropertygraph.Unresolved"
  }

  val operatorSymbols: Map[String, String] = Map(
    Operators.and                            -> "&",
    Operators.or                             -> "|",
    Operators.xor                            -> "^",
    Operators.logicalAnd                     -> "&&",
    Operators.logicalOr                      -> "||",
    PhpBuiltins.coalesceOp                   -> "??",
    PhpBuiltins.concatOp                     -> ".",
    Operators.division                       -> "/",
    Operators.equals                         -> "==",
    Operators.greaterEqualsThan              -> ">=",
    Operators.greaterThan                    -> ">",
    PhpBuiltins.identicalOp                  -> "===",
    PhpBuiltins.logicalXorOp                 -> "xor",
    Operators.minus                          -> "-",
    Operators.modulo                         -> "%",
    Operators.multiplication                 -> "*",
    Operators.notEquals                      -> "!=",
    PhpBuiltins.notIdenticalOp               -> "!==",
    Operators.plus                           -> "+",
    Operators.exponentiation                 -> "**",
    Operators.shiftLeft                      -> "<<",
    Operators.arithmeticShiftRight           -> ">>",
    Operators.lessEqualsThan                 -> "<=",
    Operators.lessThan                       -> "<",
    PhpBuiltins.spaceshipOp                  -> "<=>",
    Operators.not                            -> "~",
    Operators.logicalNot                     -> "!",
    Operators.postDecrement                  -> "--",
    Operators.postIncrement                  -> "++",
    Operators.preDecrement                   -> "--",
    Operators.preIncrement                   -> "++",
    Operators.minus                          -> "-",
    Operators.plus                           -> "+",
    Operators.assignment                     -> "=",
    Operators.assignmentAnd                  -> "&=",
    Operators.assignmentOr                   -> "|=",
    Operators.assignmentXor                  -> "^=",
    PhpBuiltins.assignmentCoalesceOp         -> "??=",
    PhpBuiltins.assignmentConcatOp           -> ".=",
    Operators.assignmentDivision             -> "/=",
    Operators.assignmentMinus                -> "-=",
    Operators.assignmentModulo               -> "%=",
    Operators.assignmentMultiplication       -> "*=",
    Operators.assignmentPlus                 -> "+=",
    Operators.assignmentExponentiation       -> "**=",
    Operators.assignmentShiftLeft            -> "<<=",
    Operators.assignmentArithmeticShiftRight -> ">>="
  )

}
