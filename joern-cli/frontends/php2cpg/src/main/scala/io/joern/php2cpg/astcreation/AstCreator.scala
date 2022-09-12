package io.joern.php2cpg.astcreation

import io.joern.php2cpg.astcreation.AstCreator.TypeConstants
import io.joern.php2cpg.parser.Domain._
import io.joern.x2cpg.Ast.storeInDiffGraph
import io.joern.x2cpg.{Ast, AstCreatorBase}
import io.joern.x2cpg.utils.NodeBuilders.operatorCallNode
import io.shiftleft.codepropertygraph.generated.PropertyNames
import io.shiftleft.codepropertygraph.generated.{EvaluationStrategies, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{
  NewBlock,
  NewCall,
  NewIdentifier,
  NewLiteral,
  NewMethod,
  NewMethodParameterIn,
  NewTypeRef
}
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate

class AstCreator(filename: String, phpAst: PhpFile) extends AstCreatorBase(filename) {

  private val logger = LoggerFactory.getLogger(AstCreator.getClass)

  override def createAst(): BatchedUpdate.DiffGraphBuilder = {
    val ast = astForPhpFile(phpAst)
    storeInDiffGraph(ast, diffGraph)
    diffGraph
  }

  private def registerType(typ: String): String = {
    // TODO Actually register type
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

      case unhandled =>
        logger.warn(s"Unhandled stmt: $unhandled")
        ???
    }
  }

  private def astForEchoStmt(echoStmt: PhpEchoStmt): Ast = {
    val callNode = operatorCallNode("<operator>.echo", code = "echo", line = echoStmt.attributes.lineNumber)
    val args     = echoStmt.exprs.map(astForExpr)
    callAst(callNode, args)
  }

  private def astForMethodDecl(decl: PhpMethodDecl): Ast = {
    val methodNode =
      NewMethod()
        .name(decl.name)
        .code(decl.name)
        .lineNumber(decl.attributes.lineNumber)
        .isExternal(false)

    val parameters   = decl.params.map(astForParam)
    val methodBody   = astForMethodBody(decl.stmts, decl.attributes.lineNumber)
    val methodReturn = methodReturnNode(TypeConstants.Unresolved, line = decl.attributes.lineNumber, column = None)

    methodAstWithAnnotations(methodNode, parameters, methodBody, methodReturn)
  }

  private def astForMethodBody(stmts: Seq[PhpStmt], lineNumber: Option[Integer]): Ast = {
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
      .lineNumber(param.attributes.lineNumber)
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

      case unhandled =>
        logger.warn(s"Unhandled expr: $unhandled")
        ???
    }
  }

  private def astForFunctionCall(call: PhpFuncCall): Ast = {
    val name = call.name match {
      case PhpNameExpr(name, _) => name
      case complexExpr          =>
        // TODO Handle this properly
        complexExpr.toString
    }
    val callNode = NewCall()
      .name(name)
      .lineNumber(call.attributes.lineNumber)
    val arguments = call.args.map(astForCallArg)
    callAst(callNode, arguments)
  }

  private def astForCallArg(arg: PhpArgument): Ast = {
    arg match {
      case PhpArg(expr, _, _, _, _) =>
        astForExpr(expr)

      case PhpVariadicPlaceholder(attributes) =>
        val identifier =
          NewIdentifier()
            .name("...")
            .lineNumber(attributes.lineNumber)
            .code("...")
            .typeFullName("PhpVariadicPlaceholder")
        Ast(identifier)
    }
  }

  private def astForVariableExpr(variable: PhpVariable): Ast = {
    astForExpr(variable.value)
  }

  private def astForNameExpr(expr: PhpNameExpr): Ast = {
    val identifier = NewIdentifier()
      .name(expr.name)
      .code(expr.name)
      .lineNumber(expr.attributes.lineNumber)

    Ast(identifier)
  }

  private def astForAssignment(assignment: PhpAssignment): Ast = {
    val operatorName = assignment.assignOp
    val callNode     = operatorCallNode(operatorName, operatorName, line = assignment.attributes.lineNumber)

    val targetAst = astForExpr(assignment.target)
    val sourceAst = astForExpr(assignment.source)

    callAst(callNode, List(targetAst, sourceAst))
  }

  private def astForScalar(scalar: PhpScalar): Ast = {
    scalar match {
      case PhpString(value, attributes) =>
        Ast(NewLiteral().code(value).typeFullName(TypeConstants.String).lineNumber(attributes.lineNumber))
      case PhpInt(value, attributes) =>
        Ast(NewLiteral().code(value).typeFullName(TypeConstants.Int).lineNumber(attributes.lineNumber))
      case PhpFloat(value, attributes) =>
        Ast(NewLiteral().code(value).typeFullName(TypeConstants.Float).lineNumber(attributes.lineNumber))
      case PhpEncapsed(parts, attributes) =>
        val callNode =
          operatorCallNode(PhpOperators.encaps, code = /* TODO */ PhpOperators.encaps, line = attributes.lineNumber)
        val args = parts.map(astForExpr)
        callAst(callNode, args)
      case PhpEncapsedPart(value, attributes) =>
        Ast(NewLiteral().code(value).typeFullName(TypeConstants.String).lineNumber(attributes.lineNumber))

      case unhandled =>
        logger.warn(s"Unhandled scalar: $unhandled")
        ???
    }
  }

  private def astForBinOp(binOp: PhpBinaryOp): Ast = {
    val leftAst  = astForExpr(binOp.left)
    val rightAst = astForExpr(binOp.right)

    val callNode = operatorCallNode(
      binOp.operator,
      /* TODO CODE */ binOp.operator,
      line = binOp.attributes.lineNumber
    )

    callAst(callNode, List(leftAst, rightAst))
  }

  private def astForUnaryOp(unaryOp: PhpUnaryOp): Ast = {
    val exprAst = astForExpr(unaryOp.expr)

    val callNode = operatorCallNode(
      unaryOp.operator,
      /* TODO CODE */ unaryOp.operator,
      line = unaryOp.attributes.lineNumber
    )

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
      .lineNumber(castExpr.attributes.lineNumber)

    val expr    = astForExpr(castExpr.expr)
    val codeStr = s"(${castExpr.typ}) ${rootCode(expr)}"

    val callNode = operatorCallNode(name = Operators.cast, codeStr, Some(castExpr.typ), castExpr.attributes.lineNumber)

    callAst(callNode, Ast(typ) :: expr :: Nil)
  }

  private def astForIssetExpr(issetExpr: PhpIsset): Ast = {
    val args = issetExpr.vars.map(astForExpr)
    val code = s"isset(${args.map(rootCode(_).mkString(","))})"

    val callNode = operatorCallNode(
      PhpOperators.isset,
      code,
      typeFullName = Some(TypeConstants.Bool),
      line = issetExpr.attributes.lineNumber
    )

    callAst(callNode, args)
  }
}

object AstCreator {
  object TypeConstants {
    val String: String     = "string"
    val Int: String        = "int"
    val Float: String      = "float"
    val Bool: String       = "bool"
    val Unresolved: String = "codepropertygraph.Unresolved"
  }

}
