package io.joern.php2cpg.astcreation

import io.joern.php2cpg.astcreation.AstCreator.TypeConstants
import io.joern.php2cpg.parser.Domain._
import io.joern.x2cpg.Ast.storeInDiffGraph
import io.joern.x2cpg.{Ast, AstCreatorBase}
import io.joern.x2cpg.utils.NodeBuilders.operatorCallNode
import io.shiftleft.codepropertygraph.generated.{EvaluationStrategies, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{
  NewBlock,
  NewCall,
  NewIdentifier,
  NewLiteral,
  NewMethod,
  NewMethodParameterIn
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
      case functionCallExpr: PhpFuncCall => astForFunctionCall(functionCallExpr)
      case variableExpr: PhpVariable     => astForVariableExpr(variableExpr)
      case nameExpr: PhpNameExpr         => astForNameExpr(nameExpr)
      case assignExpr: PhpAssignment     => astForAssignment(assignExpr)
      case scalarExpr: PhpScalar         => astForScalar(scalarExpr)

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
      .lineNumber(expr.attributes.lineNumber)

    Ast(identifier)
  }

  private def astForAssignment(assignment: PhpAssignment): Ast = {
    val operatorName = assignment match {
      case _: PhpAssign => Operators.assignment
    }

    val callNode =
      operatorCallNode(operatorName, operatorName, line = assignment.attributes.lineNumber)

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
        val callNode = operatorCallNode("<operator>.encaps", code = "TODO", line = attributes.lineNumber)
        val args     = parts.map(astForExpr)
        callAst(callNode, args)
      case PhpEncapsedPart(value, attributes) =>
        Ast(NewLiteral().code(value).typeFullName(TypeConstants.String).lineNumber(attributes.lineNumber))

      case unhandled =>
        logger.warn(s"Unhandled scalar: $unhandled")
        ???
    }
  }
}

object AstCreator {
  object TypeConstants {
    val String: String     = "string"
    val Int: String        = "int"
    val Float: String      = "float"
    val Unresolved: String = "codepropertygraph.Unresolved"
  }

}
