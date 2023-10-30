package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.parser.ParserAst
import io.joern.rubysrc2cpg.parser.ParserAst.*
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies

trait AstForFunctionsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def astForMethodDeclaration(node: MethodDeclaration): Ast = {
    // TODO: body could be a try
    val method = methodNode(
      node = node,
      name = node.methodName,
      fullName = computeMethodFullName(node.methodName),
      code = node.text,
      signature = None,
      fileName = relativeFileName
    )
    methodAstParentStack.push(method)

    val parameterAsts = node.parameters.zipWithIndex.map { case (parameterCtx, index) =>
      astForParameter(ParserAst(parameterCtx), index)
    }

    val stmtBlockAst = ParserAst(node.body) match
      case stmtList: StatementList => astForStatementListReturningLastExpression(stmtList)
      case _: StaticLiteral =>
        astForStatementListReturningLastExpression(StatementList(node.body, List(node.body)))
      case _: BinaryExpression =>
        astForStatementListReturningLastExpression(StatementList(node.body, List(node.body)))
      case _: SingleAssignment =>
        astForStatementListReturningLastExpression(StatementList(node.body, List(node.body)))
      case body =>
        logger.warn(
          s"Non-linear method bodies are not supported yet: ${body.text} (${body.getClass.getSimpleName}) ($relativeFileName), skipping"
        )
        astForUnknown(body)

    methodAstParentStack.pop()
    methodAst(method, parameterAsts, stmtBlockAst, methodReturnNode(node, Defines.Any))
  }

  // TODO: remaining cases
  protected def astForParameter(node: ParserNode, index: Int): Ast = {
    node match
      case node: MandatoryParameter =>
        val parameterIn = parameterInNode(
          node = node,
          name = node.text,
          code = node.text,
          index = index,
          isVariadic = false,
          evaluationStrategy = EvaluationStrategies.BY_REFERENCE,
          typeFullName = None
        )
        scope.addToScope(node.text, parameterIn)
        Ast(parameterIn)
      case node =>
        logger.warn(
          s"Non-mandatory parameters are not supported yet: ${node.text} (${node.getClass.getSimpleName} ($relativeFileName), skipping"
        )
        astForUnknown(node)
  }

  protected def astForSingletonMethodDeclaration(node: ParserAst.SingletonMethodDeclaration): Ast = {
    ParserAst(node.target) match
      case _: SelfIdentifier =>
        val method = methodNode(
          node = node,
          name = node.methodName,
          fullName = computeMethodFullName(node.methodName),
          code = node.text,
          signature = None,
          fileName = relativeFileName,
          astParentType = Some(getEnclosingAstType),
          astParentFullName = Some(getEnclosingAstFullName)
        )
        methodAstParentStack.push(method)
        scope.pushNewScope(method)

        val parameterAsts = node.parameters.zipWithIndex.map { case (parameterCtx, index) =>
          astForParameter(ParserAst(parameterCtx), index)
        }

        val stmtBlockAst = ParserAst(node.body) match
          case stmtList: StatementList => astForStatementList(stmtList)
          case body =>
            logger.warn(s"Non-linear method bodies are not supported yet: ${body.text}, skipping")
            astForUnknown(body)

        scope.popScope()
        methodAstParentStack.pop()
        methodAst(method, parameterAsts, stmtBlockAst, methodReturnNode(node, Defines.Any))

      case targetNode =>
        logger.warn(
          s"Non-self singleton method declarations are not supported yet: ${targetNode.text} (${targetNode.getClass.getSimpleName}), skipping"
        )
        astForUnknown(node)
  }

}
