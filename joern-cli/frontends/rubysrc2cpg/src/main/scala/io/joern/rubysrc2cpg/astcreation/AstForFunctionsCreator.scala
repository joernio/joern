package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.astcreation.RubyIntermediateAst.*
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies

trait AstForFunctionsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def astForMethodDeclaration(node: MethodDeclaration): Ast = {

    // Special case constructor methods
    val isInTypeDecl = getEnclosingAstType == "TYPE_DECL"
    val methodName = node.methodName match {
      case "initialize" if isInTypeDecl =>
        setNoDefaultConstructorForEnclosingTypeDecl
        "<init>"
      case name => name
    }
    // TODO: body could be a try
    val method = methodNode(
      node = node,
      name = methodName,
      fullName = computeMethodFullName(methodName),
      code = code(node),
      signature = None,
      fileName = relativeFileName
    )
    methodAstParentStack.push(method)

    val parameterAsts = node.parameters.zipWithIndex.map { case (parameterNode, index) =>
      astForParameter(parameterNode, index)
    }

    val stmtBlockAst = node.body match
      case stmtList: StatementList => astForStatementListReturningLastExpression(stmtList)
      case _: (StaticLiteral | BinaryExpression | SingleAssignment | SimpleIdentifier | ArrayLiteral | HashLiteral |
            SimpleCall | MemberAccess | MemberCall) =>
        astForStatementListReturningLastExpression(StatementList(List(node.body))(node.body.span))
      case body =>
        logger.warn(
          s"Non-linear method bodies are not supported yet: ${body.text} (${body.getClass.getSimpleName}) ($relativeFileName), skipping"
        )
        astForUnknown(body)

    methodAstParentStack.pop()
    methodAst(method, parameterAsts, stmtBlockAst, methodReturnNode(node, Defines.Any))
  }

  // TODO: remaining cases
  protected def astForParameter(node: RubyNode, index: Int): Ast = {
    node match
      case node: MandatoryParameter =>
        val parameterIn = parameterInNode(
          node = node,
          name = code(node),
          code = code(node),
          index = index,
          isVariadic = false,
          evaluationStrategy = EvaluationStrategies.BY_REFERENCE,
          typeFullName = None
        )
        scope.addToScope(code(node), parameterIn)
        Ast(parameterIn)
      case node =>
        logger.warn(
          s"Non-mandatory parameters are not supported yet: ${code(node)} (${node.getClass.getSimpleName} ($relativeFileName), skipping"
        )
        astForUnknown(node)
  }

  protected def astForSingletonMethodDeclaration(node: SingletonMethodDeclaration): Ast = {
    node.target match
      case _: SelfIdentifier =>
        val method = methodNode(
          node = node,
          name = node.methodName,
          fullName = computeMethodFullName(node.methodName),
          code = code(node),
          signature = None,
          fileName = relativeFileName,
          astParentType = Some(getEnclosingAstType),
          astParentFullName = Some(getEnclosingAstFullName)
        )
        methodAstParentStack.push(method)
        scope.pushNewScope(method)

        val parameterAsts = node.parameters.zipWithIndex.map { case (parameterNode, index) =>
          astForParameter(parameterNode, index)
        }

        val stmtBlockAst = node.body match
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
