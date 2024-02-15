package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.astcreation.RubyIntermediateAst.*
import io.joern.rubysrc2cpg.datastructures.{ConstructorScope, MethodScope}
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.x2cpg.{Ast, ValidationMode, Defines as XDefines}
import io.shiftleft.codepropertygraph.generated.{EvaluationStrategies, NodeTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{NewLocal, NewMethodParameterIn, NewTypeDecl}

trait AstForFunctionsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def astForMethodDeclaration(node: MethodDeclaration): Ast = {

    // Special case constructor methods
    val isInTypeDecl = scope.surroundingAstLabel.contains(NodeTypes.TYPE_DECL)
    val methodName = node.methodName match {
      case "initialize" if isInTypeDecl => XDefines.ConstructorMethodName
      case name                         => name
    }
    // TODO: body could be a try
    val fullName = computeMethodFullName(methodName)
    val method = methodNode(
      node = node,
      name = methodName,
      fullName = fullName,
      code = code(node),
      signature = None,
      fileName = relativeFileName
    )

    if (methodName == XDefines.ConstructorMethodName) scope.pushNewScope(ConstructorScope(fullName))
    else scope.pushNewScope(MethodScope(fullName))

    val parameterAsts = node.parameters.zipWithIndex.map { case (parameterNode, index) =>
      astForParameter(parameterNode, index)
    }

    val optionalStatementList = StatementList(
      node.parameters
        .collect { case x: OptionalParameter =>
          x
        }
        .map(statementForOptionalParam)
    )(TextSpan(None, None, None, None, ""))

    val stmtBlockAst = node.body match
      case stmtList: StatementList =>
        astForStatementListReturningLastExpression(
          StatementList(optionalStatementList.statements ++ stmtList.statements)(stmtList.span)
        )
      case _: (StaticLiteral | BinaryExpression | SingleAssignment | SimpleIdentifier | ArrayLiteral | HashLiteral |
            SimpleCall | MemberAccess | MemberCall) =>
        astForStatementListReturningLastExpression(
          StatementList(optionalStatementList.statements ++ List(node.body))(node.body.span)
        )
      case body =>
        logger.warn(
          s"Non-linear method bodies are not supported yet: ${body.text} (${body.getClass.getSimpleName}) ($relativeFileName), skipping"
        )
        astForUnknown(body)

    scope.popScope()
    methodAst(method, parameterAsts, stmtBlockAst, methodReturnNode(node, Defines.Any))
  }

  // TODO: remaining cases
  protected def astForParameter(node: RubyNode, index: Int): Ast = {
    node match
      case node: (MandatoryParameter | OptionalParameter) =>
        val _code = code(node)
        val parameterIn = parameterInNode(
          node = node,
          name = node.name,
          code = _code,
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

  private def generateTextSpan(node: RubyNode, text: String): TextSpan = {
    TextSpan(node.span.line, node.span.column, node.span.lineEnd, node.span.columnEnd, text)
  }

  protected def statementForOptionalParam(node: OptionalParameter): RubyNode = {
    val defaultExprNode = node.defaultExpression

    IfExpression(
      UnaryExpression(
        "!",
        SimpleCall(
          SimpleIdentifier(None)(generateTextSpan(defaultExprNode, "defined?")),
          List(SimpleIdentifier(None)(generateTextSpan(defaultExprNode, node.name)))
        )(generateTextSpan(defaultExprNode, s"defined?(${node.name})"))
      )(generateTextSpan(defaultExprNode, s"!defined?(${node.name})")),
      StatementList(
        List(
          SingleAssignment(
            SimpleIdentifier(None)(generateTextSpan(defaultExprNode, node.name)),
            "=",
            node.defaultExpression
          )(generateTextSpan(defaultExprNode, s"${node.name}=${node.defaultExpression.span.text}"))
        )
      )(generateTextSpan(defaultExprNode, "")),
      List.empty,
      None
    )(
      generateTextSpan(
        defaultExprNode,
        s"if !defined?(${node.name})  \t${node.name}=${node.defaultExpression.span.text}\n  end"
      )
    )
  }

  protected def astForAnonymousTypeDeclaration(node: AnonymousTypeDeclaration): Ast = {

    /** Handles the logic around singleton class behaviour, by registering that the anonymous type extends the base
      * variable's type, and nothing that the base variable now may be of the singleton's type.
      * @param typeDecl
      *   the resulting type decl of the anonymous type.
      */
    def handleSingletonClassBehaviour(typeDecl: NewTypeDecl): Unit = {
      typeDecl.inheritsFromTypeFullName.toList match {
        case baseVariableName :: _ =>
          // Attempt to resolve the 'true' inheritance type
          scope.lookupVariable(baseVariableName).foreach {
            case x: NewLocal if x.possibleTypes.nonEmpty => typeDecl.inheritsFromTypeFullName(x.possibleTypes)
            case x: NewMethodParameterIn if x.possibleTypes.nonEmpty =>
              typeDecl.inheritsFromTypeFullName(x.possibleTypes)
            case _ =>
          }
          scope.pushSingletonClassDeclaration(typeDecl.fullName, baseVariableName)
        case _ =>
      }
    }

    // This will link the type decl to the surrounding context via base overlays
    val typeDeclAst = astForClassDeclaration(node)
    Ast.storeInDiffGraph(typeDeclAst, diffGraph)

    typeDeclAst.nodes
      .collectFirst { case typeDecl: NewTypeDecl =>
        if (node.isInstanceOf[SingletonClassDeclaration]) handleSingletonClassBehaviour(typeDecl)

        val typeIdentifier = SimpleIdentifier()(node.span.spanStart(typeDecl.name))
        // Takes the `Class.new` before the block starts or any other keyword
        val newSpanText = typeDecl.code.takeWhile(_ != ' ')
        astForMemberCall(MemberCall(typeIdentifier, ".", "new", List.empty)(node.span.spanStart(newSpanText)))
      }
      .getOrElse(Ast())
  }

  protected def astForSingletonMethodDeclaration(node: SingletonMethodDeclaration): Ast = {
    node.target match
      case _: SelfIdentifier =>
        val fullName = computeMethodFullName(node.methodName)
        val method = methodNode(
          node = node,
          name = node.methodName,
          fullName = fullName,
          code = code(node),
          signature = None,
          fileName = relativeFileName,
          astParentType = scope.surroundingAstLabel,
          astParentFullName = scope.surroundingScopeFullName
        )

        scope.pushNewScope(MethodScope(fullName))

        val parameterAsts = node.parameters.zipWithIndex.map { case (parameterNode, index) =>
          astForParameter(parameterNode, index)
        }

        val stmtBlockAst = node.body match
          case stmtList: StatementList =>
            astForStatementList(StatementList(stmtList.statements)(stmtList.span))
          case body =>
            logger.warn(s"Non-linear method bodies are not supported yet: ${body.text}, skipping")
            astForUnknown(body)

        scope.popScope()
        methodAst(method, parameterAsts, stmtBlockAst, methodReturnNode(node, Defines.Any))

      case targetNode =>
        logger.warn(
          s"Non-self singleton method declarations are not supported yet: ${targetNode.text} (${targetNode.getClass.getSimpleName}), skipping"
        )
        astForUnknown(node)
  }

}
