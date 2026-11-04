package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.parser.BabelNodeSyntax.*
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.frontendspecific.jssrc2cpg.Defines
import io.joern.x2cpg.{Ast, ValidationMode}
import io.joern.x2cpg.datastructures.VariableScopeManager
import io.shiftleft.codepropertygraph.generated.nodes.{Expression as _, Identifier as _, *}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EdgeTypes, EvaluationStrategies, ModifierTypes}

import scala.collection.mutable

trait AstForFunctionsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  case class MethodAst(ast: Ast, methodNode: NewMethod, methodAst: Ast)

  private def handleRestInParameters(element: RestElement, param: BabelNode, paramName: String): Ast = {
    val ast         = astForNodeWithFunctionReferenceAndCall(element.argument)
    val defaultName = codeForNodes(ast.nodes.toSeq)
    val restName    = stripQuotes(nameForBabelNode(param, defaultName))
    ast.root match {
      case Some(_: NewIdentifier) =>
        val keyNode   = fieldIdentifierNode(element, restName, restName)
        val paramNode = identifierNode(element, paramName)
        val accessAst =
          createFieldAccessCallAst(paramNode, keyNode, element.lineNumber, element.columnNumber)
        createAssignmentCallAst(
          ast,
          accessAst,
          s"$restName = ${codeOf(accessAst.nodes.head)}",
          element.lineNumber,
          element.columnNumber
        )
      case _ =>
        val localParamNode = identifierNode(element, restName)
        createAssignmentCallAst(
          Ast(localParamNode),
          ast,
          s"$restName = ${codeOf(ast.nodes.head)}",
          element.lineNumber,
          element.columnNumber
        )
    }
  }

  private def handleParameters(
    parameters: Seq[BabelNode],
    additionalBlockStatements: mutable.ArrayBuffer[Ast],
    createLocals: Boolean = true
  ): Seq[NewMethodParameterIn] =
    withIndex(parameters) { case (param, index) =>
      val paramNode = param match {
        case rest: RestElement =>
          val paramName     = rest.code.replace("...", "")
          val tpe           = typeFor(rest)
          val typeFullName  = if (Defines.isBuiltinType(tpe)) tpe else Defines.Any
          val possibleTypes = Seq(tpe)
          if (createLocals) {
            val localTmpNode = localNode(rest, paramName, paramName, typeFullName)
              .order(0)
              .possibleTypes(possibleTypes)
            diffGraph.addEdge(localAstParentStack.head, localTmpNode, EdgeTypes.AST)
          }
          parameterInNode(rest, paramName, rest.code, index, true, EvaluationStrategies.BY_VALUE, typeFullName)
            .possibleTypes(possibleTypes)
        case assignment: AssignmentPattern =>
          assignment.left match {
            case lhs: (ObjectPattern | ArrayPattern) =>
              val paramName = generateUnusedVariableName(usedVariableNames, s"param$index")
              val param     = parameterInNode(
                assignment,
                paramName,
                assignment.code,
                index,
                isVariadic = false,
                EvaluationStrategies.BY_VALUE
              )
              scope.addVariable(paramName, param, Defines.Any, VariableScopeManager.ScopeType.MethodScope)

              val rhsAst = astForNodeWithFunctionReference(assignment.right)
              additionalBlockStatements.addOne(astForDeconstruction(lhs, rhsAst, assignment.code, Option(paramName)))
              param
            case lhs =>
              additionalBlockStatements.addOne(convertParamWithDefault(assignment))
              val possibleTypes = Seq(typeFor(lhs))
              parameterInNode(lhs, lhs.code, assignment.code, index, false, EvaluationStrategies.BY_VALUE)
                .possibleTypes(possibleTypes)
          }
        case arrayPattern: ArrayPattern =>
          val paramName     = generateUnusedVariableName(usedVariableNames, s"param$index")
          val tpe           = typeFor(arrayPattern)
          val typeFullName  = if (Defines.isBuiltinType(tpe)) tpe else Defines.Any
          val possibleTypes = Seq(tpe)
          val param         = parameterInNode(
            arrayPattern,
            paramName,
            arrayPattern.code,
            index,
            isVariadic = false,
            EvaluationStrategies.BY_VALUE,
            typeFullName
          ).possibleTypes(possibleTypes)
          additionalBlockStatements.addAll(arrayPattern.elements.iterator.map {
            case Some(element) =>
              element match {
                case _: Identifier =>
                  val elemName       = stripQuotes(element.code)
                  val tpe            = typeFor(element)
                  val typeFullName   = if (Defines.isBuiltinType(tpe)) tpe else Defines.Any
                  val possibleTypes  = Seq(tpe)
                  val localParamNode = identifierNode(element, elemName).possibleTypes(possibleTypes)
                  val localTmpNode   = localNode(element, elemName, elemName, typeFullName)
                    .order(0)
                    .possibleTypes(possibleTypes)
                  diffGraph.addEdge(localAstParentStack.head, localTmpNode, EdgeTypes.AST)
                  scope.addVariable(elemName, localTmpNode, Defines.Any, VariableScopeManager.ScopeType.MethodScope)

                  val paramNode = identifierNode(element, paramName)
                  scope.addVariableReference(paramName, paramNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)

                  val keyNode   = fieldIdentifierNode(element, elemName, elemName)
                  val accessAst = createFieldAccessCallAst(paramNode, keyNode, element.lineNumber, element.columnNumber)
                  createAssignmentCallAst(
                    Ast(localParamNode),
                    accessAst,
                    s"$elemName = ${codeOf(accessAst.nodes.head)}",
                    element.lineNumber,
                    element.columnNumber
                  )
                case rest: RestElement => handleRestInParameters(rest, arrayPattern, paramName)
                case _                 => astForNodeWithFunctionReference(element)
              }
            case None => Ast()
          })
          param
        case objectPattern: ObjectPattern =>
          val paramName = generateUnusedVariableName(usedVariableNames, s"param$index")
          // Handle de-structured parameters declared as `{ username: string; password: string; }`
          val typeDeclAst = astForTypeAlias(objectPattern)
          Ast.storeInDiffGraph(typeDeclAst, diffGraph)

          val tpe          = typeFor(objectPattern)
          var typeFullName = if (Defines.isBuiltinType(tpe)) tpe else Defines.Any

          val possibleTypes =
            Seq(
              typeDeclAst.root
                .collect { case t: NewTypeDecl =>
                  typeFullName = t.fullName
                  t.fullName
                }
                .getOrElse(tpe)
            )
          val param = parameterInNode(
            objectPattern,
            paramName,
            objectPattern.code,
            index,
            isVariadic = false,
            EvaluationStrategies.BY_VALUE,
            typeFullName
          ).possibleTypes(possibleTypes)
          scope.addVariable(paramName, param, typeFullName, VariableScopeManager.ScopeType.MethodScope)

          additionalBlockStatements.addAll(objectPattern.properties.iterator.map {
            case property: ObjectProperty =>
              val elemName = property.key.code

              val tpe           = typeFor(property)
              val typeFullName  = if (Defines.isBuiltinType(tpe)) tpe else Defines.Any
              val possibleTypes = Seq(tpe)

              val localParamNode =
                identifierNode(property, elemName).typeFullName(typeFullName).possibleTypes(possibleTypes)
              val localTmpNode =
                localNode(property, elemName, elemName, typeFullName).order(0).possibleTypes(possibleTypes)
              diffGraph.addEdge(localAstParentStack.head, localTmpNode, EdgeTypes.AST)
              scope.addVariable(elemName, localTmpNode, typeFullName, VariableScopeManager.ScopeType.MethodScope)

              val paramNode = identifierNode(property, paramName)
              scope.addVariableReference(paramName, paramNode, typeFullName, EvaluationStrategies.BY_REFERENCE)

              val keyNode   = fieldIdentifierNode(property, elemName, elemName)
              val accessAst =
                createFieldAccessCallAst(paramNode, keyNode, property.lineNumber, property.columnNumber)
              val assignmentCallAst = createAssignmentCallAst(
                Ast(localParamNode),
                accessAst,
                s"$elemName = ${codeOf(accessAst.nodes.head)}",
                property.lineNumber,
                property.columnNumber
              )
              // Handle identifiers referring to locals created by destructured parameters
              assignmentCallAst.nodes
                .collect { case i: NewIdentifier if localTmpNode.name == i.name => i }
                .map { i => assignmentCallAst.withRefEdge(i, localTmpNode) }
                .reduce(_ `merge` _)
            case rest: RestElement => handleRestInParameters(rest, objectPattern, paramName)
          })
          param
        case ident: Identifier =>
          // Handle types declared as `credentials: { username: string; password: string; }`
          val tpe           = typeFor(ident)
          var typeFullName  = if (Defines.isBuiltinType(tpe)) tpe else Defines.Any
          val annotatedType = ident.typeAnnotation.collect {
            case annotation: TSTypeAnnotation => annotation.typeAnnotation
            case annotation: TypeAnnotation   => annotation.typeAnnotation
          }
          val possibleType = annotatedType
            .map {
              case typeLiteral: TSTypeLiteral =>
                val typeDecl = astForTypeAlias(typeLiteral)
                Ast.storeInDiffGraph(typeDecl, diffGraph)
                typeDecl.root
                  .collect { case t: NewTypeDecl =>
                    typeFullName = t.fullName
                    t.fullName
                  }
                  .getOrElse(tpe)
              case _ => tpe
            }
            .getOrElse(tpe)

          val name = ident.name
          val node =
            parameterInNode(ident, name, ident.code, index, false, EvaluationStrategies.BY_VALUE, typeFullName)
              .possibleTypes(Seq(possibleType))
          scope.addVariable(name, node, typeFullName, VariableScopeManager.ScopeType.MethodScope)
          node
        case parameterProperty: TSParameterProperty =>
          val unpackedParam = parameterProperty.parameter

          val tpe           = typeFor(unpackedParam)
          val typeFullName  = if (Defines.isBuiltinType(tpe)) tpe else Defines.Any
          val possibleTypes = Seq(tpe)

          val name = unpackedParam match {
            case assignment: AssignmentPattern => assignment.left.code
            case ident: Identifier             => ident.name
          }
          val node =
            parameterInNode(
              parameterProperty,
              name,
              parameterProperty.code,
              index,
              false,
              EvaluationStrategies.BY_VALUE,
              typeFullName
            ).possibleTypes(possibleTypes)
          scope.addVariable(name, node, typeFullName, VariableScopeManager.ScopeType.MethodScope)
          node
        case other =>
          val tpe           = typeFor(other)
          val typeFullName  = if (Defines.isBuiltinType(tpe)) tpe else Defines.Any
          val possibleTypes = Seq(tpe)
          val node          =
            parameterInNode(
              other,
              other.code,
              other.code,
              index,
              isVariadic = false,
              EvaluationStrategies.BY_VALUE,
              typeFullName
            ).possibleTypes(possibleTypes)
          scope.addVariable(other.code, node, typeFullName, VariableScopeManager.ScopeType.MethodScope)
          node
      }
      val decoratorAsts = astsForDecorators(param)
      decoratorAsts.foreach { decoratorAst =>
        Ast.storeInDiffGraph(decoratorAst, diffGraph)
        decoratorAst.root.foreach(diffGraph.addEdge(paramNode, _, EdgeTypes.AST))
      }
      paramNode
    }

  private def convertParamWithDefault(element: AssignmentPattern): Ast = {
    val rhsAst = astForNodeWithFunctionReference(element.right)
    val lhsAst = astForNode(element.left)

    val testAst = {
      val keyNode      = identifierNode(element, codeOf(lhsAst.nodes.head))
      val voidCallNode =
        callNode(element, "void 0", "<operator>.void", DispatchTypes.STATIC_DISPATCH)
      val equalsCallAst = createEqualsCallAst(Ast(keyNode), Ast(voidCallNode), element.lineNumber, element.columnNumber)
      equalsCallAst
    }

    val falseNode = identifierNode(element, codeOf(lhsAst.nodes.head))

    val ternaryNodeAst = createTernaryCallAst(testAst, rhsAst, Ast(falseNode), element.lineNumber, element.columnNumber)
    createAssignmentCallAst(
      lhsAst,
      ternaryNodeAst,
      s"${codeOf(lhsAst.nodes.head)} = ${codeOf(ternaryNodeAst.nodes.head)}",
      element.lineNumber,
      element.columnNumber
    )
  }

  private def getParentTypeDecl: NewTypeDecl = {
    methodAstParentStack.collectFirst { case n: NewTypeDecl => n }.getOrElse(rootTypeDecl.head)
  }

  protected def astForTSDeclareFunction(func: TSDeclareFunction): Ast = {
    val functionNode  = createMethodDefinitionNode(func, ConstructorContent.empty)
    val tpe           = typeFor(func)
    val possibleTypes = Seq(tpe)
    val typeFullName  = if (Defines.isBuiltinType(tpe)) tpe else Defines.Any
    val memberNode_   = memberNode(func, functionNode.name, func.code, typeFullName, Seq(functionNode.fullName))
      .possibleTypes(possibleTypes)
    diffGraph.addEdge(getParentTypeDecl, memberNode_, EdgeTypes.AST)
    addModifier(functionNode, func)
    Ast()
  }

  protected def createMethodDefinitionNode(func: FunctionNode, methodBlockContent: ConstructorContent): NewMethod = {
    val (methodName, methodFullName) = calcMethodNameAndFullName(func)
    val methodNode_    = methodNode(func, methodName, func.code, methodFullName, None, parserResult.filename)
    val lambdaModifier = if (methodName.startsWith(io.joern.x2cpg.Defines.ClosurePrefix)) {
      modifierNode(func, ModifierTypes.LAMBDA) :: Nil
    } else {
      Nil
    }
    val modifiers = modifierNode(func, ModifierTypes.VIRTUAL) :: lambdaModifier
    methodAstParentStack.push(methodNode_)

    val thisNode =
      parameterInNode(func, "this", "this", 0, false, EvaluationStrategies.BY_VALUE)
        .dynamicTypeHintFullName(typeHintForThisExpression())
    scope.addVariable("this", thisNode, Defines.Any, VariableScopeManager.ScopeType.MethodScope)

    val paramNodes = handleParameters(paramsOf(func), mutable.ArrayBuffer.empty[Ast], createLocals = false)

    val blockAsts =
      methodBlockContent.constructorContent.flatMap(m => methodBlockContent.typeDecl.map(astForClassMember(m, _)))

    val methodReturnNode_ = methodReturnNode(func)

    methodAstParentStack.pop()

    val functionTypeAndTypeDeclAst =
      createFunctionTypeAndTypeDeclAst(
        func,
        methodNode_,
        methodAstParentStack.head,
        methodName,
        methodFullName,
        parserResult.filename
      )

    val mAst = if (blockAsts.isEmpty) {
      methodStubAst(methodNode_, (thisNode +: paramNodes).map(Ast(_)), methodReturnNode_, modifiers)
    } else {
      val bodyAst = blockAst(NewBlock(), blockAsts)
      methodAstWithAnnotations(
        methodNode_,
        (thisNode +: paramNodes).map(Ast(_)),
        bodyAst,
        methodReturnNode_,
        modifiers,
        astsForDecorators(func)
      )
    }

    Ast.storeInDiffGraph(mAst, diffGraph)
    Ast.storeInDiffGraph(functionTypeAndTypeDeclAst, diffGraph)
    diffGraph.addEdge(methodAstParentStack.head, methodNode_, EdgeTypes.AST)

    methodNode_
  }

  protected def createMethodAstAndNode(
    func: FunctionWithBody,
    shouldCreateFunctionReference: Boolean = false,
    shouldCreateAssignmentCall: Boolean = false,
    methodBlockContent: ConstructorContent
  ): MethodAst = {
    val (methodName, methodFullName) = calcMethodNameAndFullName(func)

    val methodRefNode_ = if (!shouldCreateFunctionReference) { None }
    else { Option(methodRefNode(func, methodName, methodFullName, methodFullName)) }

    val callAst = if (shouldCreateAssignmentCall && shouldCreateFunctionReference) {
      val idNode  = identifierNode(func, methodName)
      val idLocal = localNode(func, methodName, methodName, methodFullName).order(0)
      diffGraph.addEdge(localAstParentStack.head, idLocal, EdgeTypes.AST)
      scope.addVariable(methodName, idLocal, Defines.Any, VariableScopeManager.ScopeType.BlockScope)
      scope.addVariableReference(methodName, idNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)
      val code       = s"function $methodName = ${func.code}"
      val assignment = createAssignmentCallAst(idNode, methodRefNode_.get, code, func.lineNumber, func.columnNumber)
      assignment
    } else {
      Ast()
    }

    val methodNode_    = methodNode(func, methodName, func.code, methodFullName, None, parserResult.filename)
    val lambdaModifier = if (methodName.startsWith(io.joern.x2cpg.Defines.ClosurePrefix)) {
      modifierNode(func, ModifierTypes.LAMBDA) :: Nil
    } else {
      Nil
    }
    val modifierNodes = modifierNode(func, ModifierTypes.VIRTUAL) :: lambdaModifier

    methodAstParentStack.push(methodNode_)

    val body                      = bodyOf(func)
    val blockNode_                = blockNode(body, body.code, Defines.Any)
    val additionalBlockStatements = mutable.ArrayBuffer.empty[Ast]

    val capturingRefNode = if (shouldCreateFunctionReference) { methodRefNode_ }
    else { typeRefIdStack.headOption }

    scope.pushNewMethodScope(methodFullName, methodName, blockNode_, capturingRefNode)
    localAstParentStack.push(blockNode_)

    val thisNode = parameterInNode(func, "this", "this", 0, false, EvaluationStrategies.BY_VALUE)
      .dynamicTypeHintFullName(typeHintForThisExpression())
    scope.addVariable("this", thisNode, Defines.Any, VariableScopeManager.ScopeType.MethodScope)

    val paramNodes = handleParameters(paramsOf(func), additionalBlockStatements)

    val bodyStmtAsts = (func, body) match {
      case (_: ArrowFunctionExpression, expression: Expression) =>
        // when body is just one expression like const foo = () => 42, generate a Return node
        val retCode = expression.code.stripSuffix(";")
        returnAst(returnNode(expression, retCode), List(astForNodeWithFunctionReference(expression))) :: Nil
      case (_, block: BlockStatement) =>
        // use the statements of the block directly to avoid a double Block node
        createBlockStatementAsts(block.body)
      case (_, other) =>
        astForNodeWithFunctionReference(other) :: Nil
    }

    val methodBlockChildren = methodBlockContent.constructorContent.flatMap(m =>
      methodBlockContent.typeDecl.map(astForClassMember(m, _))
    ) ++ additionalBlockStatements.toList ++ bodyStmtAsts

    localAstParentStack.pop()
    scope.popScope()
    methodAstParentStack.pop()

    val functionTypeAndTypeDeclAst =
      createFunctionTypeAndTypeDeclAst(
        func,
        methodNode_,
        methodAstParentStack.head,
        methodName,
        methodFullName,
        parserResult.filename
      )

    val mAst =
      methodAstWithAnnotations(
        methodNode_,
        (thisNode +: paramNodes).map(Ast(_)),
        blockAst(blockNode_, methodBlockChildren),
        methodReturnNode(func),
        modifierNodes,
        astsForDecorators(func)
      )
    Ast.storeInDiffGraph(mAst, diffGraph)
    Ast.storeInDiffGraph(functionTypeAndTypeDeclAst, diffGraph)
    diffGraph.addEdge(methodAstParentStack.head, methodNode_, EdgeTypes.AST)

    methodRefNode_ match {
      case Some(ref) if callAst.nodes.isEmpty =>
        MethodAst(Ast(ref), methodNode_, mAst)
      case _ =>
        MethodAst(callAst, methodNode_, mAst)
    }
  }

  protected def astForFunctionDeclaration(
    func: FunctionWithBody,
    shouldCreateFunctionReference: Boolean = false,
    shouldCreateAssignmentCall: Boolean = false
  ): Ast = createMethodAstAndNode(
    func,
    shouldCreateFunctionReference,
    shouldCreateAssignmentCall,
    ConstructorContent.empty
  ).ast

}
