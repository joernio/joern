package io.joern.c2cpg.astcreation

import io.joern.x2cpg.Ast
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.utils.NodeBuilders
import io.joern.x2cpg.utils.NodeBuilders.newModifierNode
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.codepropertygraph.generated.nodes.NewIdentifier
import io.shiftleft.codepropertygraph.generated.nodes.NewMethod
import io.shiftleft.codepropertygraph.generated.nodes.NewMethodRef
import io.shiftleft.codepropertygraph.generated.nodes.NewNode
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.NewTypeDecl
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import io.shiftleft.codepropertygraph.generated.nodes.NewBinding
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTCapture
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTLambdaExpression
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTLambdaExpression.CaptureDefault

trait AstForLambdasCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  private def calculateCapturedVariables(lambdaExpression: ICPPASTLambdaExpression, bodyAst: Ast): Unit = {
    val captureDefault = lambdaExpression.getCaptureDefault
    val strategyMapping = captureDefault match {
      case CaptureDefault.BY_REFERENCE => EvaluationStrategies.BY_REFERENCE
      case _                           => EvaluationStrategies.BY_VALUE
    }
    lambdaExpression.getCaptures match {
      case captures if captures.isEmpty && captureDefault == CaptureDefault.UNSPECIFIED => // do nothing
      case captures if captures.isEmpty =>
        bodyAst.nodes.foreach {
          case i: NewIdentifier if !scope.variableIsInMethodScope(i.name) =>
            scope.updateVariableReference(i, strategyMapping)
          case _ => // do nothing
        }
      case other =>
        val validCaptures = other.filter(_.getIdentifier != null)
        bodyAst.nodes.foreach {
          case i: NewIdentifier if !scope.variableIsInMethodScope(i.name) =>
            val maybeInCaptures = validCaptures.find(c => code(c.getIdentifier) == i.name)
            val strategy = maybeInCaptures match {
              case Some(c) if c.isByReference => EvaluationStrategies.BY_REFERENCE
              case _                          => strategyMapping
            }
            scope.updateVariableReference(i, strategy)
          case _ => // do nothing
        }
    }
  }

  private def createAndPushLambdaMethod(lambdaExpression: ICPPASTLambdaExpression): (NewMethod, NewMethodRef) = {
    val MethodFullNameInfo(name, fullName, signature, returnType) = methodFullNameInfo(lambdaExpression)
    val filename                                                  = fileName(lambdaExpression)
    val codeString                                                = code(lambdaExpression)

    val methodRef        = methodRefNode(lambdaExpression, fullName, fullName, fullName)
    val lambdaMethodNode = methodNode(lambdaExpression, name, codeString, fullName, Some(signature), filename)

    val lambdaMethodBlockNode = blockNode(lambdaExpression)
    methodAstParentStack.push(lambdaMethodNode)
    scope.pushNewMethodScope(fullName, name, lambdaMethodBlockNode, Some(methodRef))

    val parameterNodes = withIndex(parameters(lambdaExpression.getDeclarator)) { (p, i) => parameterNode(p, i) }
    setVariadic(parameterNodes, lambdaExpression)
    val parameterAsts = parameterNodes.map(Ast(_))
    val lambdaBodyAst = astForMethodBody(Option(lambdaExpression.getBody), lambdaMethodBlockNode)
    calculateCapturedVariables(lambdaExpression, lambdaBodyAst)

    scope.popScope()
    methodAstParentStack.pop()

    val isStatic        = !lambdaExpression.getCaptures.exists(c => c.capturesThisPointer())
    val returnNode      = methodReturnNode(lambdaExpression, registerType(returnType))
    val virtualModifier = Some(newModifierNode(ModifierTypes.VIRTUAL))
    val staticModifier  = Option.when(isStatic)(newModifierNode(ModifierTypes.STATIC))
    val privateModifier = Some(newModifierNode(ModifierTypes.PRIVATE))
    val lambdaModifier  = Some(newModifierNode(ModifierTypes.LAMBDA))
    val modifiers       = List(virtualModifier, staticModifier, privateModifier, lambdaModifier).flatten.map(Ast(_))

    val lambdaMethodAst = Ast(lambdaMethodNode)
      .withChildren(parameterAsts)
      .withChild(lambdaBodyAst)
      .withChild(Ast(returnNode))
      .withChildren(modifiers)

    val parentNode = methodAstParentStack.collectFirst { case t: NewTypeDecl => t }
    Ast.storeInDiffGraph(lambdaMethodAst, diffGraph)
    parentNode.foreach { typeDeclNode =>
      diffGraph.addEdge(typeDeclNode, lambdaMethodNode, EdgeTypes.AST)
    }
    (lambdaMethodNode, methodRef)
  }

  private def createAndPushLambdaTypeDecl(
    lambdaExpression: ICPPASTLambdaExpression,
    lambdaMethodNode: NewMethod
  ): Unit = {
    registerType(lambdaMethodNode.fullName)
    val (astParentType, astParentFullName) = methodDeclarationParentInfo()
    val lambdaTypeDeclNode = typeDeclNode(
      lambdaExpression,
      lambdaMethodNode.name,
      lambdaMethodNode.fullName,
      lambdaMethodNode.filename,
      lambdaMethodNode.fullName,
      astParentType,
      astParentFullName,
      Seq(registerType(Defines.Function))
    )

    val functionBinding = NewBinding()
      .name(Defines.OperatorCall)
      .methodFullName(lambdaMethodNode.fullName)
      .signature(lambdaMethodNode.signature)

    val functionBindAst = Ast(functionBinding)
      .withBindsEdge(lambdaTypeDeclNode, functionBinding)
      .withRefEdge(functionBinding, lambdaMethodNode)

    Ast.storeInDiffGraph(Ast(lambdaTypeDeclNode), diffGraph)
    Ast.storeInDiffGraph(functionBindAst, diffGraph)
  }

  protected def astForLambdaExpression(lambdaExpression: ICPPASTLambdaExpression): Ast = {
    val (lambdaMethodNode, lambdaMethodRef) = createAndPushLambdaMethod(lambdaExpression)
    createAndPushLambdaTypeDecl(lambdaExpression, lambdaMethodNode)
    Ast(lambdaMethodRef)
  }

}
