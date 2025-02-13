package io.joern.c2cpg.astcreation

import io.joern.c2cpg.astcreation.C2CpgScope.ScopeVariable
import io.joern.x2cpg.Ast
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.utils.NodeBuilders
import io.joern.x2cpg.utils.NodeBuilders.newClosureBindingNode
import io.joern.x2cpg.utils.NodeBuilders.newModifierNode
import io.joern.x2cpg.AstEdge
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.codepropertygraph.generated.nodes.NewBlock
import io.shiftleft.codepropertygraph.generated.nodes.NewClosureBinding
import io.shiftleft.codepropertygraph.generated.nodes.NewIdentifier
import io.shiftleft.codepropertygraph.generated.nodes.NewLocal
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

object AstForLambdasCreator {
  private case class ClosureBindingEntry(node: ScopeVariable, binding: NewClosureBinding)
  private case class LambdaBody(body: Ast, capturedVariables: Seq[ClosureBindingEntry])
}

trait AstForLambdasCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  import AstForLambdasCreator.*

  private def defineCapturedVariables(
    lambdaExpression: ICPPASTLambdaExpression,
    lambdaMethodName: String,
    capturedVariables: Seq[(ScopeVariable, String)],
    filename: String
  ): Seq[(ClosureBindingEntry, NewLocal)] = {
    capturedVariables
      .groupBy(_._1.name)
      .map { case (name, variables) =>
        val (scopeVariable, strategy) = variables.head
        val closureBindingId          = s"$filename:$lambdaMethodName:$name"
        val closureBindingNode        = newClosureBindingNode(closureBindingId, name, strategy)
        val capturedLocal = localNode(
          lambdaExpression,
          scopeVariable.name,
          scopeVariable.name,
          scopeVariable.typeFullName,
          Option(closureBindingId)
        )
        ClosureBindingEntry(scopeVariable, closureBindingNode) -> capturedLocal
      }
      .toSeq
  }

  private def shouldBeCaptured(
    identifier: NewIdentifier,
    outerScopeVariableNames: Map[String, ScopeVariable],
    bodyAst: Ast
  ): Boolean = {
    outerScopeVariableNames.contains(identifier.name) &&
    bodyAst.refEdges.exists(edge => edge.src == identifier && !bodyAst.nodes.contains(edge.dst))
  }

  private def calculateCapturedVariables(
    lambdaExpression: ICPPASTLambdaExpression,
    bodyAst: Ast,
    variablesInScope: Seq[ScopeVariable]
  ): Seq[(ScopeVariable, String)] = {
    val captureDefault          = lambdaExpression.getCaptureDefault
    val outerScopeVariableNames = variablesInScope.map(x => x.name -> x).toMap
    val capturedVariables = lambdaExpression.getCaptures.toList match {
      case captures if captures.isEmpty && captureDefault == CaptureDefault.UNSPECIFIED =>
        Seq.empty
      case captures if captures.isEmpty =>
        val strategy = captureDefault match {
          case CaptureDefault.BY_REFERENCE => EvaluationStrategies.BY_REFERENCE
          case _                           => EvaluationStrategies.BY_VALUE
        }
        bodyAst.nodes.collect {
          case i: NewIdentifier if shouldBeCaptured(i, outerScopeVariableNames, bodyAst) =>
            (outerScopeVariableNames(i.name), strategy)
        }
      case other =>
        val validCaptures = other.filter(_.getIdentifier != null)
        bodyAst.nodes.collect {
          case i: NewIdentifier if shouldBeCaptured(i, outerScopeVariableNames, bodyAst) =>
            val maybeInCaptures = validCaptures.find(c => c.getIdentifier.getRawSignature == i.name)
            val strategy = maybeInCaptures match {
              case Some(c) if c.isByReference                            => EvaluationStrategies.BY_REFERENCE
              case None if captureDefault == CaptureDefault.BY_REFERENCE => EvaluationStrategies.BY_REFERENCE
              case _                                                     => EvaluationStrategies.BY_VALUE
            }
            (outerScopeVariableNames(i.name), strategy)
        }
    }
    capturedVariables.toSeq
  }

  private def fixupRefEdgesForCapturedLocals(bodyAst: Ast, capturedLocals: Seq[NewLocal]): Ast = {

    def needsFixedRefEdge(i: NewIdentifier): Boolean = {
      // We only need to fix the ref edge if we would cross method boundaries
      bodyAst.refEdges.exists(edge => edge.src == i && !bodyAst.nodes.contains(edge.dst))
    }

    // During the traversal of the lambda body we may ref identifier to some outer param if any.
    // This would cross method boundaries, which is invalid but at that time
    // we do not know this. Hence, we fix that up here:
    var bodyAst_ = bodyAst
    val capturedLocalFixCandidates = capturedLocals.flatMap { local =>
      bodyAst.nodes.collect { case i: NewIdentifier if i.name == local.name && needsFixedRefEdge(i) => (i, local) }
    }
    capturedLocalFixCandidates.foreach { case (i, local) =>
      val oldEdge = bodyAst_.refEdges.find(_.src == i)
      if (oldEdge.nonEmpty) {
        val fixedRefEdges = bodyAst_.refEdges.toList.diff(oldEdge.toList)
        val newRefEdges   = fixedRefEdges ++ List(AstEdge(i, local))
        bodyAst_ = bodyAst_.copy(refEdges = newRefEdges)
      }
    }
    bodyAst_
  }

  private def astForLambdaBody(
    lambdaExpression: ICPPASTLambdaExpression,
    lambdaMethodName: String,
    variablesInScope: Seq[ScopeVariable],
    filename: String
  ): LambdaBody = {
    var bodyAst = astForMethodBody(Option(lambdaExpression.getBody))
    if (bodyAst.nodes.isEmpty) return LambdaBody(Ast(), Seq.empty)

    val capturedVariables     = calculateCapturedVariables(lambdaExpression, bodyAst, variablesInScope)
    val bindingsToLocals      = defineCapturedVariables(lambdaExpression, lambdaMethodName, capturedVariables, filename)
    val capturedLocals        = bindingsToLocals.map(_._2)
    val closureBindingEntries = bindingsToLocals.map(_._1)

    bodyAst = fixupRefEdgesForCapturedLocals(bodyAst, capturedLocals)

    val capturedLocalsAsts = capturedLocals.map(Ast(_))
    val blockAst = bodyAst.root match {
      case Some(b: NewBlock) =>
        Ast(b).withChildren(capturedLocalsAsts).merge(bodyAst)
      case Some(_) =>
        Ast(blockNode(lambdaExpression.getBody)).withChildren(capturedLocalsAsts).withChild(bodyAst)
      case None => Ast()
    }
    LambdaBody(blockAst, closureBindingEntries)
  }

  private def createAndPushLambdaMethod(lambdaExpression: ICPPASTLambdaExpression): (NewMethod, LambdaBody) = {
    val MethodFullNameInfo(name, fullName, signature, returnType) = methodFullNameInfo(lambdaExpression)
    val filename                                                  = fileName(lambdaExpression)
    val codeString                                                = code(lambdaExpression)
    val variablesInScope                                          = scope.variablesInScope

    val lambdaMethodNode = methodNode(lambdaExpression, name, codeString, fullName, Some(signature), filename)

    methodAstParentStack.push(lambdaMethodNode)
    scope.pushNewScope(lambdaMethodNode)

    val parameterNodes = withIndex(parameters(lambdaExpression.getDeclarator)) { (p, i) => parameterNode(p, i) }
    setVariadic(parameterNodes, lambdaExpression)
    val parameterAsts = parameterNodes.map(Ast(_))
    val lambdaBody    = astForLambdaBody(lambdaExpression, name, variablesInScope, filename)

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
      .withChild(lambdaBody.body)
      .withChild(Ast(returnNode))
      .withChildren(modifiers)

    val parentNode = methodAstParentStack.collectFirst { case t: NewTypeDecl => t }
    Ast.storeInDiffGraph(lambdaMethodAst, diffGraph)
    parentNode.foreach { typeDeclNode =>
      diffGraph.addEdge(typeDeclNode, lambdaMethodNode, EdgeTypes.AST)
    }
    lambdaMethodNode -> lambdaBody
  }

  private def addClosureBindingsToDiffGraph(
    bindingEntries: Iterable[ClosureBindingEntry],
    methodRef: NewMethodRef
  ): Unit = {
    bindingEntries.foreach { case ClosureBindingEntry(nodeTypeInfo, closureBinding) =>
      diffGraph.addNode(closureBinding)
      diffGraph.addEdge(closureBinding, nodeTypeInfo.node, EdgeTypes.REF)
      diffGraph.addEdge(methodRef, closureBinding, EdgeTypes.CAPTURE)
    }
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
    val (lambdaMethodNode, lambdaBody) = createAndPushLambdaMethod(lambdaExpression)
    val refCode                        = lambdaMethodNode.fullName
    val refFullName                    = lambdaMethodNode.fullName
    val refTypeFullName                = lambdaMethodNode.fullName
    val methodRef                      = methodRefNode(lambdaExpression, refCode, refFullName, refTypeFullName)
    addClosureBindingsToDiffGraph(lambdaBody.capturedVariables, methodRef)
    createAndPushLambdaTypeDecl(lambdaExpression, lambdaMethodNode)
    Ast(methodRef)
  }

}
