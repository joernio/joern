package io.joern.c2cpg.astcreation

import io.joern.c2cpg.astcreation.C2CpgScope.ScopeVariable
import io.joern.x2cpg.Ast
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.utils.NodeBuilders
import io.joern.x2cpg.utils.NodeBuilders.newClosureBindingNode
import io.joern.x2cpg.utils.NodeBuilders.newModifierNode
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.codepropertygraph.generated.nodes.NewClosureBinding
import io.shiftleft.codepropertygraph.generated.nodes.NewIdentifier
import io.shiftleft.codepropertygraph.generated.nodes.NewLocal
import io.shiftleft.codepropertygraph.generated.nodes.NewMethod
import io.shiftleft.codepropertygraph.generated.nodes.NewMethodParameterIn
import io.shiftleft.codepropertygraph.generated.nodes.NewMethodRef
import io.shiftleft.codepropertygraph.generated.nodes.NewNode
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import io.shiftleft.codepropertygraph.generated.nodes.NewBlock
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.NewTypeDecl
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTLambdaExpression

object AstForLambdasCreator {

  private case class ClosureBindingEntry(node: ScopeVariable, binding: NewClosureBinding)

  private case class LambdaBody(body: Ast, capturedVariables: Seq[ClosureBindingEntry]) {
    def nodes: Seq[NewNode] = body.nodes.toSeq
  }

}

trait AstForLambdasCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  import AstForLambdasCreator.*

  private def defineCapturedVariables(
    lambdaExpression: ICPPASTLambdaExpression,
    lambdaMethodName: String,
    capturedVariables: Seq[ScopeVariable]
  ): Seq[(ClosureBindingEntry, NewLocal)] = {
    capturedVariables
      .groupBy(_.name)
      .map { case (name, variables) =>
        val closureBindingId   = s"$lambdaMethodName:$name"
        val closureBindingNode = newClosureBindingNode(closureBindingId, name, EvaluationStrategies.BY_SHARING)
        val scopeVariable      = variables.head
        val capturedLocal = localNode(
          lambdaExpression,
          scopeVariable.name,
          scopeVariable.name,
          scopeVariable.typeFullName,
          Option(closureBindingId)
        )
        scope.addToScope(scopeVariable.name, (capturedLocal, scopeVariable.typeFullName))
        ClosureBindingEntry(scopeVariable, closureBindingNode) -> capturedLocal
      }
      .toSeq
  }

  private def astForLambdaBody(
    lambdaExpression: ICPPASTLambdaExpression,
    lambdaMethodName: String,
    variablesInScope: Seq[ScopeVariable]
  ): LambdaBody = {
    val outerScopeVariableNames = variablesInScope.map(x => x.name -> x).toMap

    val bodyAst = astForMethodBody(Option(lambdaExpression.getBody))

    val capturedVariables = lambdaExpression.getCaptures.toList match {
      case Nil => Seq.empty
      case other =>
        bodyAst.nodes.collect {
          case i: NewIdentifier
              if outerScopeVariableNames.contains(i.name) &&
                other.exists(n => n.getIdentifier.getRawSignature == i.name) =>
            outerScopeVariableNames(i.name)
        }
    }

    val bindingsToLocals      = defineCapturedVariables(lambdaExpression, lambdaMethodName, capturedVariables.toSeq)
    val capturedLocals        = bindingsToLocals.map(_._2)
    val closureBindingEntries = bindingsToLocals.map(_._1)

    val blockAst = bodyAst.root match {
      case Some(b: NewBlock) =>
        capturedLocals.foreach { local =>
          diffGraph.addNode(local)
          diffGraph.addEdge(b, local, EdgeTypes.AST)
        }
        bodyAst
      case Some(_) =>
        Ast(blockNode(lambdaExpression.getBody))
          .withChildren(capturedLocals.map(Ast(_)))
          .withChild(bodyAst)
      case None => Ast()
    }
    LambdaBody(blockAst, closureBindingEntries)
  }

  private def createAndPushLambdaMethod(lambdaExpression: ICPPASTLambdaExpression): (NewMethod, LambdaBody) = {
    val filename                                                  = fileName(lambdaExpression)
    val MethodFullNameInfo(name, fullName, signature, returnType) = methodFullNameInfo(lambdaExpression)
    val codeString                                                = code(lambdaExpression)
    val variablesInScope                                          = scope.variablesInScope

    val lambdaMethodNode = methodNode(lambdaExpression, name, codeString, fullName, Some(signature), filename)

    scope.pushNewScope(lambdaMethodNode)

    val parameterNodes = withIndex(this.parameters(lambdaExpression.getDeclarator)) { (p, i) =>
      parameterNode(p, i)
    }
    setVariadic(parameterNodes, lambdaExpression)
    val parametersWithoutThis = parameterNodes.map(Ast(_))

    val lambdaBody = astForLambdaBody(lambdaExpression, name, variablesInScope)

    val thisParam = lambdaBody.nodes
      .collect { case identifier: NewIdentifier => identifier }
      .find { identifier => identifier.name == "this" || identifier.name == "super" }
      .map { _ =>
        val typeFullName = methodAstParentStack.collectFirst { case t: NewTypeDecl => t.fullName }
        Ast(
          NodeBuilders.newThisParameterNode(
            typeFullName = typeFullName.getOrElse(Defines.Any),
            dynamicTypeHintFullName = typeFullName.toSeq,
            line = line(lambdaExpression),
            column = column(lambdaExpression)
          )
        )
      }
      .toList

    val parameters = thisParam ++ parametersWithoutThis

    val lambdaParameterNamesToNodes =
      parameters
        .flatMap(_.root)
        .collect { case param: NewMethodParameterIn => param }
        .map { param => param.name -> param }
        .toMap

    val identifiersMatchingParams = lambdaBody.nodes
      .collect { case identifier: NewIdentifier => identifier }
      .filter { identifier => lambdaParameterNamesToNodes.contains(identifier.name) }

    val returnNode      = methodReturnNode(lambdaExpression, registerType(returnType))
    val virtualModifier = Some(newModifierNode(ModifierTypes.VIRTUAL))
    val staticModifier  = Option.when(thisParam.isEmpty)(newModifierNode(ModifierTypes.STATIC))
    val privateModifier = Some(newModifierNode(ModifierTypes.PRIVATE))
    val lambdaModifier  = Some(newModifierNode(ModifierTypes.LAMBDA))
    val modifiers       = List(virtualModifier, staticModifier, privateModifier, lambdaModifier).flatten.map(Ast(_))

    val lambdaMethodAstWithoutRefs =
      Ast(lambdaMethodNode)
        .withChildren(parameters)
        .withChild(lambdaBody.body)
        .withChild(Ast(returnNode))
        .withChildren(modifiers)

    val lambdaMethodAst = identifiersMatchingParams.foldLeft(lambdaMethodAstWithoutRefs)((ast, identifier) =>
      ast.withRefEdge(identifier, lambdaParameterNamesToNodes(identifier.name))
    )

    scope.popScope()

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
      astParentFullName
    )
    Ast.storeInDiffGraph(Ast(lambdaTypeDeclNode), diffGraph)
  }

  protected def astForLambdaExpression(lambdaExpression: ICPPASTLambdaExpression): Ast = {
    val (lambdaMethodNode, lambdaBody) = createAndPushLambdaMethod(lambdaExpression)
    val methodRef =
      methodRefNode(lambdaExpression, lambdaMethodNode.fullName, lambdaMethodNode.fullName, lambdaMethodNode.fullName)
    addClosureBindingsToDiffGraph(lambdaBody.capturedVariables, methodRef)
    createAndPushLambdaTypeDecl(lambdaExpression, lambdaMethodNode)
    Ast(methodRef)
  }

}
