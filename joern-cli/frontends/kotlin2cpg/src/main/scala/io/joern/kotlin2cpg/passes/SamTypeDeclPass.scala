package io.joern.kotlin2cpg.passes

import io.joern.kotlin2cpg.Constants
import io.joern.kotlin2cpg.ast.AstCreator
import io.joern.kotlin2cpg.ast.AstCreator.{BoundSamInfo, SamImplInfo, UnboundSamInfo}
import io.joern.kotlin2cpg.types.TypeConstants
import io.joern.x2cpg.{Ast, AstCreatorBase, ValidationMode}
import io.joern.x2cpg.AstNodeBuilder.bindingNode
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.{ExpressionNew, NewBinding, NewNode, NewTypeDecl}
import io.shiftleft.passes.CpgPass
import org.jetbrains.kotlin.com.intellij.psi.PsiElement
import org.jetbrains.kotlin.psi.KtCallableReferenceExpression

import scala.util.Try

class SamTypeDeclPass(cpg: Cpg, samInfoEntries: Iterable[SamImplInfo])(implicit withSchemaValidation: ValidationMode)
    extends CpgPass(cpg, "kotlin-sam-type-decls") {

  override def run(diffGraph: DiffGraphBuilder): Unit = {
    val astBuilder = new SamTypeDeclAstBuilder()
    samInfoEntries.toSeq.sortBy(_.samImplClass).foreach { samInfo =>
      val build = astBuilder.createTypeDeclAst(samInfo)
      Ast.storeInDiffGraph(build.typeDeclAst, diffGraph)
      build.bindings.foreach { binding =>
        diffGraph.addNode(binding)
        diffGraph.addEdge(build.typeDeclNode, binding, EdgeTypes.BINDS)
      }
    }
  }

}

private object SamTypeDeclPass {
  case class SamTypeDeclBuild(typeDeclAst: Ast, typeDeclNode: NewTypeDecl, bindings: Seq[NewBinding])
}

private class SamTypeDeclAstBuilder()(implicit withSchemaValidation: ValidationMode)
    extends AstCreatorBase[PsiElement, SamTypeDeclAstBuilder]("<sam-type-decl>") {

  import SamTypeDeclPass.SamTypeDeclBuild

  override def createAst(): DiffGraphBuilder = diffGraph

  override protected def code(element: PsiElement): String = {
    shortenCode(element.getText)
  }

  override def line(element: PsiElement): Option[Int] =
    Try(element.getContainingFile.getViewProvider.getDocument.getLineNumber(element.getTextOffset) + 1).toOption

  override def column(element: PsiElement): Option[Int] = {
    for {
      lineNumber <- line(element)
      lineOffset <- Try(
        element.getContainingFile.getViewProvider.getDocument.getLineStartOffset(lineNumber - 1)
      ).toOption
    } yield element.getTextOffset - lineOffset
  }

  override def lineEnd(element: PsiElement): Option[Int] = line(element)

  override def columnEnd(element: PsiElement): Option[Int] = column(element)

  override def offset(element: PsiElement): Option[(Int, Int)] = {
    Option(element).map { someElement =>
      val textRange = someElement.getTextRange
      (textRange.getStartOffset, textRange.getEndOffset)
    }
  }

  def createTypeDeclAst(samInfo: SamImplInfo): SamTypeDeclBuild = {
    samInfo match {
      case unbound: UnboundSamInfo => createUnboundSamTypeDecl(unbound.relativizedPath, unbound)
      case bound: BoundSamInfo     => createBoundSamTypeDecl(bound.relativizedPath, bound)
    }
  }

  private def registerType(typeName: String): String = typeName

  private def createBindingInfo(
    methodName: String,
    signature: String,
    methodFullName: String,
    genericSignature: Option[String]
  ): Seq[NewBinding] = {
    val bindings = Seq(bindingNode(methodName, signature, methodFullName))
    genericSignature match {
      case Some(genericSig) => bindings :+ bindingNode(methodName, genericSig, methodFullName)
      case None             => bindings
    }
  }

  private def createUnboundSamTypeDecl(relativizedPath: String, samInfo: UnboundSamInfo): SamTypeDeclBuild = {
    val samTypeDecl = typeDeclNode(
      samInfo.expr,
      samInfo.samImplClass.split('.').last,
      samInfo.samImplClass,
      relativizedPath,
      samInfo.inheritsFrom.map(registerType),
      None
    )

    val samMethodFullName = s"${samInfo.methodRefName}:${samInfo.signature}"
    val bindings =
      createBindingInfo(samInfo.samMethodName, samInfo.samMethodSig, samMethodFullName, samInfo.samGenericMethodSig)

    SamTypeDeclBuild(Ast(samTypeDecl), samTypeDecl, bindings)
  }

  private def createReceiverAccess(
    expr: KtCallableReferenceExpression,
    receiverTypeFullName: String,
    implementationTypeFullName: String
  ): Ast = {
    val thisIdent  = identifierNode(expr, "this", "this", implementationTypeFullName)
    val fieldIdent = fieldIdentifierNode(expr, Constants.ReceiverName, Constants.ReceiverName)
    val receiverFieldAccess = callNode(
      expr,
      "this.receiver",
      Operators.fieldAccess,
      Operators.fieldAccess,
      DispatchTypes.STATIC_DISPATCH,
      None,
      Some(receiverTypeFullName)
    ).argumentIndex(0)
    callAst(receiverFieldAccess, Seq(Ast(thisIdent), Ast(fieldIdent)), None)
  }

  private def createBoundSamMethodAst(
    relativizedPath: String,
    samInfo: BoundSamInfo,
    samMethodFullName: String
  ): Ast = {
    val samMethodNode = methodNode(
      samInfo.expr,
      samInfo.samMethodName,
      samInfo.samMethodName,
      samMethodFullName,
      Some(samInfo.samMethodSig),
      relativizedPath
    )

    val thisParamAst = {
      val paramNode =
        parameterInNode(samInfo.expr, "this", "this", 0, false, EvaluationStrategies.BY_SHARING, samInfo.samImplClass)
          .dynamicTypeHintFullName(IndexedSeq(samInfo.samImplClass))
      Ast(paramNode)
    }

    val valueParameterNodes = samInfo.samMethodParams.zipWithIndex.map { case ((paramName, paramType), idx) =>
      parameterInNode(
        samInfo.expr,
        paramName,
        paramName,
        idx + 1,
        false,
        EvaluationStrategies.BY_VALUE,
        registerType(paramType)
      )
    }

    val allParameterAsts = thisParamAst +: valueParameterNodes.map(Ast(_))

    val paramString      = samInfo.samMethodParams.map(_._1).mkString(", ")
    val returnType       = samInfo.samMethodReturn
    val methodReturn     = methodReturnNode(samInfo.expr, registerType(returnType))
    val calledMethodName = samInfo.methodRefName.split('.').last

    val callCode   = s"receiver.${calledMethodName}(${paramString})"
    val blockNode_ = blockNode(samInfo.expr, s"return $callCode", TypeConstants.JavaLangVoid)

    val (dispatchType, receiverAstOpt) = if (samInfo.isStaticReference) {
      val staticReceiverArg =
        createReceiverAccess(samInfo.expr, samInfo.receiverTypeFullName, samInfo.samImplClass)
      (DispatchTypes.STATIC_DISPATCH, Some(staticReceiverArg))
    } else {
      val receiverAst =
        createReceiverAccess(samInfo.expr, samInfo.receiverTypeFullName, samInfo.samImplClass)
      (DispatchTypes.DYNAMIC_DISPATCH, Some(receiverAst))
    }

    val callNode_ = callNode(
      samInfo.expr,
      callCode,
      calledMethodName,
      s"${samInfo.methodRefName}:${samInfo.signature}",
      dispatchType,
      Some(samInfo.signature),
      Some(registerType(returnType))
    )

    val callArgumentAsts = samInfo.samMethodParams.zipWithIndex.zip(valueParameterNodes).map {
      case (((paramName, paramType), idx), methodParamNode) =>
        val identNode =
          identifierNode(samInfo.expr, paramName, paramName, registerType(paramType)).argumentIndex(idx + 1)
        Ast(identNode).withRefEdge(identNode, methodParamNode)
    }

    val callAst_    = callAst(callNode_, callArgumentAsts.toList, receiverAstOpt)
    val returnNode_ = returnNode(samInfo.expr, s"return ${callCode}")
    val returnAst_  = returnAst(returnNode_, List(callAst_))

    val modifiers =
      Seq(modifierNode(samInfo.expr, ModifierTypes.PUBLIC), modifierNode(samInfo.expr, ModifierTypes.VIRTUAL))

    methodAst(samMethodNode, allParameterAsts, blockAst(blockNode_, List(returnAst_)), methodReturn, modifiers)
  }

  private def createBoundSamConstructorAst(relativizedPath: String, samInfo: BoundSamInfo): Ast = {
    val receiverType = samInfo.receiverTypeFullName
    val ctorFullName = s"${samInfo.samImplClass}.<init>:void(${receiverType})"
    val ctorNode =
      methodNode(samInfo.expr, "<init>", "<init>", ctorFullName, Some(s"void(${receiverType})"), relativizedPath)

    val ctorThisParam =
      parameterInNode(samInfo.expr, "this", "this", 0, false, EvaluationStrategies.BY_SHARING, samInfo.samImplClass)
        .dynamicTypeHintFullName(IndexedSeq(samInfo.samImplClass))

    val ctorReceiverParam = parameterInNode(
      samInfo.expr,
      Constants.ReceiverName,
      Constants.ReceiverName,
      1,
      false,
      EvaluationStrategies.BY_VALUE,
      registerType(receiverType)
    )

    val thisIdentifier = identifierNode(
      samInfo.expr,
      Constants.ThisName,
      Constants.ThisName,
      samInfo.samImplClass,
      Seq(samInfo.samImplClass)
    )

    val receiverFieldIdentifier = fieldIdentifierNode(samInfo.expr, Constants.ReceiverName, Constants.ReceiverName)
    val receiverFieldAccessNode =
      operatorCallNode(samInfo.expr, s"this.receiver", Operators.fieldAccess, Option(registerType(receiverType)))
    val receiverFieldAccessAst =
      callAst(receiverFieldAccessNode, Seq(Ast(thisIdentifier), Ast(receiverFieldIdentifier)))

    val receiverArgIdent =
      identifierNode(
        samInfo.expr,
        Constants.ReceiverName,
        shortenCode(Constants.ReceiverName),
        registerType(receiverType)
      ).argumentIndex(2)
    val receiverArgAst = Ast(receiverArgIdent).withRefEdge(receiverArgIdent, ctorReceiverParam)

    val receiverAssignmentNode =
      operatorCallNode(samInfo.expr, shortenCode(s"this.receiver = receiver"), Operators.assignment, None)
    val receiverAssignmentAst = callAst(receiverAssignmentNode, Seq(receiverFieldAccessAst, receiverArgAst))

    val ctorBlock =
      blockNode(samInfo.expr, shortenCode(s"this.receiver = receiver"), registerType(TypeConstants.Void))
    val ctorReturn    = methodReturnNode(samInfo.expr, TypeConstants.Void)
    val ctorModifiers = Seq(modifierNode(samInfo.expr, ModifierTypes.CONSTRUCTOR))

    methodAst(
      ctorNode,
      Seq(Ast(ctorThisParam), Ast(ctorReceiverParam)),
      blockAst(ctorBlock, List(receiverAssignmentAst)),
      ctorReturn,
      ctorModifiers
    )
  }

  private def createBoundSamTypeDecl(relativizedPath: String, samInfo: BoundSamInfo): SamTypeDeclBuild = {
    val samTypeDecl = typeDeclNode(
      samInfo.expr,
      samInfo.samImplClass.split('.').last,
      samInfo.samImplClass,
      relativizedPath,
      samInfo.inheritsFrom.map(registerType),
      None
    )

    val samMethodFullName = s"${samInfo.samImplClass}.${samInfo.samMethodName}:${samInfo.samMethodSig}"
    val methodAst         = createBoundSamMethodAst(relativizedPath, samInfo, samMethodFullName)
    val ctorAst           = createBoundSamConstructorAst(relativizedPath, samInfo)

    val bindings =
      createBindingInfo(samInfo.samMethodName, samInfo.samMethodSig, samMethodFullName, samInfo.samGenericMethodSig)

    SamTypeDeclBuild(Ast(samTypeDecl).withChild(methodAst).withChild(ctorAst), samTypeDecl, bindings)
  }

}
