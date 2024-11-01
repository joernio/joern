package io.joern.kotlin2cpg.types

import org.jetbrains.kotlin.descriptors.FunctionDescriptor
import org.jetbrains.kotlin.psi.KtElement
import org.jetbrains.kotlin.psi.KtExpression
import org.jetbrains.kotlin.psi.KtNameReferenceExpression
import org.jetbrains.kotlin.psi.KtQualifiedExpression

case class AnonymousObjectContext(declaration: KtElement)

trait TypeInfoProvider {

  def usedAsExpression(expr: KtExpression): Option[Boolean]

  def usedAsImplicitThis(expr: KtNameReferenceExpression): Boolean

  def isStaticMethodCall(expr: KtQualifiedExpression): Boolean

  def isReferenceToClass(expr: KtNameReferenceExpression): Boolean

  def bindingKind(expr: KtQualifiedExpression): CallKind

  def anySignature(args: Seq[Any]): String

  def isConstructorCall(expr: KtExpression): Option[Boolean]

  def hasStaticDesc(expr: KtQualifiedExpression): Boolean

  def isRefToCompanionObject(expr: KtNameReferenceExpression): Boolean
}
