package io.joern.kotlin2cpg.types

import org.jetbrains.kotlin.descriptors.DescriptorVisibility
import org.jetbrains.kotlin.descriptors.Modality
import org.jetbrains.kotlin.psi.{
  KtAnnotationEntry,
  KtBinaryExpression,
  KtCallExpression,
  KtClassLiteralExpression,
  KtClassOrObject,
  KtDestructuringDeclarationEntry,
  KtElement,
  KtExpression,
  KtFile,
  KtLambdaExpression,
  KtNameReferenceExpression,
  KtNamedFunction,
  KtParameter,
  KtPrimaryConstructor,
  KtProperty,
  KtQualifiedExpression,
  KtSecondaryConstructor,
  KtTypeAlias,
  KtTypeReference
}
import org.jetbrains.kotlin.resolve.BindingContext
import org.jetbrains.kotlin.types.KotlinType

case class AnonymousObjectContext(declaration: KtElement)

trait TypeInfoProvider {

  def usedAsExpression(expr: KtExpression): Option[Boolean]

  def isStaticMethodCall(expr: KtQualifiedExpression): Boolean

  def isReferenceToClass(expr: KtNameReferenceExpression): Boolean

  def bindingKind(expr: KtQualifiedExpression): CallKind

  def anySignature(args: Seq[Any]): String

  def isConstructorCall(expr: KtExpression): Option[Boolean]

  def hasStaticDesc(expr: KtQualifiedExpression): Boolean

  def isRefToCompanionObject(expr: KtNameReferenceExpression): Boolean
}
