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

trait TypeInfoProvider(val typeRenderer: TypeRenderer = new TypeRenderer()) {
  val bindingContext: BindingContext

  def usedAsExpression(expr: KtExpression): Option[Boolean]

  def isStaticMethodCall(expr: KtQualifiedExpression): Boolean

  def containingDeclType(expr: KtQualifiedExpression, defaultValue: String): String

  def propertyType(expr: KtProperty, defaultValue: String): String

  def fullName(expr: KtTypeAlias, defaultValue: String): String

  def aliasTypeFullName(expr: KtTypeAlias, defaultValue: String): String

  def typeFullName(expr: KtNameReferenceExpression, defaultValue: String): String

  def referenceTargetTypeFullName(expr: KtNameReferenceExpression, defaultValue: String): String

  def typeFullName(expr: KtBinaryExpression, defaultValue: String): String

  def typeFullName(expr: KtAnnotationEntry, defaultValue: String): String

  def isReferenceToClass(expr: KtNameReferenceExpression): Boolean

  def bindingKind(expr: KtQualifiedExpression): CallKind

  def anySignature(args: Seq[Any]): String

  def hasApplyOrAlsoScopeFunctionParent(expr: KtLambdaExpression): Boolean

  def isConstructorCall(expr: KtExpression): Option[Boolean]

  def typeFullName(expr: KtTypeReference, defaultValue: String): String

  def hasStaticDesc(expr: KtQualifiedExpression): Boolean

  def implicitParameterName(expr: KtLambdaExpression): Option[String]

  def isCompanionObject(expr: KtClassOrObject): Boolean

  def isRefToCompanionObject(expr: KtNameReferenceExpression): Boolean

  def typeFullName(expr: KtClassOrObject, defaultValue: String): String

  def typeFromImports(name: String, file: KtFile): Option[String]
}
