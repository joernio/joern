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
  def isExtensionFn(fn: KtNamedFunction): Boolean

  def usedAsExpression(expr: KtExpression): Option[Boolean]

  def containingTypeDeclFullName(ktFn: KtNamedFunction, defaultValue: String): String

  def isStaticMethodCall(expr: KtQualifiedExpression): Boolean

  def visibility(fn: KtNamedFunction): Option[DescriptorVisibility]

  def modality(fn: KtNamedFunction): Option[Modality]

  def modality(ktClass: KtClassOrObject): Option[Modality]

  def returnType(elem: KtNamedFunction, defaultValue: String): String

  def containingDeclFullName(expr: KtCallExpression): Option[String]

  def containingDeclType(expr: KtQualifiedExpression, defaultValue: String): String

  def expressionType(expr: KtExpression, defaultValue: String): String

  def inheritanceTypes(expr: KtClassOrObject, or: Seq[String]): Seq[String]

  def parameterType(expr: KtParameter, defaultValue: String): String

  def destructuringEntryType(expr: KtDestructuringDeclarationEntry, defaultValue: String): String

  def propertyType(expr: KtProperty, defaultValue: String): String

  def fullName(expr: KtClassOrObject, defaultValue: String, ctx: Option[AnonymousObjectContext] = None): String

  def fullName(expr: KtTypeAlias, defaultValue: String): String

  def fullNameWithSignature(expr: KtDestructuringDeclarationEntry, defaultValue: (String, String)): (String, String)

  def aliasTypeFullName(expr: KtTypeAlias, defaultValue: String): String

  def typeFullName(expr: KtNameReferenceExpression, defaultValue: String): String

  def referenceTargetTypeFullName(expr: KtNameReferenceExpression, defaultValue: String): String

  def typeFullName(expr: KtBinaryExpression, defaultValue: String): String

  def typeFullName(expr: KtAnnotationEntry, defaultValue: String): String

  def isReferenceToClass(expr: KtNameReferenceExpression): Boolean

  def bindingKind(expr: KtQualifiedExpression): CallKind

  def fullNameWithSignature(expr: KtQualifiedExpression, or: (String, String)): (String, String)

  def fullNameWithSignature(call: KtCallExpression, or: (String, String)): (String, String)

  def fullNameWithSignature(expr: KtPrimaryConstructor, or: (String, String)): (String, String)

  def fullNameWithSignature(expr: KtSecondaryConstructor, or: (String, String)): (String, String)

  def fullNameWithSignature(call: KtBinaryExpression, or: (String, String)): (String, String)

  def fullNameWithSignature(expr: KtNamedFunction, or: (String, String)): (String, String)

  def fullNameWithSignatureAsLambda(expr: KtNamedFunction, lambdaName: String): (String, String)

  def fullNameWithSignature(expr: KtClassLiteralExpression, or: (String, String)): (String, String)

  def fullNameWithSignature(expr: KtLambdaExpression, lambdaName: String): (String, String)

  def anySignature(args: Seq[Any]): String

  def returnTypeFullName(expr: KtLambdaExpression): String

  def hasApplyOrAlsoScopeFunctionParent(expr: KtLambdaExpression): Boolean

  def nameReferenceKind(expr: KtNameReferenceExpression): NameReferenceKind

  def isConstructorCall(expr: KtExpression): Option[Boolean]

  def typeFullName(expr: KtTypeReference, defaultValue: String): String

  def typeFullName(expr: KtPrimaryConstructor | KtSecondaryConstructor, defaultValue: String): String

  def typeFullName(expr: KtCallExpression, defaultValue: String): String

  def typeFullName(expr: KtParameter, defaultValue: String): String

  def typeFullName(typ: KotlinType): String

  def typeFullName(expr: KtDestructuringDeclarationEntry, defaultValue: String): String

  def hasStaticDesc(expr: KtQualifiedExpression): Boolean

  def implicitParameterName(expr: KtLambdaExpression): Option[String]

  def isCompanionObject(expr: KtClassOrObject): Boolean

  def isRefToCompanionObject(expr: KtNameReferenceExpression): Boolean

  def typeFullName(expr: KtClassOrObject, defaultValue: String): String

  def typeFromImports(name: String, file: KtFile): Option[String]
}
