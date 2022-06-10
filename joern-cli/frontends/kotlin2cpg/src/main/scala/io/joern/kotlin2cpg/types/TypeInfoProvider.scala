package io.joern.kotlin2cpg.types

import io.shiftleft.passes.KeyPool
import org.jetbrains.kotlin.psi.{
  KtBinaryExpression,
  KtCallExpression,
  KtClassLiteralExpression,
  KtClassOrObject,
  KtDestructuringDeclarationEntry,
  KtExpression,
  KtLambdaArgument,
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

trait TypeInfoProvider {
  def isStaticMethodCall(expr: KtQualifiedExpression): Boolean

  def returnType(elem: KtNamedFunction, defaultValue: String): String

  def containingDeclFullName(expr: KtCallExpression): Option[String]

  def containingDeclType(expr: KtQualifiedExpression, defaultValue: String): String

  def expressionType(expr: KtExpression, defaultValue: String): String

  def inheritanceTypes(expr: KtClassOrObject, or: Seq[String]): Seq[String]

  def parameterType(expr: KtParameter, defaultValue: String): String

  def propertyType(expr: KtProperty, defaultValue: String): String

  def fullName(expr: KtClassOrObject, defaultValue: String): String

  def fullName(expr: KtTypeAlias, defaultValue: String): String

  def fullNameWithSignature(expr: KtDestructuringDeclarationEntry, defaultValue: (String, String)): (String, String)

  def aliasTypeFullName(expr: KtTypeAlias, defaultValue: String): String

  def typeFullName(expr: KtNameReferenceExpression, defaultValue: String): String

  def referenceTargetTypeFullName(expr: KtNameReferenceExpression, defaultValue: String): String

  def typeFullName(expr: KtBinaryExpression, defaultValue: String): String

  def isReferenceToClass(expr: KtNameReferenceExpression): Boolean

  def bindingKind(expr: KtQualifiedExpression): CallKinds.CallKind

  def isReferencingMember(expr: KtNameReferenceExpression): Boolean

  def fullNameWithSignature(expr: KtQualifiedExpression, or: (String, String)): (String, String)

  def fullNameWithSignature(call: KtCallExpression, or: (String, String)): (String, String)

  def fullNameWithSignature(expr: KtPrimaryConstructor, or: (String, String)): (String, String)

  def fullNameWithSignature(expr: KtSecondaryConstructor, or: (String, String)): (String, String)

  def fullNameWithSignature(call: KtBinaryExpression, or: (String, String)): (String, String)

  def fullNameWithSignature(expr: KtNamedFunction, or: (String, String)): (String, String)

  def fullNameWithSignature(expr: KtClassLiteralExpression, or: (String, String)): (String, String)

  def fullNameWithSignature(expr: KtLambdaExpression, keyPool: KeyPool): (String, String)

  def anySignature(args: Seq[Any]): String

  def returnTypeFullName(expr: KtLambdaExpression): String

  def nameReferenceKind(expr: KtNameReferenceExpression): NameReferenceKinds.NameReferenceKind

  def isConstructorCall(expr: KtCallExpression): Option[Boolean]

  def typeFullName(expr: KtTypeReference, defaultValue: String): String

  def typeFullName(expr: KtPrimaryConstructor, defaultValue: String): String

  def typeFullName(expr: KtSecondaryConstructor, defaultValue: String): String

  def typeFullName(expr: KtCallExpression, defaultValue: String): String

  def typeFullName(expr: KtParameter, defaultValue: String): String

  def typeFullName(expr: KtDestructuringDeclarationEntry, defaultValue: String): String

  def hasStaticDesc(expr: KtQualifiedExpression): Boolean

  def implicitParameterName(expr: KtLambdaExpression): Option[String]

  def isCompanionObject(expr: KtClassOrObject): Boolean

  def isRefToCompanionObject(expr: KtNameReferenceExpression): Boolean

  def typeFullName(expr: KtClassOrObject, defaultValue: String): String
}
