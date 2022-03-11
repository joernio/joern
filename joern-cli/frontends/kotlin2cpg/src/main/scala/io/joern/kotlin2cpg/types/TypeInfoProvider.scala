package io.joern.kotlin2cpg.types

import io.shiftleft.passes.KeyPool
import org.jetbrains.kotlin.psi.{
  KtBinaryExpression,
  KtCallExpression,
  KtClassLiteralExpression,
  KtClassOrObject,
  KtDestructuringDeclarationEntry,
  KtExpression,
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
  def returnType(elem: KtNamedFunction, or: String): String

  def containingDeclType(expr: KtQualifiedExpression, or: String): String

  def expressionType(expr: KtExpression, or: String): String

  def inheritanceTypes(expr: KtClassOrObject, or: Seq[String]): Seq[String]

  def parameterType(expr: KtParameter, or: String): String

  def propertyType(expr: KtProperty, or: String): String

  def fullName(expr: KtClassOrObject, or: String): String

  def fullName(expr: KtTypeAlias, or: String): String

  def aliasTypeFullName(expr: KtTypeAlias, or: String): String

  def typeFullName(expr: KtNameReferenceExpression, or: String): String

  def referenceTargetTypeFullName(expr: KtNameReferenceExpression, or: String): String

  def typeFullName(expr: KtBinaryExpression, defaultValue: String): String

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

  def erasedSignature(args: Seq[Any]): String

  def returnTypeFullName(expr: KtLambdaExpression): String

  def nameReferenceKind(expr: KtNameReferenceExpression): NameReferenceKinds.NameReferenceKind

  def isConstructorCall(expr: KtCallExpression): Option[Boolean]

  def typeFullName(expr: KtTypeReference, or: String): String

  def typeFullName(expr: KtPrimaryConstructor, or: String): String

  def typeFullName(expr: KtSecondaryConstructor, or: String): String

  def typeFullName(expr: KtCallExpression, or: String): String

  def typeFullName(expr: KtDestructuringDeclarationEntry, defaultValue: String): String

  def hasStaticDesc(expr: KtQualifiedExpression): Boolean
}
