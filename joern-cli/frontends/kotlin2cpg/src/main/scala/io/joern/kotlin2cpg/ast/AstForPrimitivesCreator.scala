package io.joern.kotlin2cpg.ast

import io.joern.kotlin2cpg.Constants
import io.joern.kotlin2cpg.types.TypeConstants
import io.joern.kotlin2cpg.types.TypeInfoProvider
import io.joern.x2cpg.Ast
import io.joern.x2cpg.Defines
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.utils.NodeBuilders
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.NewAnnotation
import io.shiftleft.codepropertygraph.generated.nodes.NewAnnotationLiteral
import io.shiftleft.codepropertygraph.generated.nodes.NewImport
import io.shiftleft.codepropertygraph.generated.nodes.NewLocal
import io.shiftleft.codepropertygraph.generated.nodes.NewMember
import io.shiftleft.codepropertygraph.generated.nodes.NewMethodParameterIn
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import org.jetbrains.kotlin.psi.KtAnnotationEntry
import org.jetbrains.kotlin.psi.KtClassLiteralExpression
import org.jetbrains.kotlin.psi.KtConstantExpression
import org.jetbrains.kotlin.psi.KtImportDirective
import org.jetbrains.kotlin.psi.KtNameReferenceExpression
import org.jetbrains.kotlin.psi.KtStringTemplateExpression
import org.jetbrains.kotlin.psi.KtSuperExpression
import org.jetbrains.kotlin.psi.KtThisExpression
import org.jetbrains.kotlin.psi.KtTypeAlias
import org.jetbrains.kotlin.psi.KtTypeReference

import scala.annotation.unused
import scala.jdk.CollectionConverters.*
import scala.util.Try

trait AstForPrimitivesCreator(implicit withSchemaValidation: ValidationMode) {
  this: AstCreator =>

  def astForLiteral(
    expr: KtConstantExpression,
    argIdx: Option[Int],
    argName: Option[String],
    annotations: Seq[KtAnnotationEntry] = Seq()
  )(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val typeFullName   = registerType(typeInfoProvider.expressionType(expr, TypeConstants.any))
    val node           = literalNode(expr, expr.getText, typeFullName)
    val annotationAsts = annotations.map(astForAnnotationEntry)
    Ast(withArgumentName(withArgumentIndex(node, argIdx), argName)).withChildren(annotationAsts)
  }

  def astForStringTemplate(
    expr: KtStringTemplateExpression,
    argIdx: Option[Int],
    argName: Option[String],
    annotations: Seq[KtAnnotationEntry] = Seq()
  )(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val typeFullName = registerType(typeInfoProvider.expressionType(expr, TypeConstants.any))
    val outAst =
      if (expr.hasInterpolation) {
        val args = expr.getEntries.filter(_.getExpression != null).zipWithIndex.map { case (entry, idx) =>
          val entryTypeFullName = registerType(typeInfoProvider.expressionType(entry.getExpression, TypeConstants.any))
          val valueCallNode = NodeBuilders.newOperatorCallNode(
            Operators.formattedValue,
            entry.getExpression.getText,
            Option(entryTypeFullName),
            line(entry.getExpression),
            column(entry.getExpression)
          )
          val valueArgs = astsForExpression(entry.getExpression, Some(idx + 1))
          callAst(valueCallNode, valueArgs.toList)
        }
        val node =
          NodeBuilders.newOperatorCallNode(
            Operators.formatString,
            expr.getText,
            Option(typeFullName),
            line(expr),
            column(expr)
          )
        callAst(withArgumentName(withArgumentIndex(node, argIdx), argName), args.toIndexedSeq.toList)
      } else {
        val node = literalNode(expr, expr.getText, typeFullName)
        Ast(withArgumentName(withArgumentIndex(node, argIdx), argName))
      }
    outAst.withChildren(annotations.map(astForAnnotationEntry))
  }

  def astForNameReference(
    expr: KtNameReferenceExpression,
    argIdx: Option[Int],
    argName: Option[String],
    annotations: Seq[KtAnnotationEntry] = Seq()
  )(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val isReferencingMember = scope.lookupVariable(expr.getIdentifier.getText) match {
      case Some(_: NewMember) => true
      case _                  => false
    }
    val outAst =
      if (typeInfoProvider.isReferenceToClass(expr)) astForNameReferenceToType(expr, argIdx)
      else if (isReferencingMember) astForNameReferenceToMember(expr, argIdx)
      else astForNonSpecialNameReference(expr, argIdx, argName)
    outAst.withChildren(annotations.map(astForAnnotationEntry))
  }

  private def astForNameReferenceToType(expr: KtNameReferenceExpression, argIdx: Option[Int])(implicit
    typeInfoProvider: TypeInfoProvider
  ): Ast = {
    val typeFullName              = registerType(typeInfoProvider.typeFullName(expr, TypeConstants.any))
    val referencesCompanionObject = typeInfoProvider.isRefToCompanionObject(expr)
    if (referencesCompanionObject) {
      val argAsts = List(
        // TODO: change this to a TYPE_REF node as soon as the closed source data-flow engine supports it
        identifierNode(expr, expr.getIdentifier.getText, expr.getIdentifier.getText, typeFullName),
        fieldIdentifierNode(expr, Constants.companionObjectMemberName, Constants.companionObjectMemberName)
      ).map(Ast(_))
      val node = NodeBuilders.newOperatorCallNode(
        Operators.fieldAccess,
        expr.getText,
        Option(typeFullName),
        line(expr),
        column(expr)
      )
      callAst(withArgumentIndex(node, argIdx), argAsts)
    } else {
      val node = typeRefNode(expr.getIdentifier, expr.getIdentifier.getText, typeFullName)
      Ast(withArgumentIndex(node, argIdx))
    }
  }

  private def astForNameReferenceToMember(expr: KtNameReferenceExpression, argIdx: Option[Int])(implicit
    typeInfoProvider: TypeInfoProvider
  ): Ast = {
    val typeFullName = registerType(typeInfoProvider.typeFullName(expr, TypeConstants.any))
    val referenceTargetTypeFullName = registerType(
      typeInfoProvider.referenceTargetTypeFullName(expr, TypeConstants.any)
    )
    val thisNode             = identifierNode(expr, Constants.this_, Constants.this_, referenceTargetTypeFullName)
    val thisAst              = astWithRefEdgeMaybe(Constants.this_, thisNode)
    val _fieldIdentifierNode = fieldIdentifierNode(expr, expr.getReferencedName, expr.getReferencedName)
    val node = NodeBuilders.newOperatorCallNode(
      Operators.fieldAccess,
      s"${Constants.this_}.${expr.getReferencedName}",
      Option(typeFullName),
      line(expr),
      column(expr)
    )
    callAst(withArgumentIndex(node, argIdx), List(thisAst, Ast(_fieldIdentifierNode)))
  }

  private def astForNonSpecialNameReference(
    expr: KtNameReferenceExpression,
    argIdx: Option[Int],
    argName: Option[String] = None
  )(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val typeFromScopeMaybe = scope.lookupVariable(expr.getIdentifier.getText) match {
      case Some(n: NewLocal)             => Some(n.typeFullName)
      case Some(n: NewMethodParameterIn) => Some(n.typeFullName)
      case _                             => None
    }
    val typeFromProvider = typeInfoProvider.typeFullName(expr, Defines.UnresolvedNamespace)
    val typeFullName =
      typeFromScopeMaybe match {
        case Some(fullName) => registerType(fullName)
        case None           => registerType(typeFromProvider)
      }
    val name = expr.getIdentifier.getText
    val node =
      withArgumentName(withArgumentIndex(identifierNode(expr, name, name, typeFullName), argIdx), argName)
    astWithRefEdgeMaybe(name, node)
  }

  def astForSuperExpression(
    expr: KtSuperExpression,
    argIdx: Option[Int],
    argName: Option[String],
    annotations: Seq[KtAnnotationEntry] = Seq()
  )(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val typeFullName = registerType(typeInfoProvider.expressionType(expr, TypeConstants.any))
    val node = withArgumentName(
      withArgumentIndex(identifierNode(expr, expr.getText, expr.getText, typeFullName), argIdx),
      argName
    )
    astWithRefEdgeMaybe(expr.getText, node)
      .withChildren(annotations.map(astForAnnotationEntry))
  }

  def astForThisExpression(
    expr: KtThisExpression,
    argIdx: Option[Int],
    argName: Option[String],
    annotations: Seq[KtAnnotationEntry] = Seq()
  )(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val typeFullName = registerType(typeInfoProvider.expressionType(expr, TypeConstants.any))
    val node = withArgumentName(
      withArgumentIndex(identifierNode(expr, expr.getText, expr.getText, typeFullName), argIdx),
      argName
    )
    astWithRefEdgeMaybe(expr.getText, node)
      .withChildren(annotations.map(astForAnnotationEntry))
  }

  def astForClassLiteral(
    expr: KtClassLiteralExpression,
    argIdx: Option[Int],
    argName: Option[String],
    annotations: Seq[KtAnnotationEntry] = Seq()
  )(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val (fullName, signature) = typeInfoProvider.fullNameWithSignature(expr, ("", "")) // TODO: fix the fallback names
    val typeFullName          = registerType(typeInfoProvider.expressionType(expr, TypeConstants.javaLangObject))
    val node = callNode(
      expr,
      expr.getText,
      TypeConstants.classLiteralReplacementMethodName,
      fullName,
      DispatchTypes.STATIC_DISPATCH,
      Some(signature),
      Some(typeFullName)
    )
    Ast(withArgumentName(withArgumentIndex(node, argIdx), argName))
      .withChildren(annotations.map(astForAnnotationEntry))
  }

  def astForImportDirective(directive: KtImportDirective): Ast = {
    val importedAs = Try(directive.getImportedName.getIdentifier).toOption
    val isWildcard = importedAs.contains(Constants.wildcardImportName) || directive.getImportedName == null
    val node =
      NewImport()
        .isWildcard(isWildcard)
        .isExplicit(true)
        .importedAs(importedAs)
        .importedEntity(directive.getImportPath.getPathStr)
        .code(s"${Constants.importKeyword} ${directive.getImportPath.getPathStr}")
        .lineNumber(line(directive))
        .columnNumber(column(directive))
    Ast(node)
  }

  @unused
  def astForPackageDeclaration(packageName: String): Ast = {
    val node =
      if (packageName == Constants.root)
        NodeBuilders.newNamespaceBlockNode(
          NamespaceTraversal.globalNamespaceName,
          NamespaceTraversal.globalNamespaceName,
          relativizedPath
        )
      else {
        val name = packageName.split("\\.").lastOption.getOrElse("")
        NodeBuilders.newNamespaceBlockNode(name, packageName, relativizedPath)
      }
    Ast(node)
  }

  def astForAnnotationEntry(entry: KtAnnotationEntry)(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val typeFullName = registerType(typeInfoProvider.typeFullName(entry, TypeConstants.any) match {
      case value if value != TypeConstants.any => value
      case _ =>
        typeInfoProvider
          .typeFromImports(entry.getShortName.toString, entry.getContainingKtFile)
          .getOrElse(s"${Defines.UnresolvedNamespace}.${entry.getShortName.toString}")
    })

    val node =
      NewAnnotation()
        .code(entry.getText)
        .name(entry.getShortName.toString)
        .lineNumber(line(entry))
        .columnNumber(column(entry))
        .fullName(typeFullName)

    val children =
      entry.getValueArguments.asScala.flatMap { varg =>
        varg.getArgumentExpression match {
          case ste: KtStringTemplateExpression if ste.getEntries.length == 1 =>
            val node = NewAnnotationLiteral().code(ste.getText)
            Some(Ast(node))
          case ce: KtConstantExpression =>
            val node = NewAnnotationLiteral().code(ce.getText)
            Some(Ast(node))
          case _ => None
        }
      }.toList
    annotationAst(node, children)
  }

  def astForTypeAlias(typeAlias: KtTypeAlias)(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val node = typeDeclNode(
      typeAlias,
      typeAlias.getName,
      registerType(typeInfoProvider.fullName(typeAlias, TypeConstants.any)),
      relativizedPath,
      Seq(),
      Option(registerType(typeInfoProvider.aliasTypeFullName(typeAlias, TypeConstants.any)))
    )
    Ast(node)
  }

  def astForTypeReference(expr: KtTypeReference, argIdx: Option[Int], argName: Option[String])(implicit
    typeInfoProvider: TypeInfoProvider
  ): Ast = {
    val typeFullName = registerType(typeInfoProvider.typeFullName(expr, TypeConstants.any))
    val node         = typeRefNode(expr, expr.getText, typeFullName)
    Ast(withArgumentName(withArgumentIndex(node, argIdx), argName))
  }
}
