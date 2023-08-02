package io.joern.kotlin2cpg.psi

import org.jetbrains.kotlin.com.intellij.psi.PsiElement
import org.jetbrains.kotlin.psi.{
  KtBlockExpression,
  KtCallExpression,
  KtDestructuringDeclaration,
  KtDestructuringDeclarationEntry,
  KtElement,
  KtExpression,
  KtFunctionLiteral,
  KtNamedFunction,
  KtObjectLiteralExpression,
  KtProperty
}

import scala.jdk.CollectionConverters.CollectionHasAsScala

object PsiUtils {

  def nonUnderscoreDestructuringEntries(expr: KtDestructuringDeclaration): Seq[KtDestructuringDeclarationEntry] = {
    val underscore = "_"
    expr.getEntries.asScala.filterNot(_.getText == underscore).toSeq
  }

  def objectIdxMaybe(psiElem: PsiElement, containing: PsiElement) = {
    def idxFor(bodyBlockExpression: Option[KtBlockExpression], bodyExpression: Option[KtExpression]): Option[Int] = {
      val bodyStatements =
        if (bodyBlockExpression.nonEmpty) bodyBlockExpression.get.getStatements.asScala.toSeq
        else if (bodyExpression.nonEmpty) Seq(bodyExpression.get)
        else Seq()

      val anonymousObjects =
        bodyStatements.collect {
          case pt: KtProperty =>
            pt.getDelegateExpressionOrInitializer match {
              case ol: KtObjectLiteralExpression => Some(ol.getObjectDeclaration)
              case _                             => None
            }
          case c: KtCallExpression =>
            c.getValueArguments.asScala
              .map(_.getArgumentExpression)
              .collect {
                case ol: KtObjectLiteralExpression => Some(ol.getObjectDeclaration)
                case _                             => None
              }
              .flatten
          case _ => Seq()
        }.flatten
      var outIdx: Option[Int] = None
      anonymousObjects.zipWithIndex.foreach { case (elem: PsiElement, idx) =>
        if (elem == psiElem) outIdx = Some(idx + 1)
      }
      outIdx
    }
    containing match {
      case l: KtFunctionLiteral =>
        idxFor(Option(l.getBodyBlockExpression), Option(l.getBodyExpression))
      case t: KtNamedFunction =>
        idxFor(Option(t.getBodyBlockExpression), Option(t.getBodyExpression))
      case _ => None
    }
  }
}

class PsiUtils {}
