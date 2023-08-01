package io.joern.kotlin2cpg.psi

import org.jetbrains.kotlin.com.intellij.psi.PsiElement
import org.jetbrains.kotlin.psi.{
  KtCallExpression,
  KtDestructuringDeclaration,
  KtDestructuringDeclarationEntry,
  KtElement,
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

  def objectIdxMaybe(psiElem: PsiElement, containing: PsiElement) =
    containing match {
      case t: KtNamedFunction =>
        val bodyStatements =
          if (t.getBodyBlockExpression != null) t.getBodyBlockExpression.getStatements.asScala.toSeq
          else if (t.getBodyExpression != null) Seq(t.getBodyExpression)
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
      case _ => None
    }
}

class PsiUtils {}
