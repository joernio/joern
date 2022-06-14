package io.joern.kotlin2cpg.psi

import org.jetbrains.kotlin.com.intellij.psi.PsiElement
import org.jetbrains.kotlin.psi.{KtDestructuringDeclaration, KtDestructuringDeclarationEntry, KtNamedFunction}

import scala.jdk.CollectionConverters.CollectionHasAsScala

object PsiUtils {
  def line(element: PsiElement): Int = {
    try {
      element.getContainingFile.getViewProvider.getDocument
        .getLineNumber(element.getTextOffset) + 1
    } catch {
      case _: Throwable => -1
    }
  }

  def column(element: PsiElement): Int = {
    try {
      val lineNumber =
        element.getContainingFile.getViewProvider.getDocument
          .getLineNumber(element.getTextOffset)
      val lineOffset =
        element.getContainingFile.getViewProvider.getDocument.getLineStartOffset(lineNumber)
      element.getTextOffset - lineOffset
    } catch {
      case _: Throwable => -1
    }
  }

  def lineEnd(element: KtNamedFunction): Int = {
    line(
      Option(element.getBodyBlockExpression)
        .map(_.getRBrace)
        .getOrElse(element)
    )
  }

  def columnEnd(element: KtNamedFunction): Int = {
    column(
      Option(element.getBodyBlockExpression)
        .map(_.getRBrace)
        .getOrElse(element)
    )
  }

  def nonUnderscoreDestructuringEntries(expr: KtDestructuringDeclaration): Seq[KtDestructuringDeclarationEntry] = {
    val underscore = "_"
    expr.getEntries.asScala.filterNot(_.getText == underscore).toSeq
  }
}

class PsiUtils {}
