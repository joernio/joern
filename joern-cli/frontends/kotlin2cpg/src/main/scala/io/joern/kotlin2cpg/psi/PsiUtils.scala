package io.joern.kotlin2cpg.psi

import org.jetbrains.kotlin.com.intellij.psi.PsiElement
import org.jetbrains.kotlin.psi.KtNamedFunction

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
}

class PsiUtils {}
