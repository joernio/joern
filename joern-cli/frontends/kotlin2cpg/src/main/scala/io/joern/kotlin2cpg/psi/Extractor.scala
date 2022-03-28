package io.joern.kotlin2cpg.psi

import org.jetbrains.kotlin.com.intellij.psi.PsiElement

object Extractor {
  def line(element: PsiElement): Int = {
    try {
      element.getContainingFile.getViewProvider.getDocument
        .getLineNumber(element.getTextOffset)
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

}

class Extractor {}
