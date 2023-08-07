package io.joern.kotlin2cpg.psi

import org.jetbrains.kotlin.com.intellij.psi.PsiElement
import org.jetbrains.kotlin.psi.{
  KtDestructuringDeclaration,
  KtDestructuringDeclarationEntry,
  KtElement,
  KtObjectDeclaration,
  KtTreeVisitorVoid
}

import scala.jdk.CollectionConverters.CollectionHasAsScala

object PsiUtils {

  def nonUnderscoreDestructuringEntries(expr: KtDestructuringDeclaration): Seq[KtDestructuringDeclarationEntry] = {
    val underscore = "_"
    expr.getEntries.asScala.filterNot(_.getText == underscore).toSeq
  }

  def objectIdxMaybe(psiElem: PsiElement, containing: PsiElement) = {
    class ForEachTreeVisitor(block: (KtElement) => Unit) extends KtTreeVisitorVoid {
      override def visitKtElement(element: KtElement): Unit = {
        if (element != null) {
          super.visitKtElement(element)
          block(element)
        }
      }
    }

    val buf = scala.collection.mutable.ListBuffer.empty[KtObjectDeclaration]
    val visitor =
      new ForEachTreeVisitor({ elem =>
        elem match {
          case e: KtObjectDeclaration =>
            buf.append(e)
          case _ =>
        }
      })
    visitor.visitKtElement(containing.asInstanceOf[KtElement])
    var outIdx: Option[Int] = None
    buf.zipWithIndex.foreach { case (elem: PsiElement, idx) =>
      if (elem == psiElem) outIdx = Some(idx + 1)
    }
    outIdx
  }
}

class PsiUtils {}
