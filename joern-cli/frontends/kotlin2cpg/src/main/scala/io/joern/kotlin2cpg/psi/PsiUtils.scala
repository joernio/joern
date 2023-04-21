package io.joern.kotlin2cpg.psi

import org.jetbrains.kotlin.psi.{KtDestructuringDeclaration, KtDestructuringDeclarationEntry}

import scala.jdk.CollectionConverters.CollectionHasAsScala

object PsiUtils {

  def nonUnderscoreDestructuringEntries(expr: KtDestructuringDeclaration): Seq[KtDestructuringDeclarationEntry] = {
    val underscore = "_"
    expr.getEntries.asScala.filterNot(_.getText == underscore).toSeq
  }
}

class PsiUtils {}
