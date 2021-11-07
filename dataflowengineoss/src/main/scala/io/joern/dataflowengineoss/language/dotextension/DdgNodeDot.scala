package io.joern.dataflowengineoss.language.dotextension

import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.joern.dataflowengineoss.dotgenerator.{DotCpg14Generator, DotDdgGenerator, DotPdgGenerator}
import io.joern.dataflowengineoss.language._
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.shiftleft.semanticcpg.language.dotextension.{ImageViewer, Shared}
import overflowdb.traversal.Traversal

class DdgNodeDot(val traversal: Traversal[Method]) extends AnyVal {

  def dotDdg(implicit semantics: Semantics): Traversal[String] = DotDdgGenerator.toDotDdg(traversal)

  def dotPdg(implicit semantics: Semantics): Traversal[String] = DotPdgGenerator.toDotPdg(traversal)

  def dotCpg14(implicit semantics: Semantics): Traversal[String] = DotCpg14Generator.toDotCpg14(traversal)

  def plotDotDdg(implicit viewer: ImageViewer, semantics: Semantics): Unit = {
    Shared.plotAndDisplay(traversal.dotDdg.l, viewer)
  }

  def plotDotPdg(implicit viewer: ImageViewer, semantics: Semantics): Unit = {
    Shared.plotAndDisplay(traversal.dotPdg.l, viewer)
  }

  def plotDotCpg14(implicit viewer: ImageViewer, semantics: Semantics): Unit = {
    Shared.plotAndDisplay(traversal.dotCpg14.l, viewer)
  }

}
