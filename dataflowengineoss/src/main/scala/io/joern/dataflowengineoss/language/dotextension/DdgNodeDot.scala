package io.joern.dataflowengineoss.language.dotextension

import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.dotgenerator.{DotCpg14Generator, DotDdgGenerator, DotPdgGenerator}
import io.joern.dataflowengineoss.language.*
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.dotextension.{ImageViewer, Shared}

class DdgNodeDot(val traversal: Iterator[Method]) extends AnyVal {

  def dotDdg(implicit semantics: Semantics = DefaultSemantics()): Iterator[String] =
    DotDdgGenerator.toDotDdg(traversal)

  def dotPdg(implicit semantics: Semantics = DefaultSemantics()): Iterator[String] =
    DotPdgGenerator.toDotPdg(traversal)

  def dotCpg14(implicit semantics: Semantics = DefaultSemantics()): Iterator[String] =
    DotCpg14Generator.toDotCpg14(traversal)

  def plotDotDdg(implicit viewer: ImageViewer, semantics: Semantics = DefaultSemantics()): Unit = {
    Shared.plotAndDisplay(traversal.dotDdg.l, viewer)
  }

  def plotDotPdg(implicit viewer: ImageViewer, semantics: Semantics = DefaultSemantics()): Unit = {
    Shared.plotAndDisplay(traversal.dotPdg.l, viewer)
  }

  def plotDotCpg14(implicit viewer: ImageViewer, semantics: Semantics = DefaultSemantics()): Unit = {
    Shared.plotAndDisplay(traversal.dotCpg14.l, viewer)
  }

}
