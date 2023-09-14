package io.shiftleft.semanticcpg.language.dotextension

import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.semanticcpg.dotgenerator.{DotCdgGenerator, DotCfgGenerator}
import overflowdb.traversal.*

class CfgNodeDot(val traversal: Iterator[Method]) extends AnyVal {

  def dotCfg: Iterator[String] = DotCfgGenerator.dotCfg(traversal)

  def dotCdg: Iterator[String] = DotCdgGenerator.dotCdg(traversal)

  def plotDotCfg(implicit viewer: ImageViewer): Unit = {
    Shared.plotAndDisplay(dotCfg.l, viewer)
  }

  def plotDotCdg(implicit viewer: ImageViewer): Unit = {
    Shared.plotAndDisplay(dotCdg.l, viewer)
  }

}
