package io.shiftleft.semanticcpg

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.Properties
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language._

object Overlays {

  def appendOverlayName(cpg: Cpg, overlayName: String): Unit = {
    new CpgPass(cpg) {
      override def run(diffGraph: DiffGraphBuilder): Unit = {
        cpg.metaData.headOption match {
          case Some(metaData) =>
            val newValue = metaData.overlays :+ overlayName
            diffGraph.setNodeProperty(metaData, Properties.Overlays.name, newValue)
          case None =>
            System.err.println("Missing metaData block")
        }
      }
    }.createAndApply()
  }

  def removeLastOverlayName(cpg: Cpg): Unit = {
    new CpgPass(cpg) {
      override def run(diffGraph: DiffGraphBuilder): Unit = {
        cpg.metaData.headOption match {
          case Some(metaData) =>
            val newValue = metaData.overlays.dropRight(1)
            diffGraph.setNodeProperty(metaData, Properties.Overlays.name, newValue)
          case None =>
            System.err.println("Missing metaData block")
        }
      }
    }.createAndApply()
  }

  def appliedOverlays(cpg: Cpg): Seq[String] = {
    cpg.metaData.headOption match {
      case Some(metaData) => Option(metaData.overlays).getOrElse(Nil)
      case None =>
        System.err.println("Missing metaData block")
        List()
    }
  }

}
