package io.shiftleft

import io.shiftleft.cpgloading.tinkergraph.CpgLoader
import io.shiftleft.queryprimitives.steps.Implicits._
import io.shiftleft.passes.dataflows._

object Main extends App {
  val cpg = CpgLoader.loadCodePropertyGraph(args(0), runEnhancements = true)

  // Print all methods starting with "<operator>"
  cpg.method.p

}