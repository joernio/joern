package io.shiftleft

import io.shiftleft.cpgloading.tinkergraph.CpgLoader

object Main extends App {
  val cpg = CpgLoader.loadCodePropertyGraph(args(0), runEnhancements = true)

  // Print all methods starting with "<operator>"
  cpg.method.p

}