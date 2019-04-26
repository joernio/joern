package io.shiftleft

import io.shiftleft.cpgloading.CpgLoader


object Main extends App {
  val cpg = CpgLoader.loadCodePropertyGraph(args(0))
  implicit val graph = cpg.graph

  println("------------------")

  // Print all method names
  println(cpg.method.name.p)

}

