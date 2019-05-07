package io.shiftleft.joern

object Main extends App {
 val cpg = CpgLoader.load(args(0))
  println(cpg.method.name.p)
}
