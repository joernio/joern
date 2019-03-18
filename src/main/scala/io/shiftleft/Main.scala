package org.example

import javax.script.ScriptEngineManager

object Main extends App {
  /*
  val cpg = CpgLoader.loadCodePropertyGraph("path/to/cpg.bin.zip", runEnhancements = true)

  // Print all methods starting with "<operator>"
  cpg.method.name("<operator>.*").p

  // Store all methods in local list for further processing.
  val someMethods = cpg.method.l
  */

  val scriptEngine = new ScriptEngineManager().getEngineByName("scala")
  scriptEngine.eval("val a = 19")
  scriptEngine.eval("println(a/0)")

}