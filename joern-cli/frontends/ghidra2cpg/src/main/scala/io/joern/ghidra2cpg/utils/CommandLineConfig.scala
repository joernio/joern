package io.joern.ghidra2cpg.utils

object CommandLineConfig {
  // we need to mimic the cli headless call
  // therefor we have to provide the same
  // arguments
  val projectDirectory = "src/test/resources/ghidraworkingdirectory"
  val projectName      = "defaultProject"
  // we overwrite on a rerun
  val overwrite    = "-overwrite"
  val importFlag   = "-import"
  val pathToBinary = "src/test/resources/test"

  val outputFile = "cpg.bin"
}
