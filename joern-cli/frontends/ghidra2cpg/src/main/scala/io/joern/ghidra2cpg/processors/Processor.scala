package io.joern.ghidra2cpg.processors

import scala.collection.mutable.HashMap

trait Processor {
  def getInstructions: HashMap[String, String]
}
