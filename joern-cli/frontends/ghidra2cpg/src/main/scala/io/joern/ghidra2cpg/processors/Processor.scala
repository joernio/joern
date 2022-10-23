package io.joern.ghidra2cpg.processors

import scala.collection.mutable

trait Processor {
  def getInstructions: mutable.HashMap[String, String]
}
