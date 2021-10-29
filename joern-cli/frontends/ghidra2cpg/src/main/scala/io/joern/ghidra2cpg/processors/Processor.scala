package io.joern.ghidra2cpg.processors

import scala.collection.immutable._
trait Processor {
  def getInstructions: HashMap[String, String]
}
