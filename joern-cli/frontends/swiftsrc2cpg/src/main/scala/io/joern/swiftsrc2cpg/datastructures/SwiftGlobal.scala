package io.joern.swiftsrc2cpg.datastructures

import io.joern.swiftsrc2cpg.passes.Defines
import io.joern.x2cpg.datastructures.Global

import scala.jdk.CollectionConverters._

object SwiftGlobal extends Global {

  def typesSeen(): List[String] = this.synchronized {
    val types = usedTypes.keys().asScala.filterNot(_ == Defines.Any).toList
    usedTypes.clear()
    types
  }

}
