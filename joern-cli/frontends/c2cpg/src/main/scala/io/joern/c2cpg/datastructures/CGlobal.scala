package io.joern.c2cpg.datastructures

import io.joern.c2cpg.astcreation.Defines
import io.joern.x2cpg.datastructures.Global

import scala.jdk.CollectionConverters._

object CGlobal extends Global {

  def typesSeen(): List[String] = {
    val types = usedTypes.keys().asScala.filterNot(_ == Defines.anyTypeName).toList
    usedTypes.clear()
    types
  }

}
