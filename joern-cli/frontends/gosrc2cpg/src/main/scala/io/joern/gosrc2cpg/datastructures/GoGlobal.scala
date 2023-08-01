package io.joern.gosrc2cpg.datastructures

import io.joern.gosrc2cpg.astcreation.Defines
import io.joern.x2cpg.datastructures.Global

import scala.jdk.CollectionConverters.EnumerationHasAsScala

object GoGlobal extends Global {

  def typesSeen(): List[String] = {
    val types = usedTypes.keys().asScala.filterNot(_ == Defines.anyTypeName).toList
    usedTypes.clear()
    types
  }

}
