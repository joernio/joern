package io.joern.swiftsrc2cpg.astcreation

import io.joern.x2cpg.datastructures.Global

import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable

class SwiftSrcGlobal extends Global {
  val extensionInherits: ConcurrentHashMap[String, mutable.HashSet[String]] = new ConcurrentHashMap()

  def addExtensionInherits(extensionFullName: String, inheritNames: Seq[String]): Unit = {
    extensionInherits.compute(
      extensionFullName,
      (_, previousSet) => {
        if (previousSet == null) {
          mutable.HashSet.from(inheritNames)
        } else {
          previousSet.addAll(inheritNames)
        }
      }
    )
  }

}
