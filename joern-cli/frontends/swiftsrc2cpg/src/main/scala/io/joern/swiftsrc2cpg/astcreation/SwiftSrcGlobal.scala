package io.joern.swiftsrc2cpg.astcreation

import io.joern.x2cpg.datastructures.Global

import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable

class SwiftSrcGlobal extends Global {

  /** Mapping from extension fullName to the set of names it inherits from. */
  val extensionInherits: ConcurrentHashMap[String, mutable.HashSet[String]] = new ConcurrentHashMap()

  /** Mapping from extension method fullName (provided by the compiler) to the fullName the frontend generates for
    * fullName uniqueness.
    */
  val extensionMethodFullNameMapping: ConcurrentHashMap[String, String] = new ConcurrentHashMap()

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

  def addExtensionMethodFullName(extensionMethodFullName: String, fullName: String): Unit = {
    extensionMethodFullNameMapping.putIfAbsent(extensionMethodFullName, fullName)
  }

}
