package io.joern.csharpsrc2cpg.datastructures

import java.util.concurrent.ConcurrentHashMap

class CSharpGlobal {
  // Mapping method fullname to its return type and signature
  val methodFullNameReturnTypeMap: ConcurrentHashMap[String, (String, String)] = new ConcurrentHashMap()
}
