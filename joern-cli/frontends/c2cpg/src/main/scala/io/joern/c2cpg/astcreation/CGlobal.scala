package io.joern.c2cpg.astcreation

import io.joern.x2cpg.datastructures.Global
import java.util.concurrent.ConcurrentHashMap

object CGlobal {

  case class MethodInfo(
    name: String,
    code: String,
    fileName: String,
    returnType: String,
    astParentType: String,
    astParentFullName: String,
    lineNumber: Option[Int],
    columnNumber: Option[Int],
    lineNumberEnd: Option[Int],
    columnNumberEnd: Option[Int],
    signature: String,
    offset: Option[(Int, Int)],
    parameter: Seq[ParameterInfo],
    modifier: Seq[String]
  )
  case class ParameterInfo(
    name: String,
    var code: String,
    index: Int,
    var isVariadic: Boolean,
    evaluationStrategy: String,
    lineNumber: Option[Int],
    columnNumber: Option[Int],
    typeFullName: String
  )

}

class CGlobal extends Global {
  import io.joern.c2cpg.astcreation.CGlobal.MethodInfo

  val methodDeclarations: ConcurrentHashMap[String, MethodInfo] = new ConcurrentHashMap()
  val methodDefinitions: ConcurrentHashMap[String, Boolean]     = new ConcurrentHashMap()

}
