package io.joern.c2cpg.astcreation

import io.joern.x2cpg.datastructures.Global
import java.util.concurrent.ConcurrentHashMap

object CGlobal {

  final case class MethodInfo(
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
  final class ParameterInfo(
    val name: String,
    var code: String,
    val index: Int,
    var isVariadic: Boolean,
    val evaluationStrategy: String,
    val lineNumber: Option[Int],
    val columnNumber: Option[Int],
    val typeFullName: String
  )

}

class CGlobal extends Global {
  import io.joern.c2cpg.astcreation.CGlobal.MethodInfo

  val methodDeclarations: ConcurrentHashMap[String, MethodInfo] = new ConcurrentHashMap()
  val methodDefinitions: ConcurrentHashMap[String, Boolean]     = new ConcurrentHashMap()

}
