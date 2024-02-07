package io.joern.csharpsrc2cpg.utils

object Utils {
  def composeMethodLikeSignature(returnType: String, parameterTypes: collection.Seq[String]): String = {
    s"$returnType(${parameterTypes.mkString(",")})"
  }

  def composeMethodFullName(typeDeclFullName: String, name: String, signature: String): String = {
    s"$typeDeclFullName.$name:$signature"
  }

}
