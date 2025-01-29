package io.joern.csharpsrc2cpg.utils

import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.nodes.NewMethodParameterIn

object Utils {
  def composeMethodLikeSignature(returnType: String, parameterTypes: collection.Seq[String]): String = {
    s"$returnType(${parameterTypes.mkString(",")})"
  }

  def composeMethodLikeSignature(returnType: String, parameters: Seq[Ast] = Nil): String = {
    composeMethodLikeSignature(
      returnType,
      parameters.flatMap(_.nodes.collectFirst { case x: NewMethodParameterIn => x.typeFullName })
    )
  }

  def composeMethodFullName(typeDeclFullName: String, name: String, signature: String): String = {
    s"$typeDeclFullName.$name:$signature"
  }

  def composeGetterName(fieldIdentifierName: String): String = s"get_$fieldIdentifierName"

  def composeSetterName(fieldIdentifierName: String): String = s"set_$fieldIdentifierName"

}
