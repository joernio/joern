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

  /** Generates the fictitious class name that holds top-level statements.
    */
  def composeTopLevelClassName(fileName: String): String = {
    val sanitizedFileName = fileName.replace(java.io.File.separator, "_").replace(".", "_")
    s"${sanitizedFileName}_Program"
  }

  /** Strips the signature part from [[fullName]].
    *
    * Useful when handling nested methods, as method full names include signatures. To avoid a nested method's full name
    * containing both its parent's signature and its own, we remove the parent's signature when entering its scope.
    */
  def withoutSignature(fullName: String): String = fullName.split(':').toList match
    case fn :: sig :: Nil => fn
    case _                => fullName

}
