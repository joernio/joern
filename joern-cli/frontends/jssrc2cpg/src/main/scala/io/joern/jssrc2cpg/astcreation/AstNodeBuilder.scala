package io.joern.jssrc2cpg.astcreation

import io.shiftleft.codepropertygraph.generated.nodes._
import ujson.Value

trait AstNodeBuilder {

  this: AstCreator =>

  protected def newUnknown(node: Value, order: Int): NewUnknown =
    NewUnknown()
      .parserTypeName(nodeType(node).toString)
      .code(code(node))
      .order(order)
      .argumentIndex(order)
      .lineNumber(line(node))
      .columnNumber(column(node))

  protected def newTypeDecl(
    name: String,
    fullname: String,
    filename: String,
    code: String,
    astParentType: String = "",
    astParentFullName: String = "",
    order: Int = -1,
    inherits: Seq[String] = Seq.empty,
    alias: Option[String] = None,
    line: Option[Integer] = None,
    column: Option[Integer] = None
  ): NewTypeDecl =
    NewTypeDecl()
      .name(name)
      .fullName(fullname)
      .code(code)
      .isExternal(false)
      .filename(filename)
      .astParentType(astParentType)
      .astParentFullName(astParentFullName)
      .inheritsFromTypeFullName(inherits)
      .aliasTypeFullName(alias)
      .lineNumber(line)
      .columnNumber(column)
      .order(order)

}
