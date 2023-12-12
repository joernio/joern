package io.joern.csharpsrc2cpg.astcreation

import io.joern.csharpsrc2cpg.parser.DotNetJsonAst.{IdentifierName, QualifiedName}
import io.joern.csharpsrc2cpg.parser.{DotNetNodeInfo, ParserKeys}
import io.joern.x2cpg.datastructures.Stack.StackWrapper
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.NewNamespaceBlock

trait AstForDeclarationsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def astForNamespaceDeclaration(namespace: DotNetNodeInfo): Ast = {
    val nameNode = createDotNetNodeInfo(namespace.json(ParserKeys.Name))
    val fullName = methodAstParentStack.headOption match
      case Some(head: NewNamespaceBlock) => s"${head.fullName}.${stringFromIdentifierNode(nameNode)}"
      case _                             => stringFromIdentifierNode(nameNode)
    val name = fullName.split('.').filterNot(_.isBlank).lastOption.getOrElse(fullName)
    val namespaceBlock = NewNamespaceBlock()
      .name(name)
      .code(code(nameNode))
      .lineNumber(line(nameNode))
      .columnNumber(columnEnd(nameNode))
      .filename(relativeFileName)
      .fullName(fullName)
    methodAstParentStack.push(namespaceBlock)
    val memberAsts = namespace.json(ParserKeys.Members).arr.map(astForNode).toSeq
    methodAstParentStack.pop()
    Ast(namespaceBlock)
      .withChildren(memberAsts)
  }

  protected def stringFromIdentifierNode(identifierNode: DotNetNodeInfo): String = {
    identifierNode.node match
      case IdentifierName => stringFromIdentifierName(identifierNode)
      case QualifiedName  => stringFromQualifiedName(identifierNode)
      case _              => "<empty>"
  }

  protected def stringFromIdentifierName(identifierName: DotNetNodeInfo): String = {
    identifierName.json(ParserKeys.Identifier).obj(ParserKeys.Value).str
  }

  protected def stringFromQualifiedName(qualifiedName: DotNetNodeInfo): String = {
    val rhs = stringFromIdentifierNode(createDotNetNodeInfo(qualifiedName.json(ParserKeys.Right)))
    val lhs = stringFromIdentifierNode(createDotNetNodeInfo(qualifiedName.json(ParserKeys.Left)))
    s"$lhs.$rhs"
  }

}
