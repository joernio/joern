package io.joern.csharpsrc2cpg.astcreation

import io.joern.csharpsrc2cpg.datastructures.FieldDecl
import io.joern.csharpsrc2cpg.parser.DotNetJsonAst.*
import io.joern.csharpsrc2cpg.parser.{DotNetJsonAst, DotNetNodeInfo, ParserKeys}
import io.joern.x2cpg.utils.NodeBuilders.{newIdentifierNode, newOperatorCallNode}
import io.joern.x2cpg.{Ast, Defines, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.{DeclarationNew, NewCall, NewFieldIdentifier, NewLocal}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}

trait AstForPrimitivesCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def astForIdentifier(ident: DotNetNodeInfo, typeFullName: String = Defines.Any): Ast = {
    val identifierName = nameFromNode(ident)
    if identifierName != "_" then {
      scope.lookupVariable(identifierName) match {
        case Some(variable: DeclarationNew) =>
          val node = identifierFromDecl(variable, Option(ident))
          Ast(node).withRefEdge(node, variable)
        case None =>
          scope.findFieldInScope(identifierName) match {
            // Check for implicit field reference
            case Some(field) if field.node.node != DotNetJsonAst.VariableDeclarator =>
              astForFieldIdentifier(typeFullName, identifierName, field)
            case Some(field) =>
              Ast(identifierNode(ident, identifierName, identifierName, field.typeFullName))
            case None =>
              // Check for static type reference
              scope.tryResolveTypeReference(identifierName) match {
                case Some(typeReference) if typeFullName == Defines.Any =>
                  Ast(identifierNode(ident, identifierName, ident.code, typeReference.name))
                case _ =>
                  Ast(identifierNode(ident, identifierName, ident.code, typeFullName))
              }
          }
      }
    } else {
      Ast()
    }
  }

  private def astForFieldIdentifier(baseTypeFullName: String, baseIdentifierName: String, field: FieldDecl) = {
    val fieldAccess =
      newOperatorCallNode(
        Operators.fieldAccess,
        field.node.code,
        Some(field.typeFullName),
        field.node.lineNumber,
        field.node.columnNumber
      )
    val identifierAst = Ast(newIdentifierNode(baseIdentifierName, baseTypeFullName))
    val fieldIdentifier = Ast(
      NewFieldIdentifier()
        .code(field.name)
        .canonicalName(field.name)
        .lineNumber(field.node.lineNumber)
        .columnNumber(field.node.columnNumber)
    )
    callAst(fieldAccess, Seq(identifierAst, fieldIdentifier))
  }

  protected def astForUsing(usingNode: DotNetNodeInfo): Ast = {
    val namespace  = nameFromNode(usingNode)
    val alias      = namespace.split('.').last
    val importNode = newImportNode(code(usingNode), namespace, alias, usingNode)
    scope.addImportedNamespace(namespace)
    scope.addImportedTypeOrModule(namespace) // We cannot determine if the namespace refers to a type so we do both
    Ast(importNode)
  }

}
