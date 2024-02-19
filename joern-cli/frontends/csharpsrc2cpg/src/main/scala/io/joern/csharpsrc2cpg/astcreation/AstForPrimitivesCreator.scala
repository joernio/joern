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
    println(List(scope.lookupVariable(identifierName), scope.findFieldInScope(identifierName)))
    if identifierName != "_" then {
      List(scope.lookupVariable(identifierName), scope.findFieldInScope(identifierName)) match {
        case List(Some(_localNode: NewLocal), Some(fieldDecl: FieldDecl)) =>
          val node = identifierFromDecl(_localNode, Option(ident))
          Ast(node).withRefEdge(node, _localNode)
        case List(Some(variable: DeclarationNew), None) =>
          val node = identifierFromDecl(variable, Option(ident))
          Ast(node).withRefEdge(node, variable)
        case List(None, Some(field: FieldDecl)) if field.node.node != DotNetJsonAst.VariableDeclarator =>
          val fieldAccess =
            newOperatorCallNode(
              Operators.fieldAccess,
              field.node.code,
              Some(field.typeFullName).orElse(Option(typeFullName)),
              field.node.lineNumber,
              field.node.columnNumber
            )
          val identifierAst = Ast(newIdentifierNode(identifierName, typeFullName))
          val fieldIdentifier = Ast(
            NewFieldIdentifier()
              .code(field.name)
              .canonicalName(field.name)
              .lineNumber(field.node.lineNumber)
              .columnNumber(field.node.columnNumber)
          )
          callAst(fieldAccess, Seq(identifierAst, fieldIdentifier))
        case _ =>
          Ast(identifierNode(ident, identifierName, ident.code, typeFullName))
      }
    } else {
      Ast()
    }
  }

  protected def astForUsing(usingNode: DotNetNodeInfo): Ast = {
    val namespace  = nameFromNode(usingNode)
    val alias      = namespace.split('.').last
    val importNode = newImportNode(code(usingNode), namespace, alias, usingNode)
    scope.addImport(namespace)
    Ast(importNode)
  }

}
