package io.joern.csharpsrc2cpg.astcreation

import io.joern.csharpsrc2cpg.datastructures.FieldDecl
import io.joern.csharpsrc2cpg.parser.DotNetJsonAst.*
import io.joern.csharpsrc2cpg.parser.{DotNetNodeInfo, ParserKeys}
import io.joern.x2cpg.{Ast, Defines, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.{DeclarationNew, NewCall, NewLocal}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}

trait AstForPrimitivesCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def astForIdentifier(ident: DotNetNodeInfo, typeFullName: String = Defines.Any): Ast = {
    val identifierName = nameFromNode(ident)
    if identifierName != "_" then {
      List(scope.lookupVariable(identifierName), scope.findFieldInScope(identifierName)) match {
        case List(Some(variable: DeclarationNew), None) =>
          val node = identifierFromDecl(variable, Option(ident))
          Ast(node).withRefEdge(node, variable)
        case List(None, Some(field: FieldDecl)) =>
          Ast(identifierNode(ident, field.name, ident.code, field.typeFullName))
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
