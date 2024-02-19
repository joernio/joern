package io.joern.csharpsrc2cpg.astcreation

import io.joern.csharpsrc2cpg.parser.DotNetJsonAst.*
import io.joern.csharpsrc2cpg.parser.{DotNetNodeInfo, ParserKeys}
import io.joern.x2cpg.{Ast, Defines, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.{NewCall, NewLocal}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}

trait AstForPrimitivesCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def astForIdentifier(ident: DotNetNodeInfo, typeFullName: String = Defines.Any): Ast = {
    val identifierName = nameFromNode(ident)
    if identifierName != "_" then {
      scope.lookupVariable(identifierName) match {
        case Some(variable) =>
          val node = identifierFromDecl(variable, Option(ident))
          Ast(node).withRefEdge(node, variable)
        case None =>
          // If not a variable, could perhaps be a static type reference
          scope.tryResolveTypeReference(identifierName) match {
            case Some(typeReference) =>
              Ast(identifierNode(ident, identifierName, ident.code, typeReference.name))
            case None =>
              Ast(identifierNode(ident, identifierName, ident.code, typeFullName))
          }
      }
    } else {
      Ast()
    }
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
