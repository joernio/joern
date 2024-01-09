package io.joern.csharpsrc2cpg.astcreation

import io.joern.csharpsrc2cpg.parser.DotNetJsonAst.*
import io.joern.csharpsrc2cpg.parser.{DotNetNodeInfo, ParserKeys}
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.NewCall
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}

trait AstForPrimitivesCreator(implicit withSchemaValidation: ValidationMode) {
  this: AstCreator =>
  protected def astForIdentifier(ident: DotNetNodeInfo, typeFullName: String = ""): Ast = {
    val identifierName = nameFromNode(ident)
    if identifierName != "_" then {
      val variableOption = scope.lookupVariable(identifierName)
      variableOption match
        case Some((variable, variableTypeName)) => {
          val node = identifierNode(ident, identifierName, ident.code, variableTypeName)
          Ast(node).withRefEdge(node, variable)
        }
        case _ =>
          Ast(identifierNode(ident, identifierName, ident.code, typeFullName))
    } else {
      Ast()
    }
  }
}
