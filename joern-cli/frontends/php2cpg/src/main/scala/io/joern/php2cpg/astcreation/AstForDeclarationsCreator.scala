package io.joern.php2cpg.astcreation

import io.joern.php2cpg.parser.Domain.{PhpDeclareItem, PhpDeclareStmt, PhpOperators}
import io.joern.x2cpg.{Ast, Defines, ValidationMode}
import io.joern.x2cpg.utils.AstPropertiesUtil.RootProperties
import io.shiftleft.codepropertygraph.generated.Operators

trait AstForDeclarationsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def astForDeclareStmt(stmt: PhpDeclareStmt): Ast = {
    val declareAssignAsts = stmt.declares.map(astForDeclareItem)
    val declareCode       = s"${PhpOperators.declareFunc}(${declareAssignAsts.map(_.rootCodeOrEmpty).mkString(",")})"
    val declareNode       = operatorCallNode(stmt, declareCode, PhpOperators.declareFunc, None)
    val declareAst        = callAst(declareNode, declareAssignAsts)

    stmt.stmts match {
      case Some(stmtList) =>
        val stmtAsts = stmtList.flatMap(astsForStmt)
        Ast(blockNode(stmt))
          .withChild(declareAst)
          .withChildren(stmtAsts)

      case None => declareAst
    }
  }

  private def astForDeclareItem(item: PhpDeclareItem): Ast = {
    val key   = identifierNode(item, item.key.name, item.key.name, Defines.Any)
    val value = astForExpr(item.value)
    val code  = s"${key.name}=${value.rootCodeOrEmpty}"

    val declareAssignment = operatorCallNode(item, code, Operators.assignment, None)
    callAst(declareAssignment, Ast(key) :: value :: Nil)
  }

}
