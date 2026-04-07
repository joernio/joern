package io.joern.abap2cpg.astcreation.statements

import io.joern.abap2cpg.astcreation.AstHelpers
import io.joern.abap2cpg.astcreation.declarations.AstForParametersCreator
import io.joern.abap2cpg.astcreation.expressions.AstForExpressionsCreator
import io.joern.abap2cpg.parser.AbapIntermediateAst.*
import io.joern.abap2cpg.passes.AstCreator
import io.joern.x2cpg.Ast
import io.joern.x2cpg.datastructures.VariableScopeManager
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.*

/** Methods for creating statement ASTs */
trait AstForStatementsCreator {
  this: AstCreator & AstHelpers & AstForParametersCreator & AstForExpressionsCreator =>

  /** Create statements ASTs */
  protected def astForStatements(stmtList: StatementList, className: Option[String]): Seq[Ast] = {
    stmtList.statements.zipWithIndex.flatMap { case (stmt, idx) =>
      val order = idx + 1
      stmt match {
        case callExpr: CallExpr =>
          Some(astForCall(callExpr, order, className))

        case assignStmt: AssignmentStmt =>
          Some(astForAssignment(assignStmt, order, className))

        case opCall: OperatorCall =>
          Some(astForOperatorCall(opCall, order, className))

        case dataDecl: DataDeclaration =>
          val localNode = createLocal(dataDecl, order)
          scope.addVariable(dataDecl.name, localNode, dataDecl.typeName, VariableScopeManager.ScopeType.BlockScope)
          val localAst = Ast(localNode)

          // If there's an initial value, create assignment
          dataDecl.initialValue match {
            case Some(initValue) =>
              val assignment = AssignmentStmt(
                target = IdentifierExpr(dataDecl.name, dataDecl.span),
                value = initValue,
                span = dataDecl.span
              )
              Seq(localAst, astForAssignment(assignment, order + 1, className))
            case None =>
              Seq(localAst)
          }

        case _ => None
      }
    }
  }

  /** Create an assignment call */
  protected def astForAssignment(assignStmt: AssignmentStmt, order: Int, className: Option[String]): Ast = {
    val callNode = NewCall()
      .name(Operators.assignment)
      .code(s"${codeFromExpr(assignStmt.target)} = ${codeFromExpr(assignStmt.value)}")
      .methodFullName(Operators.assignment)
      .typeFullName("ANY")
      .dispatchType("STATIC_DISPATCH")
      .order(order)

    assignStmt.span.start.foreach { pos =>
      callNode.lineNumber(pos.row).columnNumber(pos.col)
    }

    val leftAst  = astForExpression(assignStmt.target, 1, className)
    val rightAst = astForExpression(assignStmt.value, 2, className)

    callAst(callNode, Seq(leftAst, rightAst))
  }
}
