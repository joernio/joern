package io.joern.csharpsrc2cpg.astcreation

import io.joern.csharpsrc2cpg.CSharpOperators
import io.joern.csharpsrc2cpg.parser.{DotNetNodeInfo, ParserKeys}
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.codepropertygraph.generated.nodes.{NewBlock, NewControlStructure}

import scala.util.{Failure, Success, Try}

trait AstForControlStructuresCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def astForThrowStatement(throwStmt: DotNetNodeInfo): Seq[Ast] = {
    val expr = createDotNetNodeInfo(throwStmt.json(ParserKeys.Expression))
    val args = astForNode(expr)
    val throwCall = createCallNodeForOperator(
      throwStmt,
      CSharpOperators.throws,
      typeFullName = Option(getTypeFullNameFromAstNode(args))
    )
    Seq(callAst(throwCall, args))
  }

  protected def astForTryStatement(tryStmt: DotNetNodeInfo): Seq[Ast] = {
    val tryNode = NewControlStructure()
      .controlStructureType(ControlStructureTypes.TRY)
      .code("try")
      .lineNumber(line(tryStmt))
      .columnNumber(column(tryStmt))
    val tryAst = astForBlock(createDotNetNodeInfo(tryStmt.json(ParserKeys.Block)), Option("try"))

    val catchAsts = Try(tryStmt.json(ParserKeys.Catches)).map(_.arr.flatMap(astForNode).toSeq).getOrElse(Seq.empty)
    val catchBlock = Option
      .when(catchAsts.nonEmpty) {
        Ast(blockNode(tryStmt).code("catch")).withChildren(catchAsts)
      }
      .toList

    val finallyBlock = Try(createDotNetNodeInfo(tryStmt.json(ParserKeys.Finally))).map(astForFinallyClause) match
      case Success(finallyAst :: Nil) =>
        finallyAst.root.collect { case x: NewBlock => x.code("finally") }
        finallyAst
      case _ => Ast()

    val controlStructureAst = Ast(tryNode)
      .withChild(tryAst)
      .withChildren(catchBlock)
      .withChild(finallyBlock)

    Seq(controlStructureAst)
  }

  protected def astForFinallyClause(finallyClause: DotNetNodeInfo): Seq[Ast] = {
    Seq(astForBlock(createDotNetNodeInfo(finallyClause.json(ParserKeys.Block)), code = Option(code(finallyClause))))
  }

  protected def astForCatchClause(catchClause: DotNetNodeInfo): Seq[Ast] = {
    val declAst = astForNode(catchClause.json(ParserKeys.Declaration)).toList
    val blockAst = astForBlock(
      createDotNetNodeInfo(catchClause.json(ParserKeys.Block)),
      code = Option(code(catchClause)),
      prefixAsts = declAst
    )
    Seq(blockAst)
  }

  protected def astForCatchDeclaration(catchDeclaration: DotNetNodeInfo): Seq[Ast] = {
    val name         = nameFromNode(catchDeclaration)
    val typeFullName = nodeTypeFullName(catchDeclaration)
    val _localNode   = localNode(catchDeclaration, name, name, typeFullName)
    val localNodeAst = Ast(_localNode)
    scope.addToScope(name, _localNode)
    Seq(localNodeAst)
  }

}
