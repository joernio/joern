package io.joern.jssrc2cpg.astcreation

import io.joern.x2cpg.datastructures.Stack._
import io.joern.jssrc2cpg.parser.BabelAst
import io.joern.jssrc2cpg.parser.BabelNodeInfo
import io.joern.x2cpg.Ast
import ujson.Obj
import ujson.Value

trait AstForStatementsCreator {

  this: AstCreator =>

  protected def createBlockStatementAsts(json: Value): List[Ast] = {
    val blockStmts = json.arr.map(createBabelNodeInfo).sortBy(_.node != BabelAst.FunctionDeclaration).toList
    blockStmts.map {
      case func @ BabelNodeInfo(BabelAst.FunctionDeclaration) =>
        astForFunctionDeclaration(func, shouldCreateAssignmentCall = true, shouldCreateFunctionReference = true)
      case other =>
        astForNode(other.json)
    }
  }

  protected def astForBlockStatement(block: BabelNodeInfo): Ast = {
    val blockNode = createBlockNode(block.code, block.lineNumber, block.columnNumber)
    scope.pushNewBlockScope(blockNode)
    localAstParentStack.push(blockNode)
    val blockStatementAsts = createBlockStatementAsts(block.json("body"))
    localAstParentStack.pop()
    scope.popScope()
    Ast(blockNode).withChildren(blockStatementAsts)
  }

  protected def astForReturnStatement(ret: BabelNodeInfo): Ast = {
    val retNode = createReturnNode(ret)
    safeObj(ret.json, "argument")
      .map { argument =>
        val argAst = astForNode(Obj(argument))
        returnAst(retNode, List(argAst))
      }
      .getOrElse(Ast(retNode))
  }

}
