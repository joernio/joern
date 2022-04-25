package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.datastructures.scope.BlockScope
import io.joern.jssrc2cpg.datastructures.scope.MethodScope
import io.joern.jssrc2cpg.datastructures.scope.ScopeType
import io.joern.jssrc2cpg.parser.BabelAst
import io.joern.jssrc2cpg.parser.BabelNodeInfo
import io.joern.jssrc2cpg.passes.Defines
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import ujson.Value

import scala.util.Try

trait AstForDeclarationsCreator {

  this: AstCreator =>

  protected def astForVariableDeclaration(declaration: BabelNodeInfo): Ast = {
    val scopeType = if (declaration.json("kind").str == "let") {
      BlockScope
    } else {
      MethodScope
    }
    declaration.json("declarations").arr.foldLeft(Ast()) { (ast, d) =>
      ast.merge(astForVariableDeclarator(d, scopeType))
    }
  }

  private def astForVariableDeclarator(declarator: Value, scopeType: ScopeType): Ast = {
    val id   = createBabelNodeInfo(declarator("id"))
    val init = Try(createBabelNodeInfo(declarator("init"))).toOption

    val typeFullName = init match {
      case Some(f @ BabelNodeInfo(BabelAst.FunctionDeclaration)) =>
        val (_, methodFullName) = calcMethodNameAndFullName(f)
        methodFullName
      case _ => Defines.ANY.label
    }

    val localNode = createLocalNode(id.code, typeFullName)
    diffGraph.addEdge(localAstParentStack.head, localNode, EdgeTypes.AST)
    scope.addVariable(id.code, localNode, scopeType)

    init match {
      case Some(f @ BabelNodeInfo(BabelAst.FunctionDeclaration)) =>
        val destAst   = astForNode(id.json)
        val sourceAst = astForFunctionDeclaration(f, shouldCreateFunctionReference = true)
        val assigmentCallAst =
          createAssignment(
            destAst.nodes.head,
            sourceAst.nodes.head,
            code(declarator),
            line = line(declarator),
            column = column(declarator)
          )
        Ast.storeInDiffGraph(destAst, diffGraph)
        Ast.storeInDiffGraph(sourceAst, diffGraph)
        assigmentCallAst
      case Some(initExpr) =>
        val destAst   = astForNode(id.json)
        val sourceAst = astForNode(initExpr.json)
        val assigmentCallAst =
          createAssignment(
            destAst.nodes.head,
            sourceAst.nodes.head,
            code(declarator),
            line = line(declarator),
            column = column(declarator)
          )
        Ast.storeInDiffGraph(destAst, diffGraph)
        Ast.storeInDiffGraph(sourceAst, diffGraph)
        assigmentCallAst
      case None => Ast()
    }
  }

}
