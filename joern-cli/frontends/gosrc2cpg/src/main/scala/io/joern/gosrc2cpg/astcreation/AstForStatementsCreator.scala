package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.parser.ParserAst.{DeclStmt, GenDecl, ValueSpec}
import io.joern.gosrc2cpg.parser.{ParserKeys, ParserNodeInfo}
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}

import scala.util.Try

trait AstForStatementsCreator { this: AstCreator =>
  def astForBlockStatement(blockStmt: ParserNodeInfo) = {

    val newBlockNode = blockNode(blockStmt)
    scope.pushNewScope(newBlockNode)
    val childAsts = Try(blockStmt.json(ParserKeys.List).arr.map(createParserNodeInfo).flatMap { parserNode =>
      parserNode.node match {
        case DeclStmt => astForDeclStatement(parserNode)
        case _        => Seq(Ast())
      }
    }).toOption.getOrElse(Seq(Ast()))
    scope.popScope()
    blockAst(newBlockNode, childAsts.toList)
  }

  def astForDeclStatement(declStmt: ParserNodeInfo) = {
    val nodeInfo = createParserNodeInfo(declStmt.json(ParserKeys.Decl))
    nodeInfo.node match {
      case GenDecl =>
        nodeInfo.json(ParserKeys.Specs).arr.map(createParserNodeInfo).flatMap { parserNode =>
          parserNode.node match {
            case ValueSpec => astForValueSpec(parserNode)
            case _         => Seq(Ast())
          }
        }
    }
  }

  def astForValueSpec(valueSpec: ParserNodeInfo) = {

    val localNodes = valueSpec.json(ParserKeys.Names).arr.map { parserNode =>
      val localParserNode = createParserNodeInfo(parserNode)

      val name = parserNode(ParserKeys.Name).str
      val typ  = valueSpec.json(ParserKeys.Type).obj(ParserKeys.Name).str
      val node = localNode(localParserNode, name, localParserNode.code, typ)
      scope.addToScope(name, (node, typ))
      Ast(node)
    }

    if (!valueSpec.json(ParserKeys.Values).isNull) {
      val callNodes = (valueSpec.json(ParserKeys.Names).arr.toList zip valueSpec.json(ParserKeys.Values).arr.toList)
        .map { case (lhs, rhs) => (createParserNodeInfo(lhs), createParserNodeInfo(rhs)) }
        .map { case (lhsParserNode, rhsParserNode) =>
          val cNode = callNode(
            rhsParserNode,
            lhsParserNode.code + rhsParserNode.code,
            Operators.assignment,
            Operators.assignment,
            DispatchTypes.STATIC_DISPATCH
          )
          val arguments = Seq(astForIdentifier(lhsParserNode), astForNode(rhsParserNode.json))
          callAst(cNode, arguments)
        }
      localNodes.toList ::: callNodes
    } else
      localNodes.toList
  }

}
