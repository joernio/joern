package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.parser.ParserAst._
import io.joern.gosrc2cpg.parser.{ParserKeys, ParserNodeInfo}
import io.joern.gosrc2cpg.utils.Operator
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}

import scala.util.Try

trait AstForStatementsCreator { this: AstCreator =>
  def astForBlockStatement(blockStmt: ParserNodeInfo) = {

    val newBlockNode = blockNode(blockStmt)
    scope.pushNewScope(newBlockNode)
    val childAsts = Try(blockStmt.json(ParserKeys.List).arr.map(createParserNodeInfo).flatMap { parserNode =>
      parserNode.node match {
        case DeclStmt   => astForDeclStatement(parserNode)
        case AssignStmt => astForAssignStatement(parserNode)
        case IncDecStmt => Seq(astForIncDecStatement(parserNode))
        case _          => Seq(Ast())
      }
    }).toOption.getOrElse(Seq(Ast()))
    scope.popScope()
    blockAst(newBlockNode, childAsts.toList)
  }

  private def astForDeclStatement(declStmt: ParserNodeInfo) = {
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

  private def astForAssignStatement(assignStmt: ParserNodeInfo) = {
    val op = assignStmt.json(ParserKeys.Tok).value match {
      case "="   => Operators.assignment
      case ":="  => Operators.assignment
      case "*="  => Operators.assignmentMultiplication
      case "/="  => Operators.assignmentDivision
      case "%="  => Operators.assignmentModulo
      case "+="  => Operators.assignmentPlus
      case "-="  => Operators.assignmentMinus
      case "<<=" => Operators.assignmentShiftLeft
      case ">>=" => Operators.assignmentArithmeticShiftRight
      case "&="  => Operators.assignmentAnd
      case "^="  => Operators.assignmentXor
      case "|="  => Operators.assignmentOr
      case _     => Operator.unknown
    }
    val cNode = callNode(assignStmt, assignStmt.code, op, op, DispatchTypes.STATIC_DISPATCH)

    // create corresponding local node as this is known as short variable declaration operator
    val localNodesIfIntialized =
      if (assignStmt.json(ParserKeys.Tok).value == ":=") createLocalNodeForShortVariableDeclaration(assignStmt)
      else Ast()
    val arguments = assignStmt.json(ParserKeys.Lhs).arr.map(astForNode).toList ::: assignStmt
      .json(ParserKeys.Rhs)
      .arr
      .map(astForNode)
      .toList
    val callAst_ = callAst(cNode, arguments)
    Seq(localNodesIfIntialized, callAst_)
  }

  private def astForIncDecStatement(incDecStatement: ParserNodeInfo) = {
    val op = incDecStatement.json(ParserKeys.Tok).value match {
      case "++" => Operators.postIncrement
      case "--" => Operators.postDecrement
      case _    => Operator.unknown
    }
    val cNode   = callNode(incDecStatement, incDecStatement.code, op, op, DispatchTypes.STATIC_DISPATCH)
    val operand = astForNode(incDecStatement.json(ParserKeys.X))
    callAst(cNode, Seq(operand))
  }

  def createLocalNodeForShortVariableDeclaration(assignStmt: ParserNodeInfo): Ast = {

    val localNodes = (assignStmt.json(ParserKeys.Lhs).arr zip assignStmt.json(ParserKeys.Rhs).arr)
      .map { case (lhs, rhs) => (createParserNodeInfo(lhs), createParserNodeInfo(rhs)) }
      .map { case (localParserNode, rhsParserNode) =>
        val name = localParserNode.json(ParserKeys.Name).str
        val typ  = getTypeOfToken(rhsParserNode)
        val node = localNode(localParserNode, name, localParserNode.code, typ)
        scope.addToScope(name, (node, typ))
        node
      }
    Ast(localNodes)
  }

  private def astForValueSpec(valueSpec: ParserNodeInfo) = {

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
          val arguments = Seq(astForNode(lhsParserNode.json), astForNode(rhsParserNode.json))
          callAst(cNode, arguments)
        }
      localNodes.toList ::: callNodes
    } else
      localNodes.toList
  }

}
