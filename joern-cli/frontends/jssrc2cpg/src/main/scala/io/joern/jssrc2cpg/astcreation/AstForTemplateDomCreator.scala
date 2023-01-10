package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.parser.BabelAst._
import io.joern.jssrc2cpg.parser.BabelNodeInfo
import io.joern.x2cpg.Ast
import ujson.Obj

trait AstForTemplateDomCreator { this: AstCreator =>

  protected def astForJsxElement(jsxElem: BabelNodeInfo): Ast = {
    val domNode = createTemplateDomNode(jsxElem.node.toString, jsxElem.code, jsxElem.lineNumber, jsxElem.columnNumber)

    val openingAst   = astForNodeWithFunctionReference(jsxElem.json("openingElement"))
    val childrenAsts = astForNodes(jsxElem.json("children").arr.toList)
    val closingAst =
      safeObj(jsxElem.json, "closingElement").map(e => astForNodeWithFunctionReference(Obj(e))).getOrElse(Ast())

    val allChildrenAsts = openingAst +: childrenAsts :+ closingAst
    setArgIndices(allChildrenAsts)

    Ast(domNode).withChildren(allChildrenAsts)
  }

  protected def astForJsxFragment(jsxFragment: BabelNodeInfo): Ast = {
    val domNode = createTemplateDomNode(
      jsxFragment.node.toString,
      jsxFragment.code,
      jsxFragment.lineNumber,
      jsxFragment.columnNumber
    )
    val childrenAsts = astForNodes(jsxFragment.json("children").arr.toList)
    setArgIndices(childrenAsts)
    Ast(domNode).withChildren(childrenAsts)
  }

  protected def astForJsxAttribute(jsxAttr: BabelNodeInfo): Ast = {
    val domNode  = createTemplateDomNode(jsxAttr.node.toString, jsxAttr.code, jsxAttr.lineNumber, jsxAttr.columnNumber)
    val valueAst = safeObj(jsxAttr.json, "value").map(e => astForNodeWithFunctionReference(Obj(e))).getOrElse(Ast())
    setArgIndices(List(valueAst))
    Ast(domNode).withChild(valueAst)
  }

  protected def astForJsxOpeningElement(jsxOpeningElem: BabelNodeInfo): Ast = {
    val domNode = createTemplateDomNode(
      jsxOpeningElem.node.toString,
      jsxOpeningElem.code,
      jsxOpeningElem.lineNumber,
      jsxOpeningElem.columnNumber
    )
    val childrenAsts = astForNodes(jsxOpeningElem.json("attributes").arr.toList)
    setArgIndices(childrenAsts)
    Ast(domNode).withChildren(childrenAsts)
  }

  protected def astForJsxClosingElement(jsxClosingElem: BabelNodeInfo): Ast = {
    val domNode = createTemplateDomNode(
      jsxClosingElem.node.toString,
      jsxClosingElem.code,
      jsxClosingElem.lineNumber,
      jsxClosingElem.columnNumber
    )
    Ast(domNode)
  }

  protected def astForJsxText(jsxText: BabelNodeInfo): Ast =
    Ast(createTemplateDomNode(jsxText.node.toString, jsxText.code, jsxText.lineNumber, jsxText.columnNumber))

  protected def astForJsxExprContainer(jsxExprContainer: BabelNodeInfo): Ast = {
    val domNode = createTemplateDomNode(
      jsxExprContainer.node.toString,
      jsxExprContainer.code,
      jsxExprContainer.lineNumber,
      jsxExprContainer.columnNumber
    )
    val nodeInfo = createBabelNodeInfo(jsxExprContainer.json("expression"))
    val exprAst = nodeInfo.node match {
      case JSXEmptyExpression => Ast()
      case _                  => astForNodeWithFunctionReference(nodeInfo.json)
    }
    setArgIndices(List(exprAst))
    Ast(domNode).withChild(exprAst)
  }

  protected def astForJsxSpreadAttribute(jsxSpreadAttr: BabelNodeInfo): Ast = {
    val domNode = createTemplateDomNode(
      jsxSpreadAttr.node.toString,
      jsxSpreadAttr.code,
      jsxSpreadAttr.lineNumber,
      jsxSpreadAttr.columnNumber
    )
    val argAst = astForNodeWithFunctionReference(jsxSpreadAttr.json("argument"))
    setArgIndices(List(argAst))
    Ast(domNode).withChild(argAst)
  }

}
