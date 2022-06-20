package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.parser.{BabelAst, BabelNodeInfo}
import io.joern.x2cpg.Ast
import ujson.Obj

trait AstForTemplateDomCreator {

  this: AstCreator =>

  protected def astForJsxElement(jsxElem: BabelNodeInfo): Ast = {
    val domNode = createTemplateDomNode(jsxElem.node.toString, jsxElem.code, jsxElem.lineNumber, jsxElem.columnNumber)

    val openingAst   = astForNode(jsxElem.json("openingElement"))
    val childrenAsts = astForNodes(jsxElem.json("children").arr.toList)
    val closingAst   = safeObj(jsxElem.json, "closingElement").map(e => astForNode(Obj(e))).getOrElse(Ast())

    val allChildrenAsts = openingAst +: childrenAsts :+ closingAst
    setIndices(allChildrenAsts)

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
    setIndices(childrenAsts)
    Ast(domNode).withChildren(childrenAsts)
  }

  protected def astForJsxAttribute(jsxAttr: BabelNodeInfo): Ast = {
    val domNode  = createTemplateDomNode(jsxAttr.node.toString, jsxAttr.code, jsxAttr.lineNumber, jsxAttr.columnNumber)
    val valueAst = safeObj(jsxAttr.json, "value").map(e => astForNode(Obj(e))).getOrElse(Ast())
    setIndices(List(valueAst))
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
    setIndices(childrenAsts)
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

  protected def astForJsxText(jsxText: BabelNodeInfo): Ast = {
    val domNode = createTemplateDomNode(jsxText.node.toString, jsxText.code, jsxText.lineNumber, jsxText.columnNumber)
    Ast(domNode)
  }

  protected def astForJsxExprContainer(jsxExprContainer: BabelNodeInfo): Ast = {
    val domNode = createTemplateDomNode(
      jsxExprContainer.node.toString,
      jsxExprContainer.code,
      jsxExprContainer.lineNumber,
      jsxExprContainer.columnNumber
    )
    val nodeInfo = createBabelNodeInfo(jsxExprContainer.json("expression"))
    val exprAst = nodeInfo.node match {
      case BabelAst.JSXEmptyExpression => Ast()
      case _                           => astForNode(nodeInfo.json)
    }
    setIndices(List(exprAst))
    Ast(domNode).withChild(exprAst)
  }

  protected def astForJsxSpreadAttribute(jsxSpreadAttr: BabelNodeInfo): Ast = {
    val domNode = createTemplateDomNode(
      jsxSpreadAttr.node.toString,
      jsxSpreadAttr.code,
      jsxSpreadAttr.lineNumber,
      jsxSpreadAttr.columnNumber
    )
    val argAst = astForNode(jsxSpreadAttr.json("argument"))
    setIndices(List(argAst))
    Ast(domNode).withChild(argAst)
  }

}
