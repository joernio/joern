package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.parser.BabelAst.*
import io.joern.jssrc2cpg.parser.BabelNodeInfo
import io.joern.x2cpg.{Ast, ValidationMode}
import ujson.Obj

trait AstForTemplateDomCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def astForJsxElement(jsxElem: BabelNodeInfo): Ast = {
    val domNode = createTemplateDomNode(jsxElem.node.toString, jsxElem.code, jsxElem.lineNumber, jsxElem.columnNumber)
    val openingAst   = astForNodeWithFunctionReference(jsxElem.json("openingElement"))
    val childrenAsts = astForNodes(jsxElem.json("children").arr.toList)
    val closingAst =
      safeObj(jsxElem.json, "closingElement")
        .map(e => astForNodeWithFunctionReference(Obj(e)))
        .getOrElse(Ast())
    val allChildrenAsts = openingAst +: childrenAsts :+ closingAst
    setArgumentIndices(allChildrenAsts)
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
    setArgumentIndices(childrenAsts)
    Ast(domNode).withChildren(childrenAsts)
  }

  protected def astForJsxAttribute(jsxAttr: BabelNodeInfo): Ast = {
    // A colon in front of a JSXAttribute cant be parsed by Babel.
    // Hence, we strip it away with astgen and restore it here.
    // parserResult.fileContent contains the unmodified Vue.js source code for the current file.
    // We look at the previous character there and re-add the colon if needed.
    val colon = pos(jsxAttr.json)
      .collect {
        case position if position > 0 && parserResult.fileContent.substring(position - 1, position) == ":" => ":"
      }
      .getOrElse("")
    val domNode =
      createTemplateDomNode(
        jsxAttr.node.toString,
        s"$colon${jsxAttr.code}",
        jsxAttr.lineNumber,
        jsxAttr.columnNumber.map(_ - colon.length)
      )
    val valueAst = safeObj(jsxAttr.json, "value")
      .map(e => astForNodeWithFunctionReference(Obj(e)))
      .getOrElse(Ast())
    setArgumentIndices(List(valueAst))
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
    setArgumentIndices(childrenAsts)
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
    setArgumentIndices(List(exprAst))
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
    setArgumentIndices(List(argAst))
    Ast(domNode).withChild(argAst)
  }

}
