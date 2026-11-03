package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.parser.BabelNodeSyntax.*
import io.joern.x2cpg.{Ast, ValidationMode}

trait AstForTemplateDomCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  /** Creates the TEMPLATE_DOM node for `node`, named after its Babel node type. */
  private def domNodeFor(node: BabelNode, colon: String = "") =
    templateDomNode(node.nodeType, s"$colon${node.code}", node.lineNumber, node.columnNumber.map(_ - colon.length))

  protected def astForJsxElement(jsxElem: JSXElement): Ast = {
    val domNode         = domNodeFor(jsxElem)
    val openingAst      = astForNodeWithFunctionReference(jsxElem.openingElement)
    val childrenAsts    = astForNodes(jsxElem.children)
    val closingAst      = jsxElem.closingElement.map(astForNodeWithFunctionReference).getOrElse(Ast())
    val allChildrenAsts = openingAst +: childrenAsts :+ closingAst
    Ast(domNode).withChildren(allChildrenAsts)
  }

  protected def astForJsxFragment(jsxFragment: JSXFragment): Ast = {
    val domNode      = domNodeFor(jsxFragment)
    val childrenAsts = astForNodes(jsxFragment.children)
    Ast(domNode).withChildren(childrenAsts)
  }

  protected def astForJsxAttribute(jsxAttr: JSXAttribute): Ast = {
    // A colon in front of a JSXAttribute cant be parsed by Babel.
    // Hence, we strip it away with astgen and restore it here.
    // parserResult.fileContent contains the unmodified Vue.js source code for the current file.
    // We look at the previous character there and re-add the colon if needed.
    val colon = jsxAttr.start
      .collect {
        case position if position > 0 && parserResult.fileContent.charAt(position - 1) == ':' => ":"
      }
      .getOrElse("")
    val domNode  = domNodeFor(jsxAttr, colon)
    val valueAst = jsxAttr.value.map(astForNodeWithFunctionReference).getOrElse(Ast())
    Ast(domNode).withChild(valueAst)
  }

  protected def astForJsxOpeningElement(jsxOpeningElem: JSXOpeningElement): Ast = {
    val domNode      = domNodeFor(jsxOpeningElem)
    val childrenAsts = astForNodes(jsxOpeningElem.attributes)
    Ast(domNode).withChildren(childrenAsts)
  }

  protected def astForJsxClosingElement(jsxClosingElem: JSXClosingElement): Ast =
    Ast(domNodeFor(jsxClosingElem))

  protected def astForJsxText(jsxText: JSXText): Ast =
    Ast(domNodeFor(jsxText))

  protected def astForJsxExprContainer(jsxExprContainer: JSXExpressionContainer | JSXSpreadChild): Ast = {
    val domNode = domNodeFor(jsxExprContainer)
    val exprAst = jsxExprContainer match {
      case container: JSXExpressionContainer =>
        container.expression match {
          case _: JSXEmptyExpression => Ast()
          case expression            => astForNodeWithFunctionReference(expression)
        }
      case spreadChild: JSXSpreadChild => astForNodeWithFunctionReference(spreadChild.expression)
    }
    Ast(domNode).withChild(exprAst)
  }

  protected def astForJsxSpreadAttribute(jsxSpreadAttr: JSXSpreadAttribute): Ast = {
    val domNode = domNodeFor(jsxSpreadAttr)
    val argAst  = astForNodeWithFunctionReference(jsxSpreadAttr.argument)
    Ast(domNode).withChild(argAst)
  }

}
