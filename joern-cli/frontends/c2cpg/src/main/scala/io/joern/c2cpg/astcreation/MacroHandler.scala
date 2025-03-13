package io.joern.c2cpg.astcreation

import io.joern.x2cpg.Ast
import io.joern.x2cpg.ValidationMode
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.nodes.AstNodeNew
import io.shiftleft.codepropertygraph.generated.nodes.ExpressionNew
import io.shiftleft.codepropertygraph.generated.nodes.NewBlock
import io.shiftleft.codepropertygraph.generated.nodes.NewCall
import io.shiftleft.codepropertygraph.generated.nodes.NewFieldIdentifier
import io.shiftleft.codepropertygraph.generated.nodes.NewLocal
import io.shiftleft.codepropertygraph.generated.nodes.NewNode
import org.apache.commons.lang3.StringUtils
import org.eclipse.cdt.core.dom.ast.IASTBinaryExpression
import org.eclipse.cdt.core.dom.ast.IASTMacroExpansionLocation
import org.eclipse.cdt.core.dom.ast.IASTNode
import org.eclipse.cdt.core.dom.ast.IASTPreprocessorMacroDefinition
import org.eclipse.cdt.internal.core.model.ASTStringUtil

import scala.annotation.nowarn
import scala.collection.mutable

trait MacroHandler { this: AstCreator =>

  private val nodeOffsetMacroPairs: mutable.Stack[(Int, IASTPreprocessorMacroDefinition)] = {
    mutable.Stack.from(
      cdtAst.getNodeLocations.toList
        .collect { case exp: IASTMacroExpansionLocation =>
          (exp.asFileLocation().getNodeOffset, exp.getExpansion.getMacroDefinition)
        }
        .sortBy(_._1)
    )
  }

  /** For the given node, determine if it is expanded from a macro, and if so, create a Call node to represent the macro
    * invocation and attach `ast` as its child.
    */
  def asChildOfMacroCall(node: IASTNode, ast: Ast): Ast = {
    // If a macro in a header file contained a method definition already seen in some
    // source file we skipped that during the previous AST creation and returned an empty AST.
    if (ast.root.isEmpty && isExpandedFromMacro(node)) return ast
    // We do nothing for locals only.
    if (ast.nodes.size == 1 && ast.root.exists(_.isInstanceOf[NewLocal])) return ast
    // Otherwise, we create the synthetic call AST.
    val matchingMacro = extractMatchingMacro(node)
    val macroCallAst  = matchingMacro.map { case (mac, args) => createMacroCallAst(ast, node, mac, args) }
    macroCallAst match {
      case Some(callAst) =>
        // We need to wrap the AST as it may contain CPG nodes not being allowed
        // to be connected via AST edges under a CALL. E.g., LOCALs but only if it is not already a BLOCK.
        val childAst = ast.root match {
          case Some(_: NewBlock) => ast
          case _ =>
            setArgumentIndices(List(ast))
            blockAst(blockNode(node), List(ast))
        }
        setArgumentIndices(List(childAst))
        callAst.withChild(childAst)
      case None => ast
    }
  }

  /** For the given node, determine if it is expanded from a macro, and if so, find the first matching (offset, macro)
    * pair in nodeOffsetMacroPairs, removing non-matching elements from the start of nodeOffsetMacroPairs. Returns
    * (Some(macroDefinition, arguments)) if a macro definition matches and None otherwise.
    */
  private def extractMatchingMacro(node: IASTNode): Option[(IASTPreprocessorMacroDefinition, List[String])] = {
    val expansionLocations = expandedFromMacro(node).filterNot(isExpandedFrom(node.getParent, _))
    val nodeOffset         = node.getFileLocation.getNodeOffset
    var matchingMacro      = Option.empty[(IASTPreprocessorMacroDefinition, List[String])]

    expansionLocations.foreach { macroLocation =>
      while (matchingMacro.isEmpty && nodeOffsetMacroPairs.headOption.exists(_._1 <= nodeOffset)) {
        val (_, macroDefinition) = nodeOffsetMacroPairs.pop()
        val macroExpansionName   = ASTStringUtil.getSimpleName(macroLocation.getExpansion.getMacroDefinition.getName)
        val macroDefinitionName  = ASTStringUtil.getSimpleName(macroDefinition.getName)
        if (macroExpansionName == macroDefinitionName) {
          val arguments = new MacroArgumentExtractor(cdtAst, node.getFileLocation).getArguments
          matchingMacro = Option((macroDefinition, arguments))
        }
      }
    }

    matchingMacro
  }

  /** Determine whether `node` is expanded from the macro expansion at `loc`.
    */
  private def isExpandedFrom(node: IASTNode, loc: IASTMacroExpansionLocation): Boolean = {
    expandedFromMacro(node).map(_.getExpansion.getMacroDefinition).contains(loc.getExpansion.getMacroDefinition)
  }

  /** Create an AST that represents a macro expansion as a call. The AST is rooted in a CALL node and contains subtrees
    * for arguments. These are also connected to the AST via ARGUMENT edges. We include line number information in the
    * CALL node that is picked up by the MethodStubCreator.
    */
  private def createMacroCallAst(
    ast: Ast,
    node: IASTNode,
    macroDef: IASTPreprocessorMacroDefinition,
    arguments: List[String]
  ): Ast = {
    val name    = ASTStringUtil.getSimpleName(macroDef.getName)
    val code    = node.getRawSignature.stripSuffix(";")
    val argAsts = argumentTrees(arguments, ast).map(_.getOrElse(Ast()))

    val callName     = StringUtils.normalizeSpace(name)
    val callFullName = StringUtils.normalizeSpace(fullName(macroDef, argAsts))
    val typeFullName = registerType(cleanType(typeFor(node)))
    val callNode =
      NewCall()
        .name(callName)
        .dispatchType(DispatchTypes.INLINED)
        .methodFullName(callFullName)
        .code(code)
        .typeFullName(typeFullName)
        .lineNumber(line(node))
        .columnNumber(column(node))
    callAst(callNode, argAsts)
  }

  private def argumentTrees(arguments: List[String], ast: Ast): List[Option[Ast]] = {
    arguments.zipWithIndex.map { case (arg, i) =>
      val rootNode = argForCode(arg, ast)
      rootNode.map(x => ast.subTreeCopy(x.asInstanceOf[AstNodeNew], i + 1))
    }
  }

  private def argForCode(code: String, ast: Ast): Option[NewNode] = {
    val normalizedCode = code.replace(" ", "")
    if (normalizedCode == "") {
      None
    } else {
      ast.nodes.collectFirst {
        case x: ExpressionNew if !x.isInstanceOf[NewFieldIdentifier] && x.code == normalizedCode => x
      }
    }
  }

  /** Create a full name field that encodes line information that can be picked up by the MethodStubCreator in order to
    * create a METHOD node with the correct location information.
    */
  private def fullName(macroDef: IASTPreprocessorMacroDefinition, argAsts: List[Ast]) = {
    val name               = ASTStringUtil.getSimpleName(macroDef.getName)
    val filename           = fileName(macroDef)
    val lineNo: Integer    = line(macroDef).getOrElse(-1)
    val lineNoEnd: Integer = lineEnd(macroDef).getOrElse(-1)
    s"$filename:$lineNo:$lineNoEnd:$name:${argAsts.size}"
  }

  private def isExpandedFromMacro(node: IASTNode): Boolean = expandedFromMacro(node).nonEmpty

  private def expandedFromMacro(node: IASTNode): Option[IASTMacroExpansionLocation] = {
    val locations = node.getNodeLocations.toList
    val locationsSorted = node match {
      // For binary expressions the expansion locations may occur in any order.
      // We manually sort them here to ignore this.
      // TODO: This may also happen with other expressions that allow for multiple sub elements.
      case _: IASTBinaryExpression => locations.sortBy(_.isInstanceOf[IASTMacroExpansionLocation])
      case _                       => locations
    }
    locationsSorted match {
      case (head: IASTMacroExpansionLocation) :: _ => Option(head)
      case _                                       => None
    }
  }

  /** The CDT utility method is unfortunately in a class that is marked as deprecated, however, this is because the CDT
    * team would like to discourage its use but at the same time does not plan to remove this code.
    */
  @nowarn
  def nodeSignature(node: IASTNode): String = {
    import org.eclipse.cdt.core.dom.ast.ASTSignatureUtil.getNodeSignature
    if (isExpandedFromMacro(node)) {
      val sig = getNodeSignature(node)
      if (sig.isEmpty) {
        node.getRawSignature
      } else {
        sig
      }
    } else {
      node.getRawSignature
    }
  }

}
