package io.joern.c2cpg.astcreation

import io.joern.c2cpg.passes.FunctionDeclNodePass
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EvaluationStrategies}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import org.eclipse.cdt.core.dom.ast.{
  IASTBinaryExpression,
  IASTMacroExpansionLocation,
  IASTNode,
  IASTPreprocessorMacroDefinition
}
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
            setOrder(List(ast))
            blockAst(blockNode(node), List(ast))
        }
        setArgumentIndices(List(childAst), callAst.argEdges.size + 1)
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
    val macroDefName     = shortName(macroDef)
    val code             = node.getRawSignature.stripSuffix(";")
    val typeFullName     = registerType(typeFor(node))
    val argAsts          = argumentTrees(arguments, ast).map(_.getOrElse(Ast()))
    val signature        = s"$typeFullName(${argAsts.size})"
    val fileName_        = fileName(macroDef)
    val macroDefFullName = s"$fileName_:$macroDefName:$signature"

    val callNode_ =
      callNode(node, code, macroDefName, macroDefFullName, DispatchTypes.INLINED, Some(signature), Some(typeFullName))

    // we stub the corresponding method manually because we want different line/column information
    // (call information from the actual AST node but the method gets the IASTPreprocessorMacroDefinition information)
    createMacroMethodStub(macroDefName, macroDefFullName, signature, typeFullName, fileName_, argAsts.size, macroDef)

    callAst(callNode_, argAsts)
  }

  private def createMacroMethodStub(
    name: String,
    fullName: String,
    signature: String,
    typeFullName: String,
    fileName: String,
    parameterCount: Int,
    macroDef: IASTPreprocessorMacroDefinition
  ): Unit = {
    val parentNode: NewTypeDecl = methodAstParentStack.collectFirst {
      // just like actual macros we put the method stub for it top-level
      case t: NewTypeDecl if t.name == NamespaceTraversal.globalNamespaceName => t
    }.get
    val astParentFullName = parentNode.fullName
    val astParentType     = parentNode.label
    val code_             = code(macroDef)

    val lineNumber      = line(macroDef)
    val columnNumber    = column(macroDef)
    val lineNumberEnd   = lineEnd(macroDef)
    val columnNumberEnd = columnEnd(macroDef)
    val offset_         = offset(macroDef)

    val parameter = (1 to parameterCount).map { index =>
      val nameAndCode = s"p$index"
      new FunctionDeclNodePass.ParameterInfo(
        name = nameAndCode,
        code = nameAndCode,
        index = index,
        isVariadic = false,
        evaluationStrategy = EvaluationStrategies.BY_VALUE,
        lineNumber = lineNumber,
        columnNumber = columnNumber,
        typeFullName = registerType(Defines.Any)
      )
    }

    val methodInfo = FunctionDeclNodePass.MethodInfo(
      name = name,
      code = code_,
      fileName = filename,
      returnType = typeFullName,
      astParentType = astParentType,
      astParentFullName = astParentFullName,
      lineNumber = lineNumber,
      columnNumber = columnNumber,
      lineNumberEnd = lineNumberEnd,
      columnNumberEnd = columnNumberEnd,
      signature = signature,
      offset = offset_,
      parameter = parameter,
      modifier = Nil,
      isExternal = true
    )
    registerMethodDeclaration(fullName, methodInfo)
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
