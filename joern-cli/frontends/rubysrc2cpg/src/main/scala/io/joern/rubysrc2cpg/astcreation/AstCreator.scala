package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.astcreation.RubyIntermediateAst.*
import io.joern.rubysrc2cpg.datastructures.{BlockScope, NamespaceScope, RubyProgramSummary, RubyScope}
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.rubysrc2cpg.utils.FreshNameGenerator
import io.joern.x2cpg.utils.NodeBuilders.{newModifierNode, newThisParameterNode}
import io.joern.x2cpg.{Ast, AstCreatorBase, AstNodeBuilder, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{DiffGraphBuilder, ModifierTypes}
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import org.slf4j.{Logger, LoggerFactory}

import java.util.regex.Matcher

class AstCreator(
  val fileName: String,
  protected val projectRoot: Option[String] = None,
  protected val programSummary: RubyProgramSummary = RubyProgramSummary(),
  val enableFileContents: Boolean = false,
  val fileContent: String = "",
  val rootNode: StatementList
)(implicit withSchemaValidation: ValidationMode)
    extends AstCreatorBase(fileName)
    with AstCreatorHelper
    with AstForStatementsCreator
    with AstForExpressionsCreator
    with AstForControlStructuresCreator
    with AstForFunctionsCreator
    with AstForTypesCreator
    with AstSummaryVisitor
    with AstNodeBuilder[RubyExpression, AstCreator] {

  val tmpGen: FreshNameGenerator[String]                      = FreshNameGenerator(i => s"<tmp-$i>")
  val procParamGen: FreshNameGenerator[Left[String, Nothing]] = FreshNameGenerator(i => Left(s"<proc-param-$i>"))

  /* Used to track variable names and their LOCAL nodes.
   */
  protected val scope: RubyScope = new RubyScope(programSummary, projectRoot)

  protected val logger: Logger = LoggerFactory.getLogger(getClass)

  protected var fileNode: Option[NewFile] = None

  protected var parseLevel: AstParseLevel = AstParseLevel.FULL_AST

  override protected def offset(node: RubyExpression): Option[(Int, Int)] = node.offset

  protected val relativeFileName: String =
    projectRoot
      .map(fileName.stripPrefix)
      .map(_.stripPrefix(java.io.File.separator))
      .getOrElse(fileName)

  private def internalLineAndColNum: Option[Int] = Option(1)

  /** The relative file name, in a unix path delimited format.
    */
  private def relativeUnixStyleFileName =
    relativeFileName.replaceAll(Matcher.quoteReplacement(java.io.File.separator), "/")

  override def createAst(): DiffGraphBuilder = {
    val ast = astForRubyFile(rootNode)
    Ast.storeInDiffGraph(ast, diffGraph)
    diffGraph
  }

  /* A Ruby file has the following AST hierarchy: FILE -> NAMESPACE_BLOCK -> METHOD.
   * The (parsed) contents of the file are put under that fictitious METHOD node, thus
   * allowing for a straightforward representation of out-of-method statements.
   */
  protected def astForRubyFile(rootStatements: StatementList): Ast = {
    fileNode =
      if enableFileContents then Option(NewFile().name(relativeFileName).content(fileContent))
      else Option(NewFile().name(relativeFileName))
    val fullName = s"$relativeUnixStyleFileName:${NamespaceTraversal.globalNamespaceName}".stripPrefix("/")

    val namespaceBlock = NewNamespaceBlock()
      .filename(relativeFileName)
      .name(NamespaceTraversal.globalNamespaceName)
      .fullName(fullName)

    scope.pushNewScope(NamespaceScope(fullName))
    val rubyFakeMethodAst = astInFakeMethod(rootStatements)
    scope.popScope()

    Ast(fileNode.get).withChild(Ast(namespaceBlock).withChild(rubyFakeMethodAst))
  }

  private def astInFakeMethod(rootNode: StatementList): Ast = {
    val name = Defines.Main
    // From the <main> method onwards, we do not embed the <global> namespace name in the full names
    val fullName =
      s"${scope.surroundingScopeFullName.head.stripSuffix(NamespaceTraversal.globalNamespaceName)}$name"
    val code = rootNode.text
    val methodNode_ = methodNode(
      node = rootNode,
      name = name,
      code = code,
      fullName = fullName,
      signature = None,
      fileName = relativeFileName
    )
    val thisParameterNode = newThisParameterNode(
      name = Defines.Self,
      code = Defines.Self,
      typeFullName = Defines.Any,
      line = methodNode_.lineNumber,
      column = methodNode_.columnNumber
    )
    val thisParameterAst = Ast(thisParameterNode)
    scope.addToScope(Defines.Self, thisParameterNode)
    val methodReturn = methodReturnNode(rootNode, Defines.Any)

    scope.newProgramScope
      .map { moduleScope =>
        scope.pushNewScope(moduleScope)
        val block = blockNode(rootNode)
        scope.pushNewScope(BlockScope(block))
        val statementAsts = rootNode.statements.flatMap(astsForStatement)
        scope.popScope()
        val bodyAst = blockAst(block, statementAsts)
        scope.popScope()
        methodAst(
          methodNode_,
          thisParameterAst :: Nil,
          bodyAst,
          methodReturn,
          newModifierNode(ModifierTypes.MODULE) :: newModifierNode(ModifierTypes.VIRTUAL) :: Nil
        )
      }
      .getOrElse(Ast())
  }

}

/** Determines till what depth the AST creator will parse until.
  */
enum AstParseLevel {

  /** This level will parse all types and methods signatures, but exclude method bodies.
    */
  case SIGNATURES

  /** This level will parse the full AST.
    */
  case FULL_AST
}
