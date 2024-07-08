package io.joern.swiftsrc2cpg.astcreation

import io.joern.swiftsrc2cpg.Config
import io.joern.swiftsrc2cpg.datastructures.Scope
import io.joern.swiftsrc2cpg.parser.SwiftJsonParser.ParseResult
import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.*
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.utils.NodeBuilders.newMethodReturnNode
import io.joern.x2cpg.{Ast, AstCreatorBase, ValidationMode, AstNodeBuilder as X2CpgAstNodeBuilder}
import io.joern.x2cpg.datastructures.Global
import io.joern.x2cpg.frontendspecific.swiftsrc2cpg.Defines
import io.joern.x2cpg.utils.NodeBuilders.newModifierNode
import io.joern.x2cpg.utils.OffsetUtils
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import io.shiftleft.codepropertygraph.generated.NodeTypes
import io.shiftleft.codepropertygraph.generated.nodes.NewBlock
import io.shiftleft.codepropertygraph.generated.nodes.NewFile
import io.shiftleft.codepropertygraph.generated.nodes.NewNode
import io.shiftleft.codepropertygraph.generated.nodes.NewTypeDecl
import io.shiftleft.codepropertygraph.generated.nodes.NewTypeRef
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.codepropertygraph.generated.nodes.File.PropertyDefaults
import org.slf4j.{Logger, LoggerFactory}
import io.shiftleft.codepropertygraph.generated.DiffGraphBuilder

import scala.collection.mutable

class AstCreator(val config: Config, val global: Global, val parserResult: ParseResult)(implicit
  withSchemaValidation: ValidationMode
) extends AstCreatorBase(parserResult.filename)
    with AstForSwiftTokenCreator
    with AstForSyntaxCreator
    with AstForExprSyntaxCreator
    with AstForTypeSyntaxCreator
    with AstForDeclSyntaxCreator
    with AstForPatternSyntaxCreator
    with AstForStmtSyntaxCreator
    with AstForSyntaxCollectionCreator
    with AstCreatorHelper
    with AstNodeBuilder
    with X2CpgAstNodeBuilder[SwiftNode, AstCreator] {

  protected val logger: Logger = LoggerFactory.getLogger(classOf[AstCreator])

  protected val scope = new Scope()

  protected val methodAstParentStack          = new Stack[NewNode]()
  protected val typeRefIdStack                = new Stack[NewTypeRef]
  protected val dynamicInstanceTypeStack      = new Stack[String]
  protected val localAstParentStack           = new Stack[NewBlock]()
  protected val functionNodeToNameAndFullName = mutable.HashMap.empty[SwiftNode, (String, String)]
  protected val usedVariableNames             = mutable.HashMap.empty[String, Int]
  protected val seenAliasTypes                = mutable.HashSet.empty[NewTypeDecl]

  protected lazy val definedSymbols: Map[String, String] = {
    config.defines.map {
      case define if define.contains("=") =>
        val s = define.split("=")
        s.head -> s(1)
      case define => define -> "true"
    }.toMap
  }

  override def createAst(): DiffGraphBuilder = {
    val fileContent = if (!config.disableFileContent) Option(parserResult.fileContent) else None
    val fileNode    = NewFile().name(parserResult.filename).order(0)
    fileContent.foreach(fileNode.content(_))
    val namespaceBlock = globalNamespaceBlock()
    methodAstParentStack.push(namespaceBlock)
    val astForFakeMethod =
      astInFakeMethod(namespaceBlock.fullName, parserResult.filename, parserResult.ast)
    val ast = Ast(fileNode).withChild(Ast(namespaceBlock).withChild(astForFakeMethod))
    Ast.storeInDiffGraph(ast, diffGraph)
    createVariableReferenceLinks()
    diffGraph
  }

  private def astInFakeMethod(fullName: String, path: String, ast: SwiftNode): Ast = {
    val name               = NamespaceTraversal.globalNamespaceName
    val fakeGlobalTypeDecl = typeDeclNode(ast, name, fullName, path, name, NodeTypes.NAMESPACE_BLOCK, fullName)
    methodAstParentStack.push(fakeGlobalTypeDecl)
    val fakeGlobalMethod =
      methodNode(ast, name, name, fullName, None, path, Option(NodeTypes.TYPE_DECL), Option(fullName))
    methodAstParentStack.push(fakeGlobalMethod)
    scope.pushNewMethodScope(fullName, name, fakeGlobalMethod, None)
    val sourceFileAst = astForNode(ast)
    val methodReturn  = newMethodReturnNode(Defines.Any, None, line(ast), column(ast))
    val modifiers = Seq(newModifierNode(ModifierTypes.VIRTUAL).order(0), newModifierNode(ModifierTypes.MODULE).order(1))
    Ast(fakeGlobalTypeDecl).withChild(
      methodAst(fakeGlobalMethod, Seq.empty, sourceFileAst, methodReturn, modifiers = modifiers)
    )
  }

  protected def astForNodeWithFunctionReferenceAndCall(node: SwiftNode): Ast = {
    node match {
      case func: FunctionDeclLike =>
        astForFunctionLike(func, shouldCreateFunctionReference = true, shouldCreateAssignmentCall = true).ast
      case _ =>
        astForNode(node)
    }
  }

  protected def astForNodeWithFunctionReference(node: SwiftNode): Ast = {
    node match {
      case func: FunctionDeclLike =>
        astForFunctionLike(func, shouldCreateFunctionReference = true).ast
      case _ =>
        astForNode(node)
    }
  }

  protected def astForNode(node: SwiftNode): Ast = node match {
    case swiftToken: SwiftToken             => astForSwiftToken(swiftToken)
    case syntax: Syntax                     => astForSyntax(syntax)
    case exprSyntax: ExprSyntax             => astForExprSyntax(exprSyntax)
    case typeSyntax: TypeSyntax             => astForTypeSyntax(typeSyntax)
    case declSyntax: DeclSyntax             => astForDeclSyntax(declSyntax)
    case patternSyntax: PatternSyntax       => astForPatternSyntax(patternSyntax)
    case stmtSyntax: StmtSyntax             => astForStmtSyntax(stmtSyntax)
    case syntaxCollection: SyntaxCollection => astForSyntaxCollection(syntaxCollection)
    case null                               => notHandledYet(node)
  }

  override protected def line(node: SwiftNode): Option[Int]      = node.startLine
  override protected def column(node: SwiftNode): Option[Int]    = node.startColumn
  override protected def lineEnd(node: SwiftNode): Option[Int]   = node.endLine
  override protected def columnEnd(node: SwiftNode): Option[Int] = node.endColumn

  private val lineOffsetTable =
    OffsetUtils.getLineOffsetTable(Option(parserResult.fileContent))
    // we add one last offset as the swift-syntax parser always expects one EOF newline
      :+ 0

  private def nodeOffsets(node: SwiftNode): Option[(Int, Int)] = {
    val offsets = for {
      lineNr      <- line(node)
      columnNr    <- column(node)
      lineEndNr   <- lineEnd(node)
      columnEndNr <- columnEnd(node)
    } yield OffsetUtils.coordinatesToOffset(lineOffsetTable, lineNr - 1, columnNr - 1, lineEndNr - 1, columnEndNr - 1)
    offsets.map { case (offset, offsetEnd) => (offset, offsetEnd - 1) }
  }

  override protected def offset(node: SwiftNode): Option[(Int, Int)] = {
    Option.when(!config.disableFileContent) { nodeOffsets(node) }.flatten
  }

  override protected def code(node: SwiftNode): String = {
    (nodeOffsets(node), node) match {
      case (Some((startOffset, endOffset)), _: TypeSyntax) =>
        parserResult.fileContent.substring(startOffset, endOffset).trim.stripSuffix("?").stripSuffix("!")
      case (Some((startOffset, endOffset)), _) =>
        shortenCode(parserResult.fileContent.substring(startOffset, endOffset).trim)
      case _ =>
        PropertyDefaults.Code
    }
  }
}
