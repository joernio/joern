package io.joern.swiftsrc2cpg.astcreation

import io.joern.swiftsrc2cpg.Config
import io.joern.swiftsrc2cpg.parser.SwiftJsonParser.ParseResult
import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.*
import io.joern.x2cpg.{Ast, AstCreatorBase, ValidationMode}
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.datastructures.{Global, VariableScopeManager}
import io.joern.x2cpg.frontendspecific.swiftsrc2cpg.Defines
import io.shiftleft.codepropertygraph.generated.{DiffGraphBuilder, ModifierTypes, NodeTypes, PropertyDefaults}
import io.shiftleft.codepropertygraph.generated.nodes.{NewBlock, NewFile, NewNode, NewTypeDecl, NewTypeRef}
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable

class AstCreator(val config: Config, val global: Global, val parserResult: ParseResult)(implicit
  withSchemaValidation: ValidationMode
) extends AstCreatorBase[SwiftNode, AstCreator](parserResult.filename)
    with AstForSwiftTokenCreator
    with AstForSyntaxCreator
    with AstForExprSyntaxCreator
    with AstForTypeSyntaxCreator
    with AstForDeclSyntaxCreator
    with AstForPatternSyntaxCreator
    with AstForStmtSyntaxCreator
    with AstForSyntaxCollectionCreator
    with AstCreatorHelper
    with AstNodeBuilder {

  protected val logger: Logger = LoggerFactory.getLogger(classOf[AstCreator])

  protected val scope = new VariableScopeManager()

  protected val methodAstParentStack     = new Stack[NewNode]()
  protected val typeRefIdStack           = new Stack[NewTypeRef]
  protected val dynamicInstanceTypeStack = new Stack[String]
  protected val localAstParentStack      = new Stack[NewBlock]()
  protected val scopeLocalUniqueNames    = mutable.HashMap.empty[String, Int]
  protected val seenAliasTypes           = mutable.HashSet.empty[NewTypeDecl]

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
    scope.createVariableReferenceLinks(diffGraph, parserResult.filename)
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
    val methodReturn  = methodReturnNode(ast, Defines.Any)
    val modifiers =
      Seq(modifierNode(ast, ModifierTypes.VIRTUAL).order(0), modifierNode(ast, ModifierTypes.MODULE).order(1))
    Ast(fakeGlobalTypeDecl).withChild(
      methodAst(fakeGlobalMethod, Seq.empty, sourceFileAst, methodReturn, modifiers = modifiers)
    )
  }

  protected def astForNode(node: SwiftNode): Ast = node match {
    case func: FunctionDeclLike             => astForFunctionLike(func)
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

  private def nodeOffsets(node: SwiftNode): Option[(Int, Int)] = {
    for {
      startOffset <- node.startOffset
      endOffset   <- node.endOffset
    } yield (math.max(startOffset, 0), math.min(endOffset, parserResult.fileContent.length))
  }

  override protected def offset(node: SwiftNode): Option[(Int, Int)] = {
    Option.when(!config.disableFileContent) { nodeOffsets(node) }.flatten
  }

  override protected def code(node: SwiftNode): String = {
    (nodeOffsets(node), node) match {
      case (Some((startOffset, endOffset)), _: TypeSyntax) =>
        parserResult.fileContent.substring(startOffset, endOffset).trim.stripSuffix("?").stripSuffix("!")
      case (Some((startOffset, endOffset)), _: identifier) =>
        parserResult.fileContent.substring(startOffset, endOffset).trim.stripSuffix("()")
      case (Some((startOffset, endOffset)), _) =>
        shortenCode(parserResult.fileContent.substring(startOffset, endOffset).trim)
      case _ =>
        PropertyDefaults.Code
    }
  }
}
