package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.astcreation.RubyIntermediateAst.*
import io.joern.rubysrc2cpg.datastructures.{BlockScope, NamespaceScope, RubyProgramSummary, RubyScope, RubyStubbedType}
import io.joern.rubysrc2cpg.parser.{RubyNodeCreator, RubyParser}
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.x2cpg.utils.NodeBuilders.{newBindingNode, newModifierNode}
import io.joern.x2cpg.{Ast, AstCreatorBase, AstNodeBuilder, ValidationMode}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EdgeTypes, ModifierTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import org.slf4j.{Logger, LoggerFactory}
import overflowdb.BatchedUpdate
import overflowdb.BatchedUpdate.DiffGraphBuilder

import java.util.regex.Matcher

class AstCreator(
  val fileName: String,
  protected val programCtx: RubyParser.ProgramContext,
  protected val projectRoot: Option[String] = None,
  protected val programSummary: RubyProgramSummary = RubyProgramSummary()
)(implicit withSchemaValidation: ValidationMode)
    extends AstCreatorBase(fileName)
    with AstCreatorHelper
    with AstForStatementsCreator
    with AstForExpressionsCreator
    with AstForFunctionsCreator
    with AstForTypesCreator
    with AstSummaryVisitor
    with AstNodeBuilder[RubyNode, AstCreator] {

  /* Used to track variable names and their LOCAL nodes.
   */
  protected val scope: RubyScope = new RubyScope(programSummary, projectRoot)

  protected val logger: Logger = LoggerFactory.getLogger(getClass)

  protected var parseLevel: AstParseLevel = AstParseLevel.FULL_AST

  protected val relativeFileName: String =
    projectRoot
      .map(fileName.stripPrefix)
      .map(_.stripPrefix(java.io.File.separator))
      .getOrElse(fileName)

  private def internalLineAndColNum: Option[Integer] = Option(1)

  /** The relative file name, in a unix path delimited format.
    */
  private def relativeUnixStyleFileName =
    relativeFileName.replaceAll(Matcher.quoteReplacement(java.io.File.separator), "/")

  override def createAst(): BatchedUpdate.DiffGraphBuilder = {
    val rootNode = new RubyNodeCreator().visit(programCtx).asInstanceOf[StatementList]
    val ast      = astForRubyFile(rootNode)
    Ast.storeInDiffGraph(ast, diffGraph)
    diffGraph
  }

  /* A Ruby file has the following AST hierarchy: FILE -> NAMESPACE_BLOCK -> METHOD.
   * The (parsed) contents of the file are put under that fictitious METHOD node, thus
   * allowing for a straightforward representation of out-of-method statements.
   */
  protected def astForRubyFile(rootStatements: StatementList): Ast = {
    val fileNode = NewFile().name(relativeFileName)
    val fullName = s"$relativeUnixStyleFileName:${NamespaceTraversal.globalNamespaceName}"
    val namespaceBlock = NewNamespaceBlock()
      .filename(relativeFileName)
      .name(NamespaceTraversal.globalNamespaceName)
      .fullName(fullName)

    scope.pushNewScope(NamespaceScope(fullName))
    val (rubyFakeMethod, rubyFakeMethodAst) = astInFakeMethod(rootStatements)
    val rubyFakeTypeDecl                    = astInFakeTypeDecl(rootStatements, rubyFakeMethod)
    scope.popScope()

    Ast(fileNode).withChild(Ast(namespaceBlock).withChild(rubyFakeMethodAst).withChild(rubyFakeTypeDecl))
  }

  private def astInFakeMethod(rootNode: StatementList): (NewMethod, Ast) = {
    val name     = Defines.Program
    val fullName = computeMethodFullName(name)
    val code     = rootNode.text
    val methodNode_ = methodNode(
      node = rootNode,
      name = name,
      code = code,
      fullName = fullName,
      signature = None,
      fileName = relativeFileName
    )
    val methodReturn = methodReturnNode(rootNode, Defines.Any)

    methodNode_ -> scope.newProgramScope
      .map { moduleScope =>
        scope.pushNewScope(moduleScope)
        val block = blockNode(rootNode)
        scope.pushNewScope(BlockScope(block))
        val statementAsts = rootNode.statements.flatMap(astsForStatement)
        scope.popScope()
        val bodyAst = blockAst(block, statementAsts)
        scope.popScope()
        methodAst(methodNode_, Seq.empty, bodyAst, methodReturn, newModifierNode(ModifierTypes.MODULE) :: Nil)
      }
      .getOrElse(Ast())
  }

  private def astInFakeTypeDecl(rootNode: StatementList, method: NewMethod): Ast = {
    val typeDeclNode_ = typeDeclNode(rootNode, method.name, method.fullName, method.filename, Nil, None)
    val members = rootNode.statements
      .collect {
        case m: MethodDeclaration =>
          val methodName = m.methodName
          NewMember()
            .name(methodName)
            .code(methodName)
            .typeFullName(Defines.Any)
            .dynamicTypeHintFullName(s"${method.fullName}:$methodName" :: Nil)
        case t: TypeDeclaration =>
          val typeName = t.name.text
          NewMember()
            .name(t.name.text)
            .code(typeName)
            .typeFullName(Defines.Any)
            .dynamicTypeHintFullName(s"${method.fullName}.$typeName" :: Nil)
      }
      .map(Ast.apply)

    val bindingNode = newBindingNode("", "", method.fullName)
    diffGraph.addEdge(typeDeclNode_, bindingNode, EdgeTypes.BINDS)
    diffGraph.addEdge(bindingNode, method, EdgeTypes.REF)

    Ast(typeDeclNode_).withChild(Ast(newModifierNode(ModifierTypes.MODULE))).withChildren(members)
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
