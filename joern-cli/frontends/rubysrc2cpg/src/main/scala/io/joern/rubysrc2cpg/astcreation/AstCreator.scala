package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.astcreation.RubyIntermediateAst.*
import io.joern.rubysrc2cpg.datastructures.{BlockScope, NamespaceScope, RubyProgramSummary, RubyScope, RubyStubbedType}
import io.joern.rubysrc2cpg.parser.{RubyNodeCreator, RubyParser}
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.x2cpg.utils.NodeBuilders.newModifierNode
import io.joern.x2cpg.{Ast, AstCreatorBase, AstNodeBuilder, ValidationMode}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, ModifierTypes, Operators}
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
    val rubyFileMethod = astInFakeMethod(rootStatements)
    scope.popScope()

    Ast(fileNode).withChild(Ast(namespaceBlock).withChild(rubyFileMethod))
  }

  private def astInFakeMethod(rootNode: StatementList): Ast = {
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

    scope.newProgramScope
      .map { moduleScope =>
        scope.pushNewScope(moduleScope)
        val block = blockNode(rootNode)
        scope.pushNewScope(BlockScope(block))
        val statementAsts         = rootNode.statements.flatMap(astsForStatement)
        val internalMethodRefAsts = methodRefNodesForInternalMethods()
        val internalTypeRefAsts   = typeRefNodesForInternalDecls()
        scope.popScope()
        val bodyAst = blockAst(block, internalTypeRefAsts ++ internalMethodRefAsts ++ statementAsts)
        scope.popScope()
        methodAst(methodNode_, Seq.empty, bodyAst, methodReturn, newModifierNode(ModifierTypes.MODULE) :: Nil)
      }
      .getOrElse(Ast())
  }

  private def methodRefNodesForInternalMethods(): List[Ast] = {
    val typeNameForMethods = scope.surroundingTypeFullName
      .map { x =>
        x.stripSuffix(s":${Defines.Program}")
      }
      .getOrElse(Defines.Undefined)

    scope.surroundingTypeFullName
      .map { x =>
        val typeNameForMethods = x.stripSuffix(s":${Defines.Program}")
        programSummary.namespaceToType
          .filter(_._1 == typeNameForMethods)
          .flatMap(_._2)
          .filter(!_.isInstanceOf[RubyStubbedType])
          .flatMap(_.methods)
          .map { method =>
            val methodRefNode = NewMethodRef()
              .code(s"def ${method.name} (...)")
              .methodFullName(scope.surroundingTypeFullName.map { x => s"$x:${method.name}" }.getOrElse(method.name))
              .typeFullName(Defines.Any)
              .lineNumber(internalLineAndColNum)
              .columnNumber(internalLineAndColNum)

            val methodRefIdent = NewIdentifier()
              .code(method.name)
              .name(method.name)
              .typeFullName(Defines.Any)
              .lineNumber(internalLineAndColNum)
              .columnNumber(internalLineAndColNum)

            astForAssignment(methodRefIdent, methodRefNode, internalLineAndColNum, internalLineAndColNum)
          }
          .toList
      }
      .getOrElse(List.empty)

  }

  private def typeRefNodesForInternalDecls(): List[Ast] = {
    scope.surroundingTypeFullName
      .map { surroundingTypeFullName =>
        programSummary.namespaceToType
          .filter(_._1.contains(surroundingTypeFullName))
          .flatMap(_._2)
          .map { x =>
            val typeRefName = x.name.split("[.]").takeRight(1).head
            val typeRefNode = NewTypeRef()
              .code(s"class ${x.name} (...)")
              .typeFullName(x.name)

            val typeRefIdent = NewIdentifier()
              .code(typeRefName)
              .name(typeRefName)
              .typeFullName(x.name)
              .lineNumber(internalLineAndColNum)
              .columnNumber(internalLineAndColNum)

            astForAssignment(typeRefIdent, typeRefNode, internalLineAndColNum, internalLineAndColNum)
          }
          .toList
      }
      .getOrElse(List.empty)

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
