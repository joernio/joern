package io.joern.csharpsrc2cpg.astcreation

import io.joern.csharpsrc2cpg.{CSharpDefines, Constants}
import io.joern.csharpsrc2cpg.datastructures.{CSharpProgramSummary, CSharpScope}
import io.joern.csharpsrc2cpg.parser.DotNetJsonAst.*
import io.joern.csharpsrc2cpg.parser.{DotNetNodeInfo, ParserKeys}
import io.joern.x2cpg.astgen.{AstGenNodeBuilder, ParserResult}
import io.joern.x2cpg.{Ast, AstCreatorBase, ValidationMode}
import io.shiftleft.codepropertygraph.generated.NodeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{NewFile, NewTypeDecl}
import org.slf4j.{Logger, LoggerFactory}
import io.shiftleft.codepropertygraph.generated.DiffGraphBuilder
import ujson.Value

import java.math.BigInteger
import java.security.MessageDigest

class AstCreator(
  val relativeFileName: String,
  val parserResult: ParserResult,
  val programSummary: CSharpProgramSummary = CSharpProgramSummary()
)(implicit withSchemaValidation: ValidationMode)
    extends AstCreatorBase(relativeFileName)
    with AstCreatorHelper
    with AstForDeclarationsCreator
    with AstForPrimitivesCreator
    with AstForExpressionsCreator
    with AstForStatementsCreator
    with AstSummaryVisitor
    with AstGenNodeBuilder[AstCreator] {

  protected val logger: Logger = LoggerFactory.getLogger(getClass)

  protected val scope: CSharpScope = new CSharpScope(programSummary)

  protected var parseLevel: AstParseLevel = AstParseLevel.FULL_AST

  override def createAst(): DiffGraphBuilder = {
    val hash = String.format(
      "%032x",
      new BigInteger(1, MessageDigest.getInstance("SHA-256").digest(parserResult.fileContent.getBytes("UTF-8")))
    )
    val fileNode        = NewFile().name(relativeFileName).content(parserResult.fileContent).order(1).hash(hash)
    val compilationUnit = createDotNetNodeInfo(parserResult.json(ParserKeys.AstRoot))
    val ast             = Ast(fileNode).withChildren(astForCompilationUnit(compilationUnit))
    Ast.storeInDiffGraph(ast, diffGraph)
    diffGraph
  }

  protected def astForCompilationUnit(cu: DotNetNodeInfo): Seq[Ast] = {
    val imports    = cu.json(ParserKeys.Usings).arr.flatMap(astForNode).toSeq
    val memberAsts = astForMembers(cu.json(ParserKeys.Members).arr.map(createDotNetNodeInfo).toSeq)
    imports ++ memberAsts
  }

  protected def astForMembers(members: Seq[DotNetNodeInfo]): Seq[Ast] = {
    val memberAsts = members.flatMap(astForNode)
    // If empty, then there is no parent namespace and this is a "global" type
    if (scope.peekScope().isEmpty) {
      val (typeDecls, other) = memberAsts.partition(x => x.root.exists(_.isInstanceOf[NewTypeDecl]))
      typeDecls
        .flatMap(_.root)
        .collect { case x: NewTypeDecl => x }
        .foreach(typ => typ.astParentFullName(Constants.Global).astParentType(NodeTypes.NAMESPACE_BLOCK))
      typeDecls.foreach(Ast.storeInDiffGraph(_, diffGraph))
      other
    } else {
      memberAsts
    }
  }

  protected def astForNode(json: Value): Seq[Ast] = {
    val nodeInfo = createDotNetNodeInfo(json)
    astForNode(nodeInfo)
  }

  protected def astForNode(nodeInfo: DotNetNodeInfo): Seq[Ast] = {
    nodeInfo.node match {
      case _: BaseStmt                                           => astForStatement(nodeInfo)
      case NamespaceDeclaration | FileScopedNamespaceDeclaration => astForNamespaceDeclaration(nodeInfo)
      case ClassDeclaration                                      => astForClassDeclaration(nodeInfo)
      case AnonymousObjectCreationExpression                     => astForAnonymousObjectCreationExpression(nodeInfo)
      case InterfaceDeclaration                                  => astForClassDeclaration(nodeInfo)
      case StructDeclaration                                     => astForClassDeclaration(nodeInfo)
      case RecordDeclaration                                     => astForRecordDeclaration(nodeInfo)
      case EnumDeclaration                                       => astForEnumDeclaration(nodeInfo)
      case EnumMemberDeclaration                                 => astForEnumMemberDeclaration(nodeInfo)
      case MethodDeclaration                                     => astForMethodDeclaration(nodeInfo)
      case ConstructorDeclaration                                => astForConstructorDeclaration(nodeInfo)
      case FieldDeclaration                                      => astForFieldDeclaration(nodeInfo)
      case VariableDeclaration                                   => astForVariableDeclaration(nodeInfo)
      case EqualsValueClause                                     => astForEqualsValueClause(nodeInfo)
      case ArrayInitializerExpression                            => astForArrayInitializerExpression(nodeInfo)
      case CollectionExpression                                  => astForCollectionExpression(nodeInfo)
      case UsingDirective                                        => astForUsing(nodeInfo) :: Nil
      case Block                                                 => Seq(astForBlock(nodeInfo))
      case IdentifierName                                        => Seq(astForIdentifier(nodeInfo))
      case LocalDeclarationStatement                             => astForLocalDeclarationStatement(nodeInfo)
      case FinallyClause                                         => astForFinallyClause(nodeInfo)
      case CatchClause                                           => astForCatchClause(nodeInfo)
      case CatchDeclaration                                      => astForCatchDeclaration(nodeInfo)
      case PropertyDeclaration                                   => astForPropertyDeclaration(nodeInfo)
      case ExpressionElement                                     => astForExpressionElement(nodeInfo)
      case _: BaseExpr                                           => astForExpression(nodeInfo)
      case _                                                     => notHandledYet(nodeInfo)
    }
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
