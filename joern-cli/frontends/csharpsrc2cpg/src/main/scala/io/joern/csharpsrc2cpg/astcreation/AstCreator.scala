package io.joern.csharpsrc2cpg.astcreation

import io.joern.csharpsrc2cpg.Constants
import io.joern.csharpsrc2cpg.datastructures.{CSharpProgramSummary, CSharpScope, MethodScope, TypeScope}
import io.joern.csharpsrc2cpg.parser.DotNetJsonAst.*
import io.joern.csharpsrc2cpg.parser.{DotNetNodeInfo, ParserKeys}
import io.joern.csharpsrc2cpg.utils.Utils.*
import io.joern.x2cpg.astgen.{AstGenNodeBuilder, ParserResult}
import io.joern.x2cpg.utils.NodeBuilders.newModifierNode
import io.joern.x2cpg.{Ast, AstCreatorBase, ValidationMode}
import io.shiftleft.codepropertygraph.generated.{DiffGraphBuilder, ModifierTypes, NodeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{
  NewBlock,
  NewFile,
  NewMethod,
  NewMethodParameterIn,
  NewMethodReturn,
  NewModifier,
  NewTypeDecl
}
import org.slf4j.{Logger, LoggerFactory}
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
    val importAsts                              = cu.json(ParserKeys.Usings).arr.flatMap(astForNode).toSeq
    val members                                 = cu.json(ParserKeys.Members).arr.map(createDotNetNodeInfo).toSeq
    val (globalStatements, nonGlobalStatements) = members.partition(_.node == GlobalStatement)
    val nonGlobalStatementAsts                  = nonGlobalStatements.flatMap(astForNode)

    // If there are global statements, we should treat this file as an entry-point.
    // Roslyn implicitly wraps these statements inside the following block:
    // ```
    // internal class Program {
    //   private static void <Main>$(string[] args) {
    //      <globalStatements>
    //   }
    // }
    // ```
    // Note: there can only be one such file in a given project, but we are currently
    // not checking this.
    if (globalStatements.nonEmpty) {
      importAsts ++ astForTopLevelStatements(globalStatements) ++ nonGlobalStatementAsts
    } else {
      importAsts ++ nonGlobalStatementAsts
    }
  }

  private def astForTopLevelStatements(topLevelStmts: Seq[DotNetNodeInfo]): Seq[Ast] = {
    val className      = composeTopLevelClassName(relativeFileName)
    val classFullName  = className
    val mainName       = Constants.TopLevelMainMethodName
    val mainParameters = List(("args", "System.String[]"))
    val voidType       = BuiltinTypes.DotNetTypeMap(BuiltinTypes.Void)
    val mainSignature  = composeMethodLikeSignature(voidType, mainParameters.map(_._2))
    val mainFullName   = composeMethodFullName(classFullName, mainName, mainSignature)

    val classNode = NewTypeDecl()
      .name(className)
      .fullName(classFullName)
      .filename(relativeFileName)

    val classModifiers = newModifierNode(ModifierTypes.INTERNAL) :: Nil
    val methodNode = NewMethod()
      .name(mainName)
      .fullName(mainFullName)
      .filename(relativeFileName)
      .signature(mainSignature)

    val methodModifiers = newModifierNode(ModifierTypes.STATIC) :: newModifierNode(ModifierTypes.PRIVATE) :: Nil
    val argsParameters  = mainParameters.map(x => NewMethodParameterIn().name(x._1).typeFullName(x._2))
    val methodBlock     = NewBlock().typeFullName(voidType)
    val methodReturn    = NewMethodReturn().typeFullName(methodBlock.typeFullName)

    val topLevelStmtAsts = {
      scope.pushNewScope(TypeScope(classFullName))
      scope.pushNewScope(MethodScope(mainFullName))
      argsParameters.foreach(x => scope.addToScope(x.name, x))

      val asts = topLevelStmts.flatMap(astForNode)

      scope.popScope()
      scope.popScope()
      asts
    }

    val methodAst = Ast(methodNode)
      .withChildren(methodModifiers.map(Ast(_)))
      .withChild(Ast(argsParameters))
      .withChild(Ast(methodBlock).withChildren(topLevelStmtAsts))
      .withChild(Ast(methodReturn))

    val classAst = Ast(classNode)
      .withChildren(classModifiers.map(Ast(_)))
      .withChild(methodAst)

    Seq(classAst)
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
