package io.joern.csharpsrc2cpg.astcreation

import io.joern.csharpsrc2cpg.parser.DotNetJsonAst.*
import io.joern.csharpsrc2cpg.parser.{DotNetNodeInfo, ParserKeys}
import io.joern.x2cpg.astgen.{AstGenNodeBuilder, ParserResult}
import io.joern.x2cpg.datastructures.Stack.Stack
import io.joern.x2cpg.{Ast, AstCreatorBase, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.{NewFile, NewNode}
import org.slf4j.{Logger, LoggerFactory}
import overflowdb.BatchedUpdate.DiffGraphBuilder
import ujson.Value

import java.math.BigInteger
import java.security.MessageDigest

class AstCreator(val relativeFileName: String, val parserResult: ParserResult)(implicit
  withSchemaValidation: ValidationMode
) extends AstCreatorBase(relativeFileName)
    with AstCreatorHelper
    with AstForDeclarationsCreator
    with AstForExpressionsCreator
    with AstGenNodeBuilder[AstCreator] {

  protected val logger: Logger = LoggerFactory.getLogger(getClass)

  protected val methodAstParentStack = new Stack[NewNode]()

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

  private def astForCompilationUnit(cu: DotNetNodeInfo): Seq[Ast] = {
    val imports    = cu.json(ParserKeys.Usings).arr.map(createDotNetNodeInfo).toSeq // TODO: Handle imports
    val memberAsts = astForMembers(cu.json(ParserKeys.Members).arr.map(createDotNetNodeInfo).toSeq)
    memberAsts
  }

  protected def astForMembers(members: Seq[DotNetNodeInfo]): Seq[Ast] = members.flatMap(astForNode)

  protected def astForNode(json: Value): Seq[Ast] = {
    val nodeInfo = createDotNetNodeInfo(json)
    astForNode(nodeInfo)
  }

  protected def astForNode(nodeInfo: DotNetNodeInfo): Seq[Ast] = {
    nodeInfo.node match {
      case NamespaceDeclaration => astForNamespaceDeclaration(nodeInfo)
      case ClassDeclaration     => astForClassDeclaration(nodeInfo)
      case MethodDeclaration    => astForMethodDeclaration(nodeInfo)
      case FieldDeclaration     => astForFieldDeclaration(nodeInfo)
      case VariableDeclaration  => astForVariableDeclaration(nodeInfo)
      case EqualsValueClause    => astForEqualsValueClause(nodeInfo)
      case UsingDirective       => notHandledYet(nodeInfo)
      case Block                => notHandledYet(nodeInfo)
      case _                    => notHandledYet(nodeInfo)
    }
  }

}
