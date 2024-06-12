package io.joern.gosrc2cpg.astcreation

import better.files.File
import io.joern.gosrc2cpg.datastructures.GoGlobal
import io.joern.gosrc2cpg.model.GoModHelper
import io.joern.gosrc2cpg.parser.ParserAst.*
import io.joern.gosrc2cpg.parser.{GoAstJsonParser, ParserKeys, ParserNodeInfo}
import io.joern.x2cpg.astgen.{AstGenNodeBuilder, ParserResult}
import io.joern.x2cpg.datastructures.Scope
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.utils.NodeBuilders.newModifierNode
import io.joern.x2cpg.{Ast, AstCreatorBase, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.NewNode
import io.shiftleft.codepropertygraph.generated.{ModifierTypes, NodeTypes}
import org.slf4j.{Logger, LoggerFactory}
import overflowdb.BatchedUpdate.DiffGraphBuilder
import ujson.Value

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}
import java.nio.file.{Files, Path, Paths}
import java.util.UUID
import scala.collection.mutable

class AstCreator(
  val jsonAstFilePath: String,
  val relPathFileName: String,
  val goMod: GoModHelper,
  val goGlobal: GoGlobal,
  tmpDir: Option[File] = None
)(implicit withSchemaValidation: ValidationMode)
    extends AstCreatorBase(relPathFileName)
    with AstCreatorHelper
    with AstForGenDeclarationCreator
    with AstForExpressionCreator
    with AstForFunctionsCreator
    with AstForPrimitivesCreator
    with AstForStatementsCreator
    with AstForTypeDeclCreator
    with AstForMethodCallExpressionCreator
    with CacheBuilder
    with AstForLambdaCreator
    with AstGenNodeBuilder[AstCreator] {

  protected val logger: Logger = LoggerFactory.getLogger(classOf[AstCreator])
  protected val tempAliasToNameSpaceMappingFilePath: Option[Path] =
    tmpDir.map(dir => Paths.get(dir.pathAsString, s"alias-cache${UUID.randomUUID().toString}"))
  protected val methodAstParentStack: Stack[NewNode]                 = new Stack()
  protected val scope: Scope[String, (NewNode, String), NewNode]     = new Scope()
  protected var aliasToNameSpaceMapping: mutable.Map[String, String] = mutable.Map.empty
  protected var parserNodeCache                                      = mutable.TreeMap[Long, ParserNodeInfo]()
  protected var lineNumberMapping: Map[Int, String]                  = Map.empty
  protected var declaredPackageName                                  = ""
  protected var fullyQualifiedPackage                                = ""
  protected var fileName                                             = ""

  var originalFilePath = ""

  override def createAst(): DiffGraphBuilder = {
    val parserResult = init()
    loadCacheToProcess()
    val rootNode = createParserNodeInfo(parserResult.json)
    val ast      = astForTranslationUnit(rootNode, parserResult)
    Ast.storeInDiffGraph(ast, diffGraph)
    diffGraph
  }

  private def astForTranslationUnit(rootNode: ParserNodeInfo, parserResult: ParserResult): Ast = {
    val name     = s"$fullyQualifiedPackage.${parserResult.filename}"
    val fullName = s"$relPathFileName:$name"
    val fakeGlobalMethodForFile =
      methodNode(
        rootNode,
        name,
        name,
        fullName,
        None,
        relPathFileName,
        Option(NodeTypes.TYPE_DECL),
        Option(fullyQualifiedPackage)
      )
    methodAstParentStack.push(fakeGlobalMethodForFile)
    scope.pushNewScope(fakeGlobalMethodForFile)
    val blockNode_   = blockNode(rootNode, Defines.empty, Defines.anyTypeName)
    val methodReturn = methodReturnNode(rootNode, Defines.anyTypeName)
    val declsAsts = rootNode
      .json(ParserKeys.Decls)
      .arr
      .flatMap { item =>
        val node = createParserNodeInfo(item)
        astForNode(node, true)
      }
      .toList
    methodAstParentStack.pop()
    scope.popScope()
    methodAst(
      fakeGlobalMethodForFile,
      Seq.empty,
      blockAst(blockNode_, declsAsts),
      methodReturn,
      newModifierNode(ModifierTypes.MODULE) :: Nil
    )
  }

  protected def astForNode(nodeInfo: ParserNodeInfo, globalStatements: Boolean = false): Seq[Ast] = {
    nodeInfo.node match {
      case GenDecl          => astForGenDecl(nodeInfo, globalStatements)
      case FuncDecl         => astForFuncDecl(nodeInfo)
      case _: BasePrimitive => astForPrimitive(nodeInfo)
      case _: BaseExpr      => astsForExpression(nodeInfo)
      case _: BaseStmt      => astsForStatement(nodeInfo)
      case _                => Seq()
    }
  }

  protected def astForNode(json: Value): Seq[Ast] = {
    astForNode(createParserNodeInfo(json))
  }

  def init(): ParserResult = {
    val parserResult = GoAstJsonParser.readFile(Paths.get(jsonAstFilePath))
    lineNumberMapping = positionLookupTables(parserResult)
    declaredPackageName = parserResult.json(ParserKeys.Name)(ParserKeys.Name).str
    fullyQualifiedPackage = goMod.getNameSpace(parserResult.fullPath, declaredPackageName)
    fileName = parserResult.filename
    originalFilePath = parserResult.fullPath
    parserResult
  }

  def cacheSerializeAndStore(): Unit = {
    tempAliasToNameSpaceMappingFilePath.map(file => {
      Files.write(file, serialise(aliasToNameSpaceMapping))
      aliasToNameSpaceMapping.clear()
    })
    lineNumberMapping = Map.empty
  }

  def loadCacheToProcess(): Unit = {
    tempAliasToNameSpaceMappingFilePath.map(file => {
      val deserialised = deserialise(Files.readAllBytes(file))
      aliasToNameSpaceMapping = deserialised.asInstanceOf[mutable.Map[String, String]]
    })
  }

  def cleanup(): Unit = {
    methodAstParentStack.clear()
    aliasToNameSpaceMapping.clear()
    lineNumberMapping = Map.empty
    parserNodeCache.clear()
    tempAliasToNameSpaceMappingFilePath.map(file => {
      if (Files.exists(file)) {
        Files.delete(file)
      }
    })
  }

  /** Serialise any object to byte array to be passed through queue
    *
    * @param value
    *   \- Any object to passed through queue as a result item.
    * @return
    *   \- Object serialised into ByteArray
    */
  private def serialise(value: Any): Array[Byte] = {
    val stream: ByteArrayOutputStream = new ByteArrayOutputStream()
    val oos                           = new ObjectOutputStream(stream)
    oos.writeObject(value)
    oos.close()
    stream.toByteArray
  }

  /** Deserialize the ByteArray back to Object.
    *
    * @param bytes
    *   \- Array[Byte] to be deserialized
    * @return
    *   \- Deserialized object
    */
  private def deserialise(bytes: Array[Byte]): Any = {
    val ois   = new ObjectInputStream(new ByteArrayInputStream(bytes))
    val value = ois.readObject
    ois.close()
    value
  }
}
