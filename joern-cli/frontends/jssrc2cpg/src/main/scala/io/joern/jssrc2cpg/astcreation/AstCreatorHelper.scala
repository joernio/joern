package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.parser.BabelNodeSyntax.*
import io.joern.x2cpg.frontendspecific.jssrc2cpg.Defines
import io.joern.x2cpg.utils.IntervalKeyPool
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.{
  AstNodeNew,
  ExpressionNew,
  NewBinding,
  NewIdentifier,
  NewMethod,
  NewNode,
  NewTypeRef
}
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, PropertyDefaults, PropertyNames}

import scala.collection.mutable

trait AstCreatorHelper(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  private val anonClassKeyPool = new IntervalKeyPool(first = 0, last = Long.MaxValue)

  /** Functions that have a body, i.e. everything we create a full method AST for. */
  type FunctionWithBody =
    FunctionDeclaration | FunctionExpression | ArrowFunctionExpression | ObjectMethod | ClassMethod | ClassPrivateMethod

  /** Function-like declarations without a body, e.g. overload signatures or members of interfaces. */
  type FunctionSignature =
    TSDeclareFunction | TSDeclareMethod | TSCallSignatureDeclaration | TSConstructSignatureDeclaration |
      TSMethodSignature

  type FunctionNode = FunctionWithBody | FunctionSignature

  /** Everything that is a function expression or declaration in the JS sense (so, no methods). */
  type FunctionLike = FunctionDeclaration | FunctionExpression | ArrowFunctionExpression

  /** Source information of a node, resolved once per node instance on demand. A node's code, line and column values are
    * queried several times while its AST is created, and resolving them requires a substring and a binary search each.
    * The cache is keyed by identity as the typed nodes are case classes wrapping JSON and would be compared (and
    * hashed) structurally otherwise.
    */
  private case class NodeLocation(
    code: String,
    line: Option[Int],
    column: Option[Int],
    lineEnd: Option[Int],
    columnEnd: Option[Int]
  )

  private val nodeLocations = new java.util.IdentityHashMap[BabelNode, NodeLocation]()

  private def locationOf(node: BabelNode): NodeLocation = {
    val cached = nodeLocations.get(node)
    if (cached != null) {
      cached
    } else {
      // Lines and columns come from the `loc` of the node as delivered by the parser. Note that the parser ends a
      // line at `\n`, `\r\n`, a lone `\r`, `\u2028` and `\u2029`.
      val location = NodeLocation(
        codeForOffsets(node.start, node.end),
        node.startLine,
        node.startColumn,
        node.endLine,
        node.endColumn
      )
      nodeLocations.put(node, location)
      location
    }
  }

  /** Source information of a node. This is an implicit class and no extension, as `code(node)` is abstract in the node
    * builder and has to be implemented as an ordinary method (see [[AstCreator]]).
    */
  implicit class BabelNodeSourceInfo(node: BabelNode) {
    def code: String                 = locationOf(node).code
    def lineNumber: Option[Int]      = locationOf(node).line
    def columnNumber: Option[Int]    = locationOf(node).column
    def lineNumberEnd: Option[Int]   = locationOf(node).lineEnd
    def columnNumberEnd: Option[Int] = locationOf(node).columnEnd
  }

  /** Clamps a raw start/end offset pair to the bounds of the file content. */
  protected def clampOffsets(startOffset: Int, endOffset: Int): (Int, Int) =
    (math.max(startOffset, 0), math.min(endOffset, parserResult.fileContent.length))

  private def codeForOffsets(startOffset: Option[Int], endOffset: Option[Int]): String =
    startOffset
      .zip(endOffset)
      .map { case (startPosition, endPosition) =>
        val (clampedStart, clampedEnd) = clampOffsets(startPosition, endPosition)
        shortenCode(parserResult.fileContent.substring(clampedStart, clampedEnd).trim)
      }
      .getOrElse(PropertyDefaults.Code)

  protected def range(node: BabelNode): Option[String] = {
    for {
      nodeStart <- node.start
      nodeEnd   <- node.end
    } yield s"$nodeStart:$nodeEnd"
  }

  /** A cheap, stable key identifying a function node within a file, used to memoize its (name, fullName). We rely on
    * the node's source range, falling back to its rendered JSON when offsets are unavailable.
    */
  protected def functionNodeKey(node: BabelNode): String =
    range(node).getOrElse(node.json.render())

  protected def setOrderExplicitly(ast: Ast, order: Int): Unit = {
    ast.root.foreach { case expr: ExpressionNew => expr.order = order }
  }

  protected def notHandledYet(node: BabelNode): Ast = {
    val text =
      s"""Node type '${node.nodeType}' not handled yet!
         |  Code: '${node.code}'
         |  File: '${parserResult.fullPath}'
         |  Line: ${node.lineNumber.getOrElse(-1)}
         |  Column: ${node.columnNumber.getOrElse(-1)}
         |  """.stripMargin
    logger.info(text)
    Ast(unknownNode(node, node.code))
  }

  protected def createFunctionTypeAndTypeDeclAst(
    node: BabelNode,
    methodNode: NewMethod,
    parentNode: NewNode,
    methodName: String,
    methodFullName: String,
    filename: String
  ): Ast = {
    registerType(methodFullName)

    val astParentType        = parentNode.label
    val astParentFullName    = parentNode.properties(PropertyNames.FullName).toString
    val functionTypeDeclNode =
      typeDeclNode(
        node,
        methodName,
        methodFullName,
        filename,
        methodName,
        astParentType = astParentType,
        astParentFullName = astParentFullName,
        List(Defines.Any)
      )

    functionTypeDecls.update(methodFullName, functionTypeDeclNode)

    // Problem for https://github.com/ShiftLeftSecurity/codescience/issues/3626 here.
    // As the type (thus, the signature) of the function node is unknown (i.e., ANY*)
    // we can't generate the correct binding with signature.
    val bindingNode = NewBinding().name("").signature("")
    Ast(functionTypeDeclNode).withBindsEdge(functionTypeDeclNode, bindingNode).withRefEdge(bindingNode, methodNode)
  }

  protected def registerType(typeFullName: String): Unit = {
    usedTypes.add(typeFullName)
  }

  protected def codeForNodes(nodes: Seq[NewNode]): Option[String] = nodes.collectFirst {
    case id: NewIdentifier => id.name.replace("...", "")
    case clazz: NewTypeRef => clazz.code.stripPrefix("class ")
  }

  protected def nameForBabelNode(node: BabelNode, defaultName: Option[String]): String = {
    defaultName
      .orElse(codesForNode(node).headOption)
      .getOrElse {
        val tmpName    = generateUnusedVariableName(usedVariableNames, "_tmp")
        val nLocalNode = localNode(node, tmpName, tmpName, Defines.Any).order(0)
        diffGraph.addEdge(localAstParentStack.head, nLocalNode, EdgeTypes.AST)
        tmpName
      }
  }

  protected def generateUnusedVariableName(
    usedVariableNames: mutable.HashMap[String, Int],
    variableName: String
  ): String = {
    val counter             = usedVariableNames.getOrElse(variableName, -1) + 1
    val currentVariableName = s"${variableName}_$counter"
    usedVariableNames.update(variableName, counter)
    currentVariableName
  }

  protected def calcMethodNameAndFullName(func: FunctionNode): (String, String) = {
    // functionNode.getName is not necessarily unique and thus the full name calculated based on the scope
    // is not necessarily unique. Specifically we have this problem with lambda functions which are defined
    // in the same scope.
    val funcKey = functionNodeKey(func)
    functionNodeToNameAndFullName.get(funcKey) match {
      case Some(nameAndFullName) => nameAndFullName
      case None                  =>
        val intendedName   = calcMethodName(func)
        val fullNamePrefix = s"${parserResult.filename}:${scope.computeScopePath}:"
        var name           = intendedName
        var fullName       = ""
        var isUnique       = false
        var i              = 1
        while (!isUnique) {
          fullName = s"$fullNamePrefix$name"
          if (functionFullNames.contains(fullName)) {
            name = s"$intendedName$i"
            i += 1
          } else {
            isUnique = true
          }
        }
        functionFullNames.add(fullName)
        functionNodeToNameAndFullName(funcKey) = (name, fullName)
        (name, fullName)
    }
  }

  /** The parameters of a function; signatures without a body call them `parameters`, everything else `params`. */
  protected def paramsOf(func: FunctionNode): Seq[BabelNode] = func match {
    case f: FunctionDeclaration             => f.params
    case f: FunctionExpression              => f.params
    case f: ArrowFunctionExpression         => f.params
    case f: ObjectMethod                    => f.params
    case f: ClassMethod                     => f.params
    case f: ClassPrivateMethod              => f.params
    case f: TSDeclareFunction               => f.params
    case f: TSDeclareMethod                 => f.params
    case f: TSCallSignatureDeclaration      => f.parameters
    case f: TSConstructSignatureDeclaration => f.parameters
    case f: TSMethodSignature               => f.parameters
  }

  protected def bodyOf(func: FunctionWithBody): BlockStatement | Expression = func match {
    case f: FunctionDeclaration     => f.body
    case f: FunctionExpression      => f.body
    case f: ArrowFunctionExpression => f.body
    case f: ObjectMethod            => f.body
    case f: ClassMethod             => f.body
    case f: ClassPrivateMethod      => f.body
  }

  /** The name of a method, getter or setter is its key; plain functions are named by their identifier. */
  private def methodKey(func: FunctionNode): Option[BabelNode] = func match {
    case f: ObjectMethod       => Some(f.key)
    case f: ClassMethod        => Some(f.key)
    case f: ClassPrivateMethod => Some(f.key)
    case f: TSDeclareMethod    => Some(f.key)
    case f: TSMethodSignature  => Some(f.key)
    case _                     => None
  }

  private def methodKind(func: FunctionNode): Option[String] = func match {
    case f: ObjectMethod       => Some(f.kind)
    case f: ClassMethod        => Some(f.kind)
    case f: ClassPrivateMethod => Some(f.kind)
    case f: TSDeclareMethod    => f.kind
    case f: TSMethodSignature  => Some(f.kind)
    case _                     => None
  }

  private def functionId(func: FunctionNode): Option[Identifier] = func match {
    case f: FunctionDeclaration     => f.id
    case f: FunctionExpression      => f.id
    case f: ArrowFunctionExpression => f.id
    case f: ClassMethod             => f.id
    case f: ClassPrivateMethod      => f.id
    case f: ObjectMethod            => f.id
    case f: TSDeclareFunction       => f.id
    case f: TSDeclareMethod         => f.id
    case _                          => None
  }

  private def calcMethodName(func: FunctionNode): String = func match {
    case f: ObjectMethod if f.computed =>
      generateUnusedVariableName(usedVariableNames, "_computed_object_method")
    case f: ObjectMethod if isMethodOrGetSet(func) && f.key.code.startsWith("'") =>
      nextClosureName()
    case _: TSCallSignatureDeclaration =>
      nextClosureName()
    case _: TSConstructSignatureDeclaration =>
      io.joern.x2cpg.Defines.ConstructorMethodName
    case _ if isMethodOrGetSet(func) =>
      methodKey(func).get match {
        case key: Identifier => key.name
        case key             => key.code
      }
    case _ if methodKind(func).contains("constructor") =>
      io.joern.x2cpg.Defines.ConstructorMethodName
    case _ =>
      functionId(func) match {
        case Some(id) => id.name
        case None     => nextClosureName()
      }
  }

  private def isMethodOrGetSet(func: FunctionNode): Boolean =
    methodKind(func).exists(kind => kind == "method" || kind == "get" || kind == "set")

  protected def codeOf(node: NewNode): String = node match {
    case astNodeNew: AstNodeNew => astNodeNew.code
    case _                      => ""
  }

  protected def stripQuotes(str: String): String = str
    .stripPrefix("\"")
    .stripSuffix("\"")
    .stripPrefix("'")
    .stripSuffix("'")
    .stripPrefix("`")
    .stripSuffix("`")

  protected def calcTypeNameAndFullName(
    classNode: BabelNode,
    preCalculatedName: Option[String] = None
  ): (String, String) = {
    val name           = preCalculatedName.getOrElse(calcTypeName(classNode))
    val fullNamePrefix = s"${parserResult.filename}:${scope.computeScopePath}:"
    val fullName       = s"$fullNamePrefix$name"
    (name, fullName)
  }

  private def typeId(node: BabelNode): Option[BabelNode] = node match {
    case n: ClassDeclaration       => n.id
    case n: ClassExpression        => n.id
    case n: DeclareClass           => Some(n.id)
    case n: TSTypeAliasDeclaration => Some(n.id)
    case n: DeclareTypeAlias       => Some(n.id)
    case n: TypeAlias              => Some(n.id)
    case n: TSEnumDeclaration      => Some(n.id)
    case n: TSInterfaceDeclaration => Some(n.id)
    case n: TSModuleDeclaration    => Some(n.id)
    case n: GenericTypeAnnotation  => Some(n.id)
    case _                         => None
  }

  /** In JS, it is possible to create anonymous classes. We have to handle this here.
    */
  private def calcTypeName(classNode: BabelNode): String =
    typeId(classNode).map(_.code).getOrElse(nextAnonClassName())

  /** If `node` is the callee of a fake EJS output call, its source bytes are exactly the EJS output-tag prefix `<%`;
    * the character right after (the tag's third char) selects escaped (`=`) vs. unescaped (`-`) output. Returns the CPG
    * call name to use, or None for every ordinary node. The `<%` guard is safe: a bare `<%` is never the full source
    * text of a real JS/TS identifier.
    */
  protected def ejsOutputCallName(node: BabelNode): Option[String] = {
    val fileContent = parserResult.fileContent
    node.end match {
      case Some(endOffset) if node.code == Defines.EjsOutputTagPrefix && endOffset < fileContent.length =>
        fileContent.charAt(endOffset) match {
          case '=' => Some(Defines.EscapedOutputName)
          case '-' => Some(Defines.UnescapedOutputName)
          case _   => None
        }
      case _ => None
    }
  }

  private def nextAnonClassName(): String = s"<anon-class>${anonClassKeyPool.next}"

}
