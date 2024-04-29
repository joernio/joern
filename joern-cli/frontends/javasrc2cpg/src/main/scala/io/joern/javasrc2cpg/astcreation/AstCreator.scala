package io.joern.javasrc2cpg.astcreation

import com.github.javaparser.ast.expr.{
  AnnotationExpr,
  BooleanLiteralExpr,
  CharLiteralExpr,
  DoubleLiteralExpr,
  Expression,
  IntegerLiteralExpr,
  LongLiteralExpr,
  MethodCallExpr,
  NullLiteralExpr,
  StringLiteralExpr,
  TextBlockLiteralExpr
}
import com.github.javaparser.ast.nodeTypes.{NodeWithName, NodeWithSimpleName}
import com.github.javaparser.ast.{CompilationUnit, ImportDeclaration, Node, PackageDeclaration}
import com.github.javaparser.resolution.UnsolvedSymbolException
import com.github.javaparser.resolution.declarations.{
  ResolvedMethodDeclaration,
  ResolvedMethodLikeDeclaration,
  ResolvedReferenceTypeDeclaration
}
import com.github.javaparser.resolution.types.ResolvedType
import com.github.javaparser.resolution.types.parametrization.ResolvedTypeParametersMap
import com.github.javaparser.symbolsolver.JavaSymbolSolver
import io.joern.javasrc2cpg.astcreation.declarations.AstForDeclarationsCreator
import io.joern.javasrc2cpg.astcreation.expressions.AstForExpressionsCreator
import io.joern.javasrc2cpg.astcreation.statements.AstForStatementsCreator
import io.joern.javasrc2cpg.scope.Scope
import io.joern.javasrc2cpg.scope.Scope.*
import io.joern.javasrc2cpg.typesolvers.TypeInfoCalculator
import io.joern.javasrc2cpg.typesolvers.TypeInfoCalculator.TypeConstants
import io.joern.javasrc2cpg.util.BindingTable.createBindingTable
import io.joern.javasrc2cpg.util.MultiBindingTableAdapterForJavaparser.{
  InnerClassDeclaration,
  JavaparserBindingDeclType,
  RegularClassDeclaration
}
import io.joern.javasrc2cpg.util.{
  BindingTable,
  BindingTableAdapterForJavaparser,
  MultiBindingTableAdapterForJavaparser,
  NameConstants
}
import io.joern.x2cpg.datastructures.Global
import io.joern.x2cpg.utils.OffsetUtils
import io.joern.x2cpg.{Ast, AstCreatorBase, AstNodeBuilder, ValidationMode}
import io.shiftleft.codepropertygraph.generated.NodeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{NewClosureBinding, NewFile, NewImport, NewNamespaceBlock}
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate.DiffGraphBuilder

import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.RichOptional
import scala.language.{existentials, implicitConversions}
import scala.util.{Failure, Success, Try}

case class ClosureBindingEntry(node: ScopeVariable, binding: NewClosureBinding)

case class ExpectedType(fullName: Option[String], resolvedType: Option[ResolvedType] = None)
object ExpectedType {
  def empty: ExpectedType   = ExpectedType(None, None)
  val Int: ExpectedType     = ExpectedType(Some(TypeConstants.Int))
  val Boolean: ExpectedType = ExpectedType(Some(TypeConstants.Boolean))
  val Void: ExpectedType    = ExpectedType(Some(TypeConstants.Void))
}

case class AstWithStaticInit(ast: Seq[Ast], staticInits: Seq[Ast])

object AstWithStaticInit {
  val empty: AstWithStaticInit = AstWithStaticInit(Seq.empty, Seq.empty)

  def apply(ast: Ast): AstWithStaticInit = {
    AstWithStaticInit(Seq(ast), staticInits = Seq.empty)
  }
}

/** Translate a Java Parser AST into a CPG AST
  */
class AstCreator(
  val filename: String,
  javaParserAst: CompilationUnit,
  fileContent: Option[String],
  global: Global,
  val symbolSolver: JavaSymbolSolver,
  keepTypeArguments: Boolean
)(implicit val withSchemaValidation: ValidationMode)
    extends AstCreatorBase(filename)
    with AstNodeBuilder[Node, AstCreator]
    with AstForDeclarationsCreator
    with AstForExpressionsCreator
    with AstForStatementsCreator {

  private val logger = LoggerFactory.getLogger(this.getClass)

  private[astcreation] val scope = Scope()

  private[astcreation] val typeInfoCalc: TypeInfoCalculator =
    TypeInfoCalculator(global, symbolSolver, keepTypeArguments)
  private[astcreation] val bindingTableCache = mutable.HashMap.empty[String, BindingTable]

  /** Entry point of AST creation. Translates a compilation unit created by JavaParser into a DiffGraph containing the
    * corresponding CPG AST.
    */
  def createAst(): DiffGraphBuilder = {
    val fileNode = NewFile().name(filename).order(0)
    fileContent.foreach(fileNode.content(_))
    val ast = astForTranslationUnit(javaParserAst)
    storeInDiffGraph(ast)
    diffGraph.addNode(fileNode)
    diffGraph
  }

  /** Copy nodes/edges of given `AST` into the diff graph
    */
  def storeInDiffGraph(ast: Ast): Unit = {
    Ast.storeInDiffGraph(ast, diffGraph)
  }

  def toOptionList[T](items: collection.Seq[Option[T]]): Option[List[T]] = {
    items.foldLeft[Option[List[T]]](Some(Nil)) {
      case (Some(acc), Some(value)) => Some(acc :+ value)
      case _                        => None
    }
  }
  protected def line(node: Node): Option[Integer]      = node.getBegin.map(x => Integer.valueOf(x.line)).toScala
  protected def column(node: Node): Option[Integer]    = node.getBegin.map(x => Integer.valueOf(x.column)).toScala
  protected def lineEnd(node: Node): Option[Integer]   = node.getEnd.map(x => Integer.valueOf(x.line)).toScala
  protected def columnEnd(node: Node): Option[Integer] = node.getEnd.map(x => Integer.valueOf(x.column)).toScala
  protected def code(node: Node): String               = "" // TODO: javasrc2cpg uses custom code strings everywhere

  private val lineOffsetTable = OffsetUtils.getLineOffsetTable(fileContent)

  override protected def offset(node: Node): Option[(Int, Int)] = {
    Option
      .when(fileContent.isDefined) {
        for {
          lineNr      <- line(node)
          columnNr    <- column(node)
          lineEndNr   <- lineEnd(node)
          columnEndNr <- columnEnd(node)
        } yield OffsetUtils.coordinatesToOffset(
          lineOffsetTable,
          lineNr - 1,
          columnNr - 1,
          lineEndNr - 1,
          columnEndNr - 1
        )
      }
      .flatten
  }

  private def getImportCode(importDeclaration: ImportDeclaration): String = {
    importDeclaration.toString.trim.stripSuffix(";")
  }

  // TODO: Handle static imports correctly.
  private def addImportsToScope(compilationUnit: CompilationUnit): Seq[NewImport] = {
    val (asteriskImports, specificImports) = compilationUnit.getImports.asScala.toList.partition(_.isAsterisk)
    val specificImportNodes = specificImports.map { importStmt =>
      val name         = importStmt.getName.getIdentifier
      val typeFullName = importStmt.getNameAsString // fully qualified name
      typeInfoCalc.registerType(typeFullName)
      val importNode = NewImport()
        .importedAs(name)
        .importedEntity(typeFullName)
        .code(getImportCode(importStmt))

      if (!importStmt.isStatic) {
        scope.addTopLevelType(name, typeFullName)
      }
      importNode
    }

    val asteriskImportNodes = asteriskImports match {
      case imp :: Nil =>
        val name         = NameConstants.WildcardImportName
        val typeFullName = imp.getNameAsString
        val importNode = NewImport()
          .importedAs(name)
          .importedEntity(typeFullName)
          .isWildcard(true)
          .code(getImportCode(imp))
        scope.addWildcardImport(typeFullName)
        Seq(importNode)
      case _ => // Only try to guess a wildcard import if exactly one is defined
        Seq.empty
    }
    specificImportNodes ++ asteriskImportNodes
  }

  /** Translate compilation unit into AST
    */
  private def astForTranslationUnit(compilationUnit: CompilationUnit): Ast = {
    try {
      val namespaceBlock = namespaceBlockForPackageDecl(compilationUnit.getPackageDeclaration.toScala)

      scope.pushNamespaceScope(namespaceBlock)

      val importNodes = addImportsToScope(compilationUnit).map(Ast(_))

      val typeDeclAsts = compilationUnit.getTypes.asScala.map { typ =>
        astForTypeDeclaration(typ)
      }

      // TODO: Add ASTs
      scope.popNamespaceScope()
      Ast(namespaceBlock).withChildren(typeDeclAsts).withChildren(importNodes)
    } catch {
      case t: UnsolvedSymbolException =>
        logger.error(s"Unsolved symbol exception caught in $filename")
        Ast()
      case t: Throwable =>
        logger.error(s"Parsing file $filename failed", t)
        Ast()
    }
  }

  /** Translate package declaration into AST consisting of a corresponding namespace block.
    */
  private def namespaceBlockForPackageDecl(packageDecl: Option[PackageDeclaration]): NewNamespaceBlock = {
    packageDecl match {
      case Some(decl) =>
        val packageName = decl.getName.toString
        val fullName    = s"$filename:$packageName"
        NewNamespaceBlock()
          .name(packageName)
          .fullName(fullName)
          .filename(filename)
      case None =>
        globalNamespaceBlock()
    }
  }

  private[astcreation] def tryWithSafeStackOverflow[T](expr: => T): Try[T] = {
    try {
      Try(expr)
    } catch {
      // This is really, really ugly, but there's a bug in the JavaParser symbol solver that can lead to
      // unterminated recursion in some cases where types cannot be resolved.
      case e: StackOverflowError =>
        logger.debug(s"Caught StackOverflowError in $filename")
        Failure(e)
    }
  }

  private def fullNameForBindingTable(typeDecl: ResolvedReferenceTypeDeclaration): String = {
    typeInfoCalc.fullNameWithoutRegistering(typeDecl).getOrElse {
      val qualifiedName = typeDecl.getQualifiedName
      logger.warn(s"Could not get full name for resolved type decl $qualifiedName. THIS SHOULD NOT HAPPEN!")
      qualifiedName
    }
  }

  def getMultiBindingTable(typeDecl: JavaparserBindingDeclType): BindingTable = {
    val fullName = typeDecl match {
      case RegularClassDeclaration(resolvedRefType, _) => fullNameForBindingTable(resolvedRefType)

      case innerClassDeclaration: InnerClassDeclaration => innerClassDeclaration.fullName
    }
    bindingTableCache.getOrElseUpdate(
      fullName,
      createBindingTable(
        fullName,
        typeDecl,
        getMultiBindingTable,
        new MultiBindingTableAdapterForJavaparser(methodSignature)
      )
    )
  }

  def getBindingTable(typeDecl: ResolvedReferenceTypeDeclaration): BindingTable = {
    getMultiBindingTable(RegularClassDeclaration(typeDecl, ResolvedTypeParametersMap.empty()))
  }

  def expressionReturnTypeFullName(expr: Expression): Option[String] = {
    val resolvedTypeOption = tryWithSafeStackOverflow(expr.calculateResolvedType()) match {
      case Failure(ex) =>
        ex match {
          // If ast parser fails to resolve type, try resolving locally by using name
          // Precaution when resolving by name, we only want to resolve for case when the expr is solely a MethodCallExpr
          // and doesn't have a scope to it
          case symbolException: UnsolvedSymbolException =>
            expr match {
              case callExpr: MethodCallExpr =>
                callExpr.getScope.toScala match {
                  case Some(_: Expression) => None
                  case _                   => scope.lookupType(symbolException.getName, includeWildcards = false)
                }
              case _ => None
            }
          case _ => None
        }
      case Success(resolvedType) => typeInfoCalc.fullNameWithoutRegistering(resolvedType)
    }
    resolvedTypeOption.orElse(exprNameFromStack(expr))
  }

  private def exprNameFromStack(expr: Expression): Option[String] = expr match {
    case annotation: AnnotationExpr =>
      scope.lookupType(annotation.getNameAsString)
    case namedExpr: NodeWithName[_] =>
      scope.lookupVariableOrType(namedExpr.getNameAsString)
    case namedExpr: NodeWithSimpleName[_] =>
      scope.lookupVariableOrType(namedExpr.getNameAsString)
    // JavaParser doesn't handle literals well for some reason
    case _: BooleanLiteralExpr   => Some("boolean")
    case _: CharLiteralExpr      => Some("char")
    case _: DoubleLiteralExpr    => Some("double")
    case _: IntegerLiteralExpr   => Some("int")
    case _: LongLiteralExpr      => Some("long")
    case _: NullLiteralExpr      => Some("null")
    case _: StringLiteralExpr    => Some("java.lang.String")
    case _: TextBlockLiteralExpr => Some("java.lang.String")
    case _                       => None
  }

  def argumentTypesForMethodLike(maybeResolvedMethodLike: Try[ResolvedMethodLikeDeclaration]): Option[List[String]] = {
    maybeResolvedMethodLike.toOption
      .flatMap(calcParameterTypes(_, ResolvedTypeParametersMap.empty()))
  }

  def unknownAst(node: Node): Ast = Ast(unknownNode(node, node.toString))

}
