package io.joern.javasrc2cpg.astcreation

import com.github.javaparser.ast.body.VariableDeclarator
import com.github.javaparser.ast.expr.AssignExpr.Operator
import com.github.javaparser.ast.expr.{
  AnnotationExpr,
  ArrayAccessExpr,
  ArrayCreationExpr,
  ArrayInitializerExpr,
  AssignExpr,
  BinaryExpr,
  BooleanLiteralExpr,
  CastExpr,
  CharLiteralExpr,
  ClassExpr,
  ConditionalExpr,
  DoubleLiteralExpr,
  EnclosedExpr,
  Expression,
  FieldAccessExpr,
  InstanceOfExpr,
  IntegerLiteralExpr,
  LambdaExpr,
  LiteralExpr,
  LongLiteralExpr,
  MarkerAnnotationExpr,
  MethodCallExpr,
  NameExpr,
  NormalAnnotationExpr,
  NullLiteralExpr,
  ObjectCreationExpr,
  SingleMemberAnnotationExpr,
  StringLiteralExpr,
  SuperExpr,
  TextBlockLiteralExpr,
  ThisExpr,
  UnaryExpr,
  VariableDeclarationExpr
}
import com.github.javaparser.ast.nodeTypes.{NodeWithName, NodeWithSimpleName}
import com.github.javaparser.ast.{CompilationUnit, Node, NodeList, PackageDeclaration}
import com.github.javaparser.resolution.UnsolvedSymbolException
import com.github.javaparser.resolution.declarations.{
  ResolvedFieldDeclaration,
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
import io.joern.javasrc2cpg.util.Util.{composeMethodFullName, composeMethodLikeSignature, composeUnresolvedSignature}
import io.joern.javasrc2cpg.util.{BindingTable, BindingTableAdapterForJavaparser, NameConstants}
import io.joern.x2cpg.Defines.*
import io.joern.x2cpg.datastructures.Global
import io.joern.x2cpg.passes.frontend.TypeNodePass
import io.joern.x2cpg.utils.AstPropertiesUtil.*
import io.joern.x2cpg.utils.NodeBuilders
import io.joern.x2cpg.utils.NodeBuilders.{newAnnotationLiteralNode, newIdentifierNode, newOperatorCallNode}
import io.joern.x2cpg.{Ast, AstCreatorBase, AstNodeBuilder, Defines, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.AstNode.PropertyDefaults
import io.shiftleft.codepropertygraph.generated.nodes.{
  NewArrayInitializer,
  NewBlock,
  NewCall,
  NewClosureBinding,
  NewFieldIdentifier,
  NewIdentifier,
  NewImport,
  NewLiteral,
  NewLocal,
  NewMethodParameterIn,
  NewNamespaceBlock,
  NewNode,
  NewTypeRef
}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EdgeTypes, NodeTypes, Operators}
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate.DiffGraphBuilder

import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.RichOptional
import scala.language.{existentials, implicitConversions}
import scala.util.{Failure, Success, Try}

case class ClosureBindingEntry(node: ScopeVariable, binding: NewClosureBinding)

case class PartialConstructor(initNode: NewCall, initArgs: Seq[Ast], blockAst: Ast)

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
  global: Global,
  val symbolSolver: JavaSymbolSolver
)(implicit val withSchemaValidation: ValidationMode)
    extends AstCreatorBase(filename)
    with AstNodeBuilder[Node, AstCreator]
    with AstForDeclarationsCreator
    with AstForExpressionsCreator
    with AstForStatementsCreator {

  private val logger = LoggerFactory.getLogger(this.getClass)

  private[astcreation] val scope = Scope()

  private[astcreation] val typeInfoCalc: TypeInfoCalculator = TypeInfoCalculator(global, symbolSolver)
  private val partialConstructorQueue: mutable.ArrayBuffer[PartialConstructor] = mutable.ArrayBuffer.empty
  private[astcreation] val bindingTableCache = mutable.HashMap.empty[String, BindingTable]

  /** Entry point of AST creation. Translates a compilation unit created by JavaParser into a DiffGraph containing the
    * corresponding CPG AST.
    */
  def createAst(): DiffGraphBuilder = {
    val ast = astForTranslationUnit(javaParserAst)
    storeInDiffGraph(ast)
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
  protected def columnEnd(node: Node): Option[Integer] = node.getEnd.map(x => Integer.valueOf(x.line)).toScala

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

      if (importStmt.isStatic()) {
        scope.addStaticImport(importNode)
      } else {
        scope.addType(name, typeFullName)
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
        astForTypeDecl(typ, astParentType = NodeTypes.NAMESPACE_BLOCK, astParentFullName = namespaceBlock.fullName)
      }

      // TODO: Add ASTs
      scope.popScope()
      Ast(namespaceBlock).withChildren(typeDeclAsts).withChildren(importNodes)
    } catch {
      case t: UnsolvedSymbolException =>
        logger.error(s"Unsolved symbol exception caught in $filename")
        Ast()
      case t: Throwable =>
        logger.error(s"Parsing file $filename failed with $t")
        logger.error(s"Caused by ${t.getCause}")
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

  def getBindingTable(typeDecl: ResolvedReferenceTypeDeclaration): BindingTable = {
    val fullName = typeInfoCalc.fullName(typeDecl).getOrElse {
      val qualifiedName = typeDecl.getQualifiedName
      logger.warn(s"Could not get full name for resolved type decl $qualifiedName. THIS SHOULD NOT HAPPEN!")
      qualifiedName
    }
    bindingTableCache.getOrElseUpdate(
      fullName,
      createBindingTable(fullName, typeDecl, getBindingTable, new BindingTableAdapterForJavaparser(methodSignature))
    )
  }

  private def convertAnnotationValueExpr(expr: Expression): Option[Ast] = {
    expr match {
      case arrayInit: ArrayInitializerExpr =>
        val arrayInitNode = NewArrayInitializer()
          .code(arrayInit.toString)
        val initElementAsts = arrayInit.getValues.asScala.toList.map { value =>
          convertAnnotationValueExpr(value)
        }

        setArgumentIndices(initElementAsts.flatten)

        val returnAst = initElementAsts.foldLeft(Ast(arrayInitNode)) {
          case (ast, Some(elementAst)) =>
            ast.withChild(elementAst)
          case (ast, _) => ast
        }
        Some(returnAst)

      case annotationExpr: AnnotationExpr =>
        Some(astForAnnotationExpr(annotationExpr))

      case literalExpr: LiteralExpr =>
        Some(astForAnnotationLiteralExpr(literalExpr))

      case _: ClassExpr =>
        // TODO: Implement for known case
        None

      case _: FieldAccessExpr =>
        // TODO: Implement for known case
        None

      case _: BinaryExpr =>
        // TODO: Implement for known case
        None

      case _: NameExpr =>
        // TODO: Implement for known case
        None

      case _ =>
        logger.info(s"convertAnnotationValueExpr not yet implemented for unknown case ${expr.getClass}")
        None
    }
  }

  private def astForAnnotationLiteralExpr(literalExpr: LiteralExpr): Ast = {
    val valueNode =
      literalExpr match {
        case literal: StringLiteralExpr    => newAnnotationLiteralNode(literal.getValue)
        case literal: IntegerLiteralExpr   => newAnnotationLiteralNode(literal.getValue)
        case literal: BooleanLiteralExpr   => newAnnotationLiteralNode(java.lang.Boolean.toString(literal.getValue))
        case literal: CharLiteralExpr      => newAnnotationLiteralNode(literal.getValue)
        case literal: DoubleLiteralExpr    => newAnnotationLiteralNode(literal.getValue)
        case literal: LongLiteralExpr      => newAnnotationLiteralNode(literal.getValue)
        case _: NullLiteralExpr            => newAnnotationLiteralNode("null")
        case literal: TextBlockLiteralExpr => newAnnotationLiteralNode(literal.getValue)
      }

    Ast(valueNode)
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
                  case _                   => scope.lookupType(symbolException.getName)
                }
              case _ => None
            }
          case _ => None
        }
      case Success(resolvedType) => typeInfoCalc.fullName(resolvedType)
    }
    resolvedTypeOption.orElse(exprNameFromStack(expr))
  }

  def astForAnnotationExpr(annotationExpr: AnnotationExpr): Ast = {
    val fallbackType = s"${Defines.UnresolvedNamespace}.${annotationExpr.getNameAsString}"
    val fullName     = expressionReturnTypeFullName(annotationExpr).getOrElse(fallbackType)
    val code         = annotationExpr.toString
    val name         = annotationExpr.getName.getIdentifier
    val node         = annotationNode(annotationExpr, code, name, fullName)
    annotationExpr match {
      case _: MarkerAnnotationExpr =>
        annotationAst(node, List.empty)
      case normal: NormalAnnotationExpr =>
        val assignmentAsts = normal.getPairs.asScala.toList.map { pair =>
          annotationAssignmentAst(
            pair.getName.getIdentifier,
            pair.toString,
            convertAnnotationValueExpr(pair.getValue).getOrElse(Ast())
          )
        }
        annotationAst(node, assignmentAsts)
      case single: SingleMemberAnnotationExpr =>
        val assignmentAsts = List(
          annotationAssignmentAst(
            "value",
            single.getMemberValue.toString,
            convertAnnotationValueExpr(single.getMemberValue).getOrElse(Ast())
          )
        )
        annotationAst(node, assignmentAsts)
    }
  }

  private def astForUnaryExpr(expr: UnaryExpr, expectedType: ExpectedType): Ast = {
    val operatorName = expr.getOperator match {
      case UnaryExpr.Operator.LOGICAL_COMPLEMENT => Operators.logicalNot
      case UnaryExpr.Operator.POSTFIX_DECREMENT  => Operators.postDecrement
      case UnaryExpr.Operator.POSTFIX_INCREMENT  => Operators.postIncrement
      case UnaryExpr.Operator.PREFIX_DECREMENT   => Operators.preDecrement
      case UnaryExpr.Operator.PREFIX_INCREMENT   => Operators.preIncrement
      case UnaryExpr.Operator.BITWISE_COMPLEMENT => Operators.not
      case UnaryExpr.Operator.PLUS               => Operators.plus
      case UnaryExpr.Operator.MINUS              => Operators.minus
    }

    val argsAsts = astsForExpression(expr.getExpression, expectedType)

    val typeFullName =
      expressionReturnTypeFullName(expr)
        .orElse(argsAsts.headOption.flatMap(_.rootType))
        .orElse(expectedType.fullName)
        .getOrElse(TypeConstants.Any)

    val callNode = newOperatorCallNode(
      operatorName,
      code = expr.toString,
      typeFullName = Some(typeFullName),
      line = line(expr),
      column = column(expr)
    )

    callAst(callNode, argsAsts)
  }

  private def astForArrayAccessExpr(expr: ArrayAccessExpr, expectedType: ExpectedType): Ast = {
    val typeFullName =
      expressionReturnTypeFullName(expr)
        .orElse(expectedType.fullName)
        .getOrElse(TypeConstants.Any)
    val callNode = newOperatorCallNode(
      Operators.indexAccess,
      code = expr.toString,
      typeFullName = Some(typeFullName),
      line = line(expr),
      column = column(expr)
    )

    val arrayExpectedType = expectedType.copy(fullName = expectedType.fullName.map(_ ++ "[]"))
    val nameAst           = astsForExpression(expr.getName, arrayExpectedType)
    val indexAst          = astsForExpression(expr.getIndex, ExpectedType.Int)
    val args              = nameAst ++ indexAst
    callAst(callNode, args)
  }

  private def astForArrayCreationExpr(expr: ArrayCreationExpr, expectedType: ExpectedType): Ast = {
    val maybeInitializerAst = expr.getInitializer.toScala.map(astForArrayInitializerExpr(_, expectedType))

    maybeInitializerAst.flatMap(_.root) match {
      case Some(initializerRoot: NewCall) => initializerRoot.code(expr.toString)
      case _                              => // This should never happen
    }

    maybeInitializerAst.getOrElse {
      val typeFullName = expressionReturnTypeFullName(expr).orElse(expectedType.fullName).getOrElse(TypeConstants.Any)
      val callNode     = newOperatorCallNode(Operators.alloc, code = expr.toString, typeFullName = Some(typeFullName))
      val levelAsts = expr.getLevels.asScala.flatMap { lvl =>
        lvl.getDimension.toScala match {
          case Some(dimension) => astsForExpression(dimension, ExpectedType.Int)

          case None => Seq.empty
        }
      }.toSeq
      callAst(callNode, levelAsts)
    }
  }

  private def astForArrayInitializerExpr(expr: ArrayInitializerExpr, expectedType: ExpectedType): Ast = {
    val typeFullName =
      expressionReturnTypeFullName(expr)
        .orElse(expectedType.fullName)
        .getOrElse(TypeConstants.Any)
    val callNode = newOperatorCallNode(
      Operators.arrayInitializer,
      code = expr.toString,
      typeFullName = Some(typeFullName),
      line = line(expr),
      column = column(expr)
    )

    val MAX_INITIALIZERS = 1000

    val expectedValueType = expr.getValues.asScala.headOption.map { value =>
      // typeName and resolvedType may represent different types since typeName can fall
      // back to known information or primitive types. While this certainly isn't ideal,
      // it shouldn't cause issues since resolvedType is only used where the extra type
      // information not available in typeName is necessary.
      val typeName     = expressionReturnTypeFullName(value)
      val resolvedType = tryWithSafeStackOverflow(value.calculateResolvedType()).toOption
      ExpectedType(typeName, resolvedType)
    }
    val args = expr.getValues.asScala
      .slice(0, MAX_INITIALIZERS)
      .flatMap(astsForExpression(_, expectedValueType.getOrElse(ExpectedType.empty)))
      .toSeq

    val ast = callAst(callNode, args)

    if (expr.getValues.size() > MAX_INITIALIZERS) {
      val placeholder = NewLiteral()
        .typeFullName(TypeConstants.Any)
        .code("<too-many-initializers>")
        .lineNumber(line(expr))
        .columnNumber(column(expr))
      ast.withChild(Ast(placeholder)).withArgEdge(callNode, placeholder)
    } else {
      ast
    }
  }

  def astForBinaryExpr(expr: BinaryExpr, expectedType: ExpectedType): Ast = {
    val operatorName = expr.getOperator match {
      case BinaryExpr.Operator.OR                   => Operators.logicalOr
      case BinaryExpr.Operator.AND                  => Operators.logicalAnd
      case BinaryExpr.Operator.BINARY_OR            => Operators.or
      case BinaryExpr.Operator.BINARY_AND           => Operators.and
      case BinaryExpr.Operator.DIVIDE               => Operators.division
      case BinaryExpr.Operator.EQUALS               => Operators.equals
      case BinaryExpr.Operator.GREATER              => Operators.greaterThan
      case BinaryExpr.Operator.GREATER_EQUALS       => Operators.greaterEqualsThan
      case BinaryExpr.Operator.LESS                 => Operators.lessThan
      case BinaryExpr.Operator.LESS_EQUALS          => Operators.lessEqualsThan
      case BinaryExpr.Operator.LEFT_SHIFT           => Operators.shiftLeft
      case BinaryExpr.Operator.SIGNED_RIGHT_SHIFT   => Operators.logicalShiftRight
      case BinaryExpr.Operator.UNSIGNED_RIGHT_SHIFT => Operators.arithmeticShiftRight
      case BinaryExpr.Operator.XOR                  => Operators.xor
      case BinaryExpr.Operator.NOT_EQUALS           => Operators.notEquals
      case BinaryExpr.Operator.PLUS                 => Operators.addition
      case BinaryExpr.Operator.MINUS                => Operators.subtraction
      case BinaryExpr.Operator.MULTIPLY             => Operators.multiplication
      case BinaryExpr.Operator.REMAINDER            => Operators.modulo
    }

    val args =
      astsForExpression(expr.getLeft, expectedType) ++ astsForExpression(expr.getRight, expectedType)

    val typeFullName =
      expressionReturnTypeFullName(expr)
        .orElse(args.headOption.flatMap(_.rootType))
        .orElse(args.lastOption.flatMap(_.rootType))
        .orElse(expectedType.fullName)
        .getOrElse(TypeConstants.Any)

    val callNode = newOperatorCallNode(
      operatorName,
      code = expr.toString,
      typeFullName = Some(typeFullName),
      line = line(expr),
      column = column(expr)
    )

    callAst(callNode, args)
  }

  private def astForCastExpr(expr: CastExpr, expectedType: ExpectedType): Ast = {
    val typeFullName =
      typeInfoCalc
        .fullName(expr.getType)
        .orElse(expectedType.fullName)
        .getOrElse(TypeConstants.Any)

    val callNode = newOperatorCallNode(
      Operators.cast,
      code = expr.toString,
      typeFullName = Some(typeFullName),
      line = line(expr),
      column = column(expr)
    )

    val typeNode = NewTypeRef()
      .code(expr.getType.toString)
      .lineNumber(line(expr))
      .columnNumber(column(expr))
      .typeFullName(typeFullName)
    val typeAst = Ast(typeNode)

    val exprAst = astsForExpression(expr.getExpression, ExpectedType.empty)

    callAst(callNode, Seq(typeAst) ++ exprAst)
  }

  private def astsForAssignExpr(expr: AssignExpr, expectedExprType: ExpectedType): Seq[Ast] = {
    val operatorName = expr.getOperator match {
      case Operator.ASSIGN               => Operators.assignment
      case Operator.PLUS                 => Operators.assignmentPlus
      case Operator.MINUS                => Operators.assignmentMinus
      case Operator.MULTIPLY             => Operators.assignmentMultiplication
      case Operator.DIVIDE               => Operators.assignmentDivision
      case Operator.BINARY_AND           => Operators.assignmentAnd
      case Operator.BINARY_OR            => Operators.assignmentOr
      case Operator.XOR                  => Operators.assignmentXor
      case Operator.REMAINDER            => Operators.assignmentModulo
      case Operator.LEFT_SHIFT           => Operators.assignmentShiftLeft
      case Operator.SIGNED_RIGHT_SHIFT   => Operators.assignmentArithmeticShiftRight
      case Operator.UNSIGNED_RIGHT_SHIFT => Operators.assignmentLogicalShiftRight
    }

    val maybeResolvedType = Try(expr.getTarget.calculateResolvedType()).toOption
    val expectedType = maybeResolvedType
      .map { resolvedType =>
        ExpectedType(typeInfoCalc.fullName(resolvedType), Some(resolvedType))
      }
      .getOrElse(expectedExprType) // resolved target type should be more accurate
    val targetAst = astsForExpression(expr.getTarget, expectedType)
    val argsAsts  = astsForExpression(expr.getValue, expectedType)
    val valueType = argsAsts.headOption.flatMap(_.rootType)

    val typeFullName =
      targetAst.headOption
        .flatMap(_.rootType)
        .orElse(valueType)
        .orElse(expectedType.fullName)
        .getOrElse(TypeConstants.Any)

    val code = s"${targetAst.rootCodeOrEmpty} ${expr.getOperator.asString} ${argsAsts.rootCodeOrEmpty}"

    val callNode = newOperatorCallNode(operatorName, code, Some(typeFullName), line(expr), column(expr))

    if (partialConstructorQueue.isEmpty) {
      val assignAst = callAst(callNode, targetAst ++ argsAsts)
      Seq(assignAst)
    } else {
      if (partialConstructorQueue.size > 1) {
        logger.warn("BUG: Received multiple partial constructors from assignment. Dropping all but the first.")
      }
      val partialConstructor = partialConstructorQueue.head
      partialConstructorQueue.clear()

      targetAst.flatMap(_.root).toList match {
        case List(identifier: NewIdentifier) =>
          // In this case we have a simple assign. No block needed.
          // e.g. Foo f = new Foo();
          val initAst = completeInitForConstructor(partialConstructor, Ast(identifier.copy))
          Seq(callAst(callNode, targetAst ++ argsAsts), initAst)

        case _ =>
          // In this case the left hand side is more complex than an identifier, so
          // we need to contain the constructor in a block.
          // e.g. items[10] = new Foo();
          val valueAst = partialConstructor.blockAst
          Seq(callAst(callNode, targetAst ++ Seq(valueAst)))
      }
    }
  }

  private def localsForVarDecl(varDecl: VariableDeclarationExpr): List[NewLocal] = {
    varDecl.getVariables.asScala.map { variable =>
      val name = variable.getName.toString
      val typeFullName =
        tryWithSafeStackOverflow(typeInfoCalc.fullName(variable.getType)).toOption.flatten
          .orElse(scope.lookupType(variable.getTypeAsString))
          .getOrElse(TypeConstants.Any)
      val code = s"${variable.getType} $name"
      NewLocal()
        .name(name)
        .code(code)
        .typeFullName(typeFullName)
        .lineNumber(line(varDecl))
        .columnNumber(column(varDecl))
    }.toList
  }

  private def copyAstForVarDeclInit(targetAst: Ast): Ast = {
    targetAst.root match {
      case Some(identifier: NewIdentifier) => Ast(identifier.copy)

      case Some(fieldAccess: NewCall) if fieldAccess.name == Operators.fieldAccess =>
        val maybeIdentifier = targetAst.nodes.collectFirst { case node if node.isInstanceOf[NewIdentifier] => node }
        val maybeField = targetAst.nodes.collectFirst { case node if node.isInstanceOf[NewFieldIdentifier] => node }

        (maybeIdentifier, maybeField) match {
          case (Some(identifier), Some(fieldIdentifier)) =>
            val args = List(identifier, fieldIdentifier).map(node => Ast(node.copy))
            callAst(fieldAccess.copy, args)

          case _ =>
            logger.warn(s"Attempting to copy field access without required children: ${fieldAccess.code}")
            Ast()
        }

      case Some(root) =>
        logger.warn(s"Attempting to copy unhandled root type for var decl init: $root")
        Ast()

      case None =>
        Ast()
    }
  }

  def assignmentsForVarDecl(
    variables: Iterable[VariableDeclarator],
    lineNumber: Option[Integer],
    columnNumber: Option[Integer]
  ): Seq[Ast] = {
    val variablesWithInitializers =
      variables.filter(_.getInitializer.toScala.isDefined)
    val assignments = variablesWithInitializers.flatMap { variable =>
      val name                    = variable.getName.toString
      val initializer             = variable.getInitializer.toScala.get // Won't crash because of filter
      val initializerTypeFullName = variable.getInitializer.toScala.flatMap(expressionReturnTypeFullName)
      val javaParserVarType       = variable.getTypeAsString
      val variableTypeFullName =
        tryWithSafeStackOverflow(typeInfoCalc.fullName(variable.getType)).toOption.flatten
          // TODO: Surely the variable being declared can't already be in scope?
          .orElse(scope.lookupVariable(name).typeFullName)
          .orElse(scope.lookupType(javaParserVarType))

      val typeFullName =
        variableTypeFullName.orElse(initializerTypeFullName)

      // Need the actual resolvedType here for when the RHS is a lambda expression.
      val resolvedExpectedType =
        tryWithSafeStackOverflow(symbolSolver.toResolvedType(variable.getType, classOf[ResolvedType])).toOption
      val initializerAsts = astsForExpression(initializer, ExpectedType(typeFullName, resolvedExpectedType))

      val typeName = typeFullName
        .map(TypeNodePass.fullToShortName)
        .getOrElse(s"${Defines.UnresolvedNamespace}.${variable.getTypeAsString}")
      val code = s"$typeName $name = ${initializerAsts.rootCodeOrEmpty}"

      val callNode = newOperatorCallNode(Operators.assignment, code, typeFullName, lineNumber, columnNumber)

      val targetAst = scope.lookupVariable(name).getVariable() match {
        // TODO: This definitely feels like a bug. Why is the found member not being used for anything?
        case Some(ScopeMember(_, false)) =>
          val thisType = scope.enclosingTypeDeclFullName
          fieldAccessAst(NameConstants.This, thisType, name, typeFullName, line(variable), column(variable))

        case maybeCorrespNode =>
          val identifier = identifierNode(variable, name, name, typeFullName.getOrElse(TypeConstants.Any))
          Ast(identifier).withRefEdges(identifier, maybeCorrespNode.map(_.node).toList)
      }

      // Since all partial constructors will be dealt with here, don't pass them up.
      val declAst = callAst(callNode, Seq(targetAst) ++ initializerAsts)

      val constructorAsts = partialConstructorQueue.map(completeInitForConstructor(_, copyAstForVarDeclInit(targetAst)))
      partialConstructorQueue.clear()

      Seq(declAst) ++ constructorAsts
    }

    assignments.toList
  }

  private def completeInitForConstructor(partialConstructor: PartialConstructor, targetAst: Ast): Ast = {
    val initNode = partialConstructor.initNode
    val args     = partialConstructor.initArgs

    targetAst.root match {
      case Some(identifier: NewIdentifier) =>
        scope.lookupVariable(identifier.name).variableNode.foreach { variableNode =>
          diffGraph.addEdge(identifier, variableNode, EdgeTypes.REF)
        }

      case _ => // Nothing to do in this case
    }

    callAst(initNode, args.toList, Some(targetAst))
  }

  private def astsForVariableDecl(varDecl: VariableDeclarationExpr): Seq[Ast] = {
    val locals    = localsForVarDecl(varDecl)
    val localAsts = locals.map { Ast(_) }

    locals.foreach { local =>
      scope.addLocal(local)
    }

    val assignments =
      assignmentsForVarDecl(varDecl.getVariables.asScala, line(varDecl), column(varDecl))

    localAsts ++ assignments
  }

  private def astForClassExpr(expr: ClassExpr): Ast = {
    val someTypeFullName = Some(TypeConstants.Class)
    val callNode = newOperatorCallNode(Operators.fieldAccess, expr.toString, someTypeFullName, line(expr), column(expr))

    val identifierType = typeInfoCalc.fullName(expr.getType)
    val identifier = identifierNode(expr, expr.getTypeAsString, expr.getTypeAsString, identifierType.getOrElse("ANY"))
    val idAst      = Ast(identifier)

    val fieldIdentifier = NewFieldIdentifier()
      .canonicalName("class")
      .code("class")
      .lineNumber(line(expr))
      .columnNumber(column(expr))
    val fieldIdAst = Ast(fieldIdentifier)

    callAst(callNode, Seq(idAst, fieldIdAst))
  }

  private def astForConditionalExpr(expr: ConditionalExpr, expectedType: ExpectedType): Ast = {
    val condAst = astsForExpression(expr.getCondition, ExpectedType.Boolean)
    val thenAst = astsForExpression(expr.getThenExpr, expectedType)
    val elseAst = astsForExpression(expr.getElseExpr, expectedType)

    val typeFullName =
      expressionReturnTypeFullName(expr)
        .orElse(thenAst.headOption.flatMap(_.rootType))
        .orElse(elseAst.headOption.flatMap(_.rootType))
        .orElse(expectedType.fullName)
        .getOrElse(TypeConstants.Any)

    val callNode =
      newOperatorCallNode(Operators.conditional, expr.toString, Some(typeFullName), line(expr), column(expr))

    callAst(callNode, condAst ++ thenAst ++ elseAst)
  }

  private def astForEnclosedExpression(expr: EnclosedExpr, expectedType: ExpectedType): Seq[Ast] = {
    astsForExpression(expr.getInner, expectedType)
  }

  private def astForFieldAccessExpr(expr: FieldAccessExpr, expectedType: ExpectedType): Ast = {
    val typeFullName =
      expressionReturnTypeFullName(expr)
        .orElse(expectedType.fullName)
        .getOrElse(TypeConstants.Any)

    val callNode =
      newOperatorCallNode(Operators.fieldAccess, expr.toString, Some(typeFullName), line(expr), column(expr))

    val fieldIdentifier = expr.getName
    val identifierAsts  = astsForExpression(expr.getScope, ExpectedType.empty)
    val fieldIdentifierNode = NewFieldIdentifier()
      .canonicalName(fieldIdentifier.toString)
      .lineNumber(line(fieldIdentifier))
      .columnNumber(column(fieldIdentifier))
      .code(fieldIdentifier.toString)
    val fieldIdAst = Ast(fieldIdentifierNode)

    callAst(callNode, identifierAsts ++ Seq(fieldIdAst))
  }

  private def astForInstanceOfExpr(expr: InstanceOfExpr): Ast = {
    val booleanTypeFullName = Some(TypeConstants.Boolean)
    val callNode =
      newOperatorCallNode(Operators.instanceOf, expr.toString, booleanTypeFullName, line(expr), column(expr))

    val exprAst      = astsForExpression(expr.getExpression, ExpectedType.empty)
    val typeFullName = typeInfoCalc.fullName(expr.getType).getOrElse(TypeConstants.Any)
    val typeNode =
      NewTypeRef()
        .code(expr.getType.toString)
        .lineNumber(line(expr))
        .columnNumber(column(expr.getType))
        .typeFullName(typeFullName)
    val typeAst = Ast(typeNode)

    callAst(callNode, exprAst ++ Seq(typeAst))
  }

  private def fieldAccessAst(
    identifierName: String,
    identifierType: Option[String],
    fieldIdentifierName: String,
    returnType: Option[String],
    lineNo: Option[Integer],
    columnNo: Option[Integer]
  ): Ast = {
    val typeFullName     = identifierType.orElse(Some(TypeConstants.Any)).map(typeInfoCalc.registerType)
    val identifier       = newIdentifierNode(identifierName, typeFullName.getOrElse("ANY"))
    val maybeCorrespNode = scope.lookupVariable(identifierName).variableNode

    val fieldIdentifier = NewFieldIdentifier()
      .code(fieldIdentifierName)
      .canonicalName(fieldIdentifierName)
      .lineNumber(lineNo)
      .columnNumber(columnNo)

    val fieldAccessCode = s"$identifierName.$fieldIdentifierName"
    val fieldAccess =
      newOperatorCallNode(
        Operators.fieldAccess,
        fieldAccessCode,
        returnType.orElse(Some(TypeConstants.Any)),
        lineNo,
        columnNo
      )

    val identifierAst = Ast(identifier)
    val fieldIdentAst = Ast(fieldIdentifier)

    callAst(fieldAccess, Seq(identifierAst, fieldIdentAst))
      .withRefEdges(identifier, maybeCorrespNode.toList)
  }

  private def astForNameExpr(nameExpr: NameExpr, expectedType: ExpectedType): Ast = {
    val name = nameExpr.getName.toString
    val typeFullName = expressionReturnTypeFullName(nameExpr)
      .orElse(expectedType.fullName)
      .map(typeInfoCalc.registerType)

    tryWithSafeStackOverflow(nameExpr.resolve()) match {
      case Success(value) if value.isField =>
        val identifierName = if (value.asField.isStatic) {
          // A static field represented by a NameExpr must belong to the class in which it's used. Static fields
          // from other classes are represented by a FieldAccessExpr instead.
          scope.enclosingTypeDecl.map(_.name).getOrElse(s"${Defines.UnresolvedNamespace}.$name")
        } else {
          NameConstants.This
        }

        val identifierTypeFullName =
          value match {
            case fieldDecl: ResolvedFieldDeclaration =>
              // TODO It is not quite correct to use the declaring classes type.
              // Instead we should take the using classes type which is either the same or a
              // sub class of the declaring class.
              typeInfoCalc.fullName(fieldDecl.declaringType())
          }

        fieldAccessAst(identifierName, identifierTypeFullName, name, typeFullName, line(nameExpr), column(nameExpr))

      case _ =>
        val identifier = identifierNode(nameExpr, name, name, typeFullName.getOrElse(TypeConstants.Any))

        val variableOption = scope
          .lookupVariable(name)
          .variableNode
          .collect {
            case parameter: NewMethodParameterIn => parameter

            case local: NewLocal => local
          }

        variableOption.foldLeft(Ast(identifier))((ast, variableNode) => ast.withRefEdge(identifier, variableNode))
    }

  }

  def initNode(
    namespaceName: Option[String],
    argumentTypes: Option[List[String]],
    argsSize: Int,
    code: String,
    lineNumber: Option[Integer] = None,
    columnNumber: Option[Integer] = None
  ): NewCall = {
    val initSignature = argumentTypes match {
      case Some(tpe)          => composeMethodLikeSignature(TypeConstants.Void, tpe)
      case _ if argsSize == 0 => composeMethodLikeSignature(TypeConstants.Void, Nil)
      case _                  => composeUnresolvedSignature(argsSize)
    }
    val namespace          = namespaceName.getOrElse(Defines.UnresolvedNamespace)
    val initMethodFullName = composeMethodFullName(namespace, Defines.ConstructorMethodName, initSignature)
    NewCall()
      .name(Defines.ConstructorMethodName)
      .methodFullName(initMethodFullName)
      .signature(initSignature)
      .typeFullName(TypeConstants.Void)
      .code(code)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .lineNumber(lineNumber)
      .columnNumber(columnNumber)
  }

  /** The below representation for constructor invocations and object creations was chosen for the sake of consistency
    * with the Java frontend. It follows the bytecode approach of splitting a constructor call into separate `alloc` and
    * `init` calls.
    *
    * There are two cases to consider. The first is a constructor invocation in an assignment, for example:
    *
    * Foo f = new Foo(42);
    *
    * is represented as
    *
    * Foo f = <operator>.alloc() f.init(42);
    *
    * The second case is a constructor invocation not in an assignment, for example as an argument to a method call. In
    * this case, the representation does not stay as close to Java as in case
    *   1. In particular, a new BLOCK is introduced to contain the constructor invocation. For example:
    *
    * foo(new Foo(42));
    *
    * is represented as
    *
    * foo({ Foo temp = alloc(); temp.init(42); temp })
    *
    * This is not valid Java code, but this representation is a decent compromise between staying faithful to Java and
    * being consistent with the Java bytecode frontend.
    */
  private def astForObjectCreationExpr(expr: ObjectCreationExpr, expectedType: ExpectedType): Ast = {
    val maybeResolvedExpr = tryWithSafeStackOverflow(expr.resolve())
    val argumentAsts      = argAstsForCall(expr, maybeResolvedExpr, expr.getArguments)

    val typeFullName = tryWithSafeStackOverflow(typeInfoCalc.fullName(expr.getType)).toOption.flatten
      .orElse(scope.lookupType(expr.getTypeAsString))
      .orElse(expectedType.fullName)

    val argumentTypes = argumentTypesForMethodLike(maybeResolvedExpr)

    val allocNode = newOperatorCallNode(
      Operators.alloc,
      expr.toString,
      typeFullName.orElse(Some(TypeConstants.Any)),
      line(expr),
      column(expr)
    )

    val initCall = initNode(
      typeFullName.orElse(Some(TypeConstants.Any)),
      argumentTypes,
      argumentAsts.size,
      expr.toString,
      line(expr)
    )

    // Assume that a block ast is required, since there isn't enough information to decide otherwise.
    // This simplifies logic elsewhere, and unnecessary blocks will be garbage collected soon.
    val blockAst = blockAstForConstructorInvocation(line(expr), column(expr), allocNode, initCall, argumentAsts)

    expr.getParentNode.toScala match {
      case Some(parent) if parent.isInstanceOf[VariableDeclarator] || parent.isInstanceOf[AssignExpr] =>
        val partialConstructor = PartialConstructor(initCall, argumentAsts, blockAst)
        partialConstructorQueue.append(partialConstructor)
        Ast(allocNode)

      case _ =>
        blockAst
    }
  }

  private var tempConstCount = 0
  private def blockAstForConstructorInvocation(
    lineNumber: Option[Integer],
    columnNumber: Option[Integer],
    allocNode: NewCall,
    initNode: NewCall,
    args: Seq[Ast]
  ): Ast = {
    val blockNode = NewBlock()
      .lineNumber(lineNumber)
      .columnNumber(columnNumber)
      .typeFullName(allocNode.typeFullName)

    val tempName = "$obj" ++ tempConstCount.toString
    tempConstCount += 1
    val identifier    = newIdentifierNode(tempName, allocNode.typeFullName)
    val identifierAst = Ast(identifier)

    val allocAst = Ast(allocNode)

    val assignmentNode = newOperatorCallNode(Operators.assignment, PropertyDefaults.Code, Some(allocNode.typeFullName))

    val assignmentAst = callAst(assignmentNode, List(identifierAst, allocAst))

    val identifierWithDefaultOrder = identifier.copy.order(PropertyDefaults.Order)
    val identifierForInit          = identifierWithDefaultOrder.copy
    val initWithDefaultOrder       = initNode.order(PropertyDefaults.Order)
    val initAst                    = callAst(initWithDefaultOrder, args, Some(Ast(identifierForInit)))

    val returnAst = Ast(identifierWithDefaultOrder.copy)

    Ast(blockNode)
      .withChild(assignmentAst)
      .withChild(initAst)
      .withChild(returnAst)
  }

  def argumentTypesForMethodLike(maybeResolvedMethodLike: Try[ResolvedMethodLikeDeclaration]): Option[List[String]] = {
    maybeResolvedMethodLike.toOption
      .flatMap(calcParameterTypes(_, ResolvedTypeParametersMap.empty()))
  }

  private def astForThisExpr(expr: ThisExpr, expectedType: ExpectedType): Ast = {
    val typeFullName =
      expressionReturnTypeFullName(expr)
        .orElse(expectedType.fullName)

    val identifier = identifierNode(expr, expr.toString, expr.toString, typeFullName.getOrElse("ANY"))
    val thisParam  = scope.lookupVariable(NameConstants.This).variableNode

    thisParam.foreach { thisNode =>
      diffGraph.addEdge(identifier, thisNode, EdgeTypes.REF)
    }

    Ast(identifier)
  }

  def astsForExpression(expression: Expression, expectedType: ExpectedType): Seq[Ast] = {
    // TODO: Implement missing handlers
    // case _: MethodReferenceExpr     => Seq()
    // case _: PatternExpr             => Seq()
    // case _: SuperExpr               => Seq()
    // case _: SwitchExpr              => Seq()
    // case _: TypeExpr                => Seq()
    expression match {
      case _: AnnotationExpr          => Seq()
      case x: ArrayAccessExpr         => Seq(astForArrayAccessExpr(x, expectedType))
      case x: ArrayCreationExpr       => Seq(astForArrayCreationExpr(x, expectedType))
      case x: ArrayInitializerExpr    => Seq(astForArrayInitializerExpr(x, expectedType))
      case x: AssignExpr              => astsForAssignExpr(x, expectedType)
      case x: BinaryExpr              => Seq(astForBinaryExpr(x, expectedType))
      case x: CastExpr                => Seq(astForCastExpr(x, expectedType))
      case x: ClassExpr               => Seq(astForClassExpr(x))
      case x: ConditionalExpr         => Seq(astForConditionalExpr(x, expectedType))
      case x: EnclosedExpr            => astForEnclosedExpression(x, expectedType)
      case x: FieldAccessExpr         => Seq(astForFieldAccessExpr(x, expectedType))
      case x: InstanceOfExpr          => Seq(astForInstanceOfExpr(x))
      case x: LambdaExpr              => Seq(astForLambdaExpr(x, expectedType))
      case x: LiteralExpr             => Seq(astForLiteralExpr(x))
      case x: MethodCallExpr          => Seq(astForMethodCall(x, expectedType))
      case x: NameExpr                => Seq(astForNameExpr(x, expectedType))
      case x: ObjectCreationExpr      => Seq(astForObjectCreationExpr(x, expectedType))
      case x: SuperExpr               => Seq(astForSuperExpr(x, expectedType))
      case x: ThisExpr                => Seq(astForThisExpr(x, expectedType))
      case x: UnaryExpr               => Seq(astForUnaryExpr(x, expectedType))
      case x: VariableDeclarationExpr => astsForVariableDecl(x)
      case x                          => Seq(unknownAst(x))
    }
  }

  def unknownAst(node: Node): Ast = Ast(unknownNode(node, node.toString))

  private def someWithDotSuffix(prefix: String): Option[String] = Some(s"$prefix.")

  private def codeForScopeExpr(scopeExpr: Expression, isScopeForStaticCall: Boolean): Option[String] = {
    scopeExpr match {
      case scope: NameExpr => someWithDotSuffix(scope.getNameAsString)

      case fieldAccess: FieldAccessExpr =>
        val maybeScopeString = codeForScopeExpr(fieldAccess.getScope, isScopeForStaticCall = false)
        val name             = fieldAccess.getNameAsString
        maybeScopeString
          .map { scopeString =>
            s"$scopeString$name"
          }
          .orElse(Some(name))
          .flatMap(someWithDotSuffix)

      case _: SuperExpr => someWithDotSuffix(NameConstants.Super)

      case _: ThisExpr => someWithDotSuffix(NameConstants.This)

      case scopeMethodCall: MethodCallExpr =>
        codePrefixForMethodCall(scopeMethodCall) match {
          case "" => Some("")
          case prefix =>
            val argumentsCode = getArgumentCodeString(scopeMethodCall.getArguments)
            someWithDotSuffix(s"$prefix${scopeMethodCall.getNameAsString}($argumentsCode)")
        }

      case objectCreationExpr: ObjectCreationExpr =>
        val typeName        = objectCreationExpr.getTypeAsString
        val argumentsString = getArgumentCodeString(objectCreationExpr.getArguments)
        someWithDotSuffix(s"new $typeName($argumentsString)")

      case _ => None
    }
  }

  private def codePrefixForMethodCall(call: MethodCallExpr): String = {
    tryWithSafeStackOverflow(call.resolve()) match {
      case Success(resolvedCall) =>
        call.getScope.toScala
          .flatMap(codeForScopeExpr(_, resolvedCall.isStatic))
          .getOrElse(if (resolvedCall.isStatic) "" else s"${NameConstants.This}.")

      case _ =>
        // If the call is unresolvable, we cannot make a good guess about what the prefix should be
        ""
    }
  }

  private def createObjectNode(
    typeFullName: Option[String],
    call: MethodCallExpr,
    dispatchType: String
  ): Option[NewIdentifier] = {
    val maybeScope = call.getScope.toScala

    Option.when(maybeScope.isDefined || dispatchType == DispatchTypes.DYNAMIC_DISPATCH) {
      val name = maybeScope.map(_.toString).getOrElse(NameConstants.This)
      identifierNode(call, name, name, typeFullName.getOrElse("ANY"))
    }
  }

  private def astForLiteralExpr(expr: LiteralExpr): Ast = {
    val typeFullName = expressionReturnTypeFullName(expr).getOrElse(TypeConstants.Any)
    val literalNode =
      NewLiteral()
        .code(expr.toString)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
        .typeFullName(typeFullName)
    Ast(literalNode)
  }

  private def getExpectedParamType(maybeResolvedCall: Try[ResolvedMethodLikeDeclaration], idx: Int): ExpectedType = {
    maybeResolvedCall.toOption
      .map { methodDecl =>
        val paramCount = methodDecl.getNumberOfParams

        val resolvedType = if (idx < paramCount) {
          Some(methodDecl.getParam(idx).getType)
        } else if (paramCount > 0 && methodDecl.getParam(paramCount - 1).isVariadic) {
          Some(methodDecl.getParam(paramCount - 1).getType)
        } else {
          None
        }

        val typeName = resolvedType.flatMap(typeInfoCalc.fullName)
        ExpectedType(typeName, resolvedType)
      }
      .getOrElse(ExpectedType.empty)
  }

  private def dispatchTypeForCall(maybeDecl: Try[ResolvedMethodDeclaration], maybeScope: Option[Expression]): String = {
    maybeScope match {
      case Some(_: SuperExpr) =>
        DispatchTypes.STATIC_DISPATCH
      case _ =>
        maybeDecl match {
          case Success(decl) =>
            if (decl.isStatic) DispatchTypes.STATIC_DISPATCH else DispatchTypes.DYNAMIC_DISPATCH

          case _ =>
            DispatchTypes.DYNAMIC_DISPATCH
        }
    }
  }

  private def targetTypeForCall(callExpr: MethodCallExpr): Option[String] = {
    val maybeType = callExpr.getScope.toScala match {
      case Some(callScope: ThisExpr) =>
        expressionReturnTypeFullName(callScope)
          .orElse(scope.enclosingTypeDeclFullName)

      case Some(callScope: SuperExpr) =>
        expressionReturnTypeFullName(callScope)
          .orElse(scope.enclosingTypeDecl.flatMap(_.inheritsFromTypeFullName.headOption))

      case Some(scope) => expressionReturnTypeFullName(scope)

      case None =>
        tryWithSafeStackOverflow(callExpr.resolve()).toOption
          .flatMap { methodDeclOption =>
            if (methodDeclOption.isStatic) typeInfoCalc.fullName(methodDeclOption.declaringType())
            else scope.enclosingTypeDeclFullName
          }
          .orElse(scope.enclosingTypeDeclFullName)
    }

    maybeType.map(typeInfoCalc.registerType)
  }

  def argAstsForCall(
    call: Node,
    tryResolvedDecl: Try[ResolvedMethodLikeDeclaration],
    args: NodeList[Expression]
  ): Seq[Ast] = {
    val hasVariadicParameter = tryResolvedDecl.map(_.hasVariadicParameter).getOrElse(false)
    val paramCount           = tryResolvedDecl.map(_.getNumberOfParams).getOrElse(-1)

    val argsAsts = args.asScala.zipWithIndex.flatMap { case (arg, idx) =>
      val expectedType = getExpectedParamType(tryResolvedDecl, idx)
      astsForExpression(arg, expectedType)
    }.toList

    tryResolvedDecl match {
      case Success(_) if hasVariadicParameter =>
        val expectedVariadicTypeFullName = getExpectedParamType(tryResolvedDecl, paramCount - 1).fullName
        val (regularArgs, varargs)       = argsAsts.splitAt(paramCount - 1)
        val arrayInitializer = newOperatorCallNode(
          Operators.arrayInitializer,
          Operators.arrayInitializer,
          expectedVariadicTypeFullName,
          line(call),
          column(call)
        )

        val arrayInitializerAst = callAst(arrayInitializer, varargs)

        regularArgs ++ Seq(arrayInitializerAst)

      case _ => argsAsts
    }
  }

  private def getArgumentCodeString(args: NodeList[Expression]): String = {
    args.asScala
      .map {
        case _: LambdaExpr => "<lambda>"
        case other         => other.toString
      }
      .mkString(", ")
  }

  private def astForMethodCall(call: MethodCallExpr, expectedReturnType: ExpectedType): Ast = {
    val maybeResolvedCall = tryWithSafeStackOverflow(call.resolve())
    val argumentAsts      = argAstsForCall(call, maybeResolvedCall, call.getArguments)

    val expressionTypeFullName = expressionReturnTypeFullName(call).orElse(expectedReturnType.fullName)

    val argumentTypes = argumentTypesForMethodLike(maybeResolvedCall)
    val returnType = maybeResolvedCall
      .map { resolvedCall =>
        typeInfoCalc.fullName(resolvedCall.getReturnType, ResolvedTypeParametersMap.empty())
      }
      .toOption
      .flatten
      .orElse(expressionTypeFullName)

    val dispatchType = dispatchTypeForCall(maybeResolvedCall, call.getScope.toScala)

    val receiverTypeOption = targetTypeForCall(call)
    val scopeAsts = call.getScope.toScala match {
      case Some(scope) => astsForExpression(scope, ExpectedType(receiverTypeOption))

      case None =>
        val objectNode =
          createObjectNode(receiverTypeOption, call, dispatchType)
        for {
          obj       <- objectNode
          thisParam <- scope.lookupVariable(NameConstants.This).variableNode
        } diffGraph.addEdge(obj, thisParam, EdgeTypes.REF)
        objectNode.map(Ast(_)).toList
    }

    val receiverType = receiverTypeOption.orElse(scopeAsts.rootType).filter(_ != TypeConstants.Any)

    val argumentsCode = getArgumentCodeString(call.getArguments)
    val codePrefix    = codePrefixForMethodCall(call)
    val callCode      = s"$codePrefix${call.getNameAsString}($argumentsCode)"

    val callName       = call.getNameAsString
    val namespace      = receiverType.getOrElse(Defines.UnresolvedNamespace)
    val signature      = composeSignature(returnType, argumentTypes, argumentAsts.size)
    val methodFullName = composeMethodFullName(namespace, callName, signature)
    val callRoot = NewCall()
      .name(callName)
      .methodFullName(methodFullName)
      .signature(signature)
      .code(callCode)
      .dispatchType(dispatchType)
      .lineNumber(line(call))
      .columnNumber(column(call))
      .typeFullName(expressionTypeFullName.getOrElse(TypeConstants.Any))

    callAst(callRoot, argumentAsts, scopeAsts.headOption)
  }

  private def astForSuperExpr(superExpr: SuperExpr, expectedType: ExpectedType): Ast = {
    val typeFullName =
      expressionReturnTypeFullName(superExpr)
        .orElse(expectedType.fullName)
        .getOrElse(TypeConstants.Any)

    typeInfoCalc.registerType(typeFullName)

    val identifier = identifierNode(superExpr, NameConstants.This, NameConstants.Super, typeFullName)
    Ast(identifier)
  }

}
