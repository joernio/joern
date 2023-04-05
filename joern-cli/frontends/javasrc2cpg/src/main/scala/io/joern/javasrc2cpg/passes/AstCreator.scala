package io.joern.javasrc2cpg.passes

import com.github.javaparser.ast.`type`.TypeParameter
import com.github.javaparser.ast.{CompilationUnit, Node, NodeList, PackageDeclaration}
import com.github.javaparser.ast.body.{
  AnnotationDeclaration,
  BodyDeclaration,
  CallableDeclaration,
  ClassOrInterfaceDeclaration,
  ConstructorDeclaration,
  EnumConstantDeclaration,
  FieldDeclaration,
  InitializerDeclaration,
  MethodDeclaration,
  Parameter,
  TypeDeclaration,
  VariableDeclarator
}
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
import com.github.javaparser.ast.stmt.{
  AssertStmt,
  BlockStmt,
  BreakStmt,
  CatchClause,
  ContinueStmt,
  DoStmt,
  EmptyStmt,
  ExplicitConstructorInvocationStmt,
  ExpressionStmt,
  ForEachStmt,
  ForStmt,
  IfStmt,
  LabeledStmt,
  ReturnStmt,
  Statement,
  SwitchEntry,
  SwitchStmt,
  SynchronizedStmt,
  ThrowStmt,
  TryStmt,
  WhileStmt
}
import com.github.javaparser.resolution.UnsolvedSymbolException
import com.github.javaparser.resolution.declarations.{
  ResolvedFieldDeclaration,
  ResolvedMethodDeclaration,
  ResolvedMethodLikeDeclaration,
  ResolvedReferenceTypeDeclaration,
  ResolvedTypeParameterDeclaration
}
import com.github.javaparser.resolution.types.parametrization.ResolvedTypeParametersMap
import com.github.javaparser.resolution.types.{ResolvedReferenceType, ResolvedType, ResolvedTypeVariable}
import com.github.javaparser.symbolsolver.JavaSymbolSolver
import io.joern.javasrc2cpg.typesolvers.TypeInfoCalculator
import io.joern.javasrc2cpg.util.BindingTable.createBindingTable
import io.joern.x2cpg.utils.NodeBuilders.{
  annotationLiteralNode,
  callNode,
  fieldIdentifierNode,
  identifierNode,
  methodReturnNode,
  modifierNode,
  operatorCallNode
}
import io.joern.javasrc2cpg.util.Scope.ScopeTypes.{BlockScope, MethodScope, NamespaceScope, TypeDeclScope}
import io.joern.javasrc2cpg.util.Scope.WildcardImportName
import io.joern.javasrc2cpg.util.{
  BindingTable,
  BindingTableAdapterForJavaparser,
  BindingTableAdapterForLambdas,
  BindingTableEntry,
  LambdaBindingInfo,
  NameConstants,
  NodeTypeInfo,
  Scope
}
import io.joern.javasrc2cpg.typesolvers.TypeInfoCalculator.{ObjectMethodSignatures, TypeConstants}
import io.joern.javasrc2cpg.util.Util.{
  composeMethodFullName,
  composeMethodLikeSignature,
  composeUnresolvedSignature,
  memberNode
}
import io.shiftleft.codepropertygraph.generated.{
  ControlStructureTypes,
  DispatchTypes,
  EdgeTypes,
  EvaluationStrategies,
  ModifierTypes,
  NodeTypes,
  Operators
}
import io.shiftleft.codepropertygraph.generated.nodes.{
  NewAnnotation,
  NewArrayInitializer,
  NewBinding,
  NewBlock,
  NewCall,
  NewClosureBinding,
  NewControlStructure,
  NewFieldIdentifier,
  NewIdentifier,
  NewJumpTarget,
  NewLiteral,
  NewLocal,
  NewMember,
  NewMethod,
  NewMethodParameterIn,
  NewMethodRef,
  NewMethodReturn,
  NewModifier,
  NewNamespaceBlock,
  NewNode,
  NewReturn,
  NewTypeDecl,
  NewTypeRef,
  NewUnknown
}
import io.joern.x2cpg.{Ast, AstCreatorBase, Defines}
import io.joern.x2cpg.datastructures.Global
import io.joern.x2cpg.passes.frontend.TypeNodePass
import io.joern.x2cpg.utils.AstPropertiesUtil._
import io.joern.x2cpg.utils.NodeBuilders
import io.shiftleft.codepropertygraph.generated.nodes.AstNode.PropertyDefaults
import io.shiftleft.codepropertygraph.generated.nodes.MethodParameterIn.{PropertyDefaults => ParameterDefaults}
import io.shiftleft.passes.IntervalKeyPool
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate.DiffGraphBuilder

import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters.RichOptional
import scala.language.{existentials, implicitConversions}
import scala.util.{Failure, Success, Try}

case class ClosureBindingEntry(nodeTypeInfo: NodeTypeInfo, binding: NewClosureBinding)

case class LambdaImplementedInfo(
  implementedInterface: Option[ResolvedReferenceType],
  implementedMethod: Option[ResolvedMethodDeclaration]
)

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
class AstCreator(filename: String, javaParserAst: CompilationUnit, global: Global, symbolSolver: JavaSymbolSolver)
    extends AstCreatorBase(filename) {

  import io.joern.javasrc2cpg.passes.AstCreator._

  private val logger = LoggerFactory.getLogger(this.getClass)

  private val scopeStack                       = Scope()
  private val typeInfoCalc: TypeInfoCalculator = TypeInfoCalculator(global, symbolSolver)
  private val partialConstructorQueue: mutable.ArrayBuffer[PartialConstructor] = mutable.ArrayBuffer.empty
  private val bindingTableCache = mutable.HashMap.empty[String, BindingTable]

  // TODO: Perhaps move this to a NameProvider or some such? Look at kt2cpg to see if some unified representation
  // makes sense.
  private val LambdaNamePrefix   = "lambda$"
  private val lambdaKeyPool      = new IntervalKeyPool(first = 0, last = Long.MaxValue)
  private val IndexNamePrefix    = "$idx"
  private val indexKeyPool       = new IntervalKeyPool(first = 0, last = Long.MaxValue)
  private val IterableNamePrefix = "$iterLocal"
  private val iterableKeyPool    = new IntervalKeyPool(first = 0, last = Long.MaxValue)

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

  private def addImportsToScope(compilationUnit: CompilationUnit): Unit = {
    val (asteriskImports, specificImports) = compilationUnit.getImports.asScala.toList.partition(_.isAsterisk)
    specificImports.foreach { importStmt =>
      val name         = importStmt.getName.getIdentifier
      val typeFullName = importStmt.getNameAsString // fully qualified name
      typeInfoCalc.registerType(typeFullName)
      val importNode = identifierNode(name, Some(typeFullName))
      scopeStack.addToScope(importNode, name, Some(typeFullName))
    }

    asteriskImports match {
      case imp :: Nil =>
        val name         = WildcardImportName
        val typeFullName = Some(imp.getNameAsString)
        val importNode   = identifierNode(name, typeFullName)
        scopeStack.addToScope(importNode, name, typeFullName)

      case _ => // Only try to guess a wildcard import if exactly one is defined
    }
  }

  /** Translate compilation unit into AST
    */
  private def astForTranslationUnit(compilationUnit: CompilationUnit): Ast = {

    try {
      val ast = astForPackageDeclaration(compilationUnit.getPackageDeclaration.toScala)

      scopeStack.pushNewScope(NamespaceScope)

      val namespaceBlockFullName = {
        ast.root.collect { case x: NewNamespaceBlock => x.fullName }.getOrElse("none")
      }

      addImportsToScope(compilationUnit)

      val typeDeclAsts = compilationUnit.getTypes.asScala.map { typ =>
        astForTypeDecl(typ, astParentType = NodeTypes.NAMESPACE_BLOCK, astParentFullName = namespaceBlockFullName)
      }

      val lambdaTypeDeclAsts = scopeStack.getLambdaDeclsInScope.toSeq

      scopeStack.popScope()
      ast.withChildren(typeDeclAsts).withChildren(lambdaTypeDeclAsts)
    } catch {
      case t: UnsolvedSymbolException =>
        logger.error(s"Unsolved symbol exception caught in $filename")
        throw t
      case t: Throwable =>
        logger.error(s"Parsing file $filename failed with $t")
        logger.error(s"Caused by ${t.getCause}")
        throw t
    }
  }

  /** Translate package declaration into AST consisting of a corresponding namespace block.
    */
  private def astForPackageDeclaration(packageDecl: Option[PackageDeclaration]): Ast = {

    val namespaceBlock = packageDecl match {
      case Some(decl) =>
        val packageName = decl.getName.toString
        val fullName    = s"$filename:$packageName"
        NewNamespaceBlock()
          .name(packageName)
          .fullName(fullName)
      case None =>
        globalNamespaceBlock()
    }
    Ast(namespaceBlock.filename(absolutePath(filename)))
  }

  private def tryWithSafeStackOverflow[T](expr: => T): Try[T] = {
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

  private def composeSignature(
    maybeReturnType: Option[String],
    maybeParameterTypes: Option[List[String]],
    parameterCount: Int
  ): String = {
    (maybeReturnType, maybeParameterTypes) match {
      case (Some(returnType), Some(parameterTypes)) =>
        composeMethodLikeSignature(returnType, parameterTypes)

      case _ =>
        composeUnresolvedSignature(parameterCount)
    }
  }

  private def methodSignature(method: ResolvedMethodDeclaration, typeParamValues: ResolvedTypeParametersMap): String = {
    val maybeParameterTypes = calcParameterTypes(method, typeParamValues)

    val maybeReturnType =
      Try(method.getReturnType).toOption
        .flatMap(returnType => typeInfoCalc.fullName(returnType, typeParamValues))

    composeSignature(maybeReturnType, maybeParameterTypes, method.getNumberOfParams)
  }

  private def toOptionList[T](items: collection.Seq[Option[T]]): Option[List[T]] = {
    items.foldLeft[Option[List[T]]](Some(Nil)) {
      case (Some(acc), Some(value)) => Some(acc :+ value)
      case _                        => None
    }
  }

  private def calcParameterTypes(
    methodLike: ResolvedMethodLikeDeclaration,
    typeParamValues: ResolvedTypeParametersMap
  ): Option[List[String]] = {
    val parameterTypes =
      Range(0, methodLike.getNumberOfParams)
        .flatMap { index =>
          Try(methodLike.getParam(index)).toOption
        }
        .map { param =>
          Try(param.getType).toOption
            .flatMap(paramType => typeInfoCalc.fullName(paramType, typeParamValues))
        }

    toOptionList(parameterTypes)
  }

  def getBindingTable(typeDecl: ResolvedReferenceTypeDeclaration): BindingTable = {
    val fullName = typeInfoCalc.fullName(typeDecl).getOrElse {
      val qualifiedName = typeDecl.getQualifiedName
      logger.warn(s"Could not get full name for resolved type decl $qualifiedName. THIS SHOULD NOT HAPPEN!")
      qualifiedName
    }
    bindingTableCache.getOrElseUpdate(
      fullName,
      createBindingTable[ResolvedReferenceTypeDeclaration](
        fullName,
        typeDecl,
        getBindingTable,
        methodSignature,
        new BindingTableAdapterForJavaparser(methodSignature),
        (parentDecl, thisDecl) => thisDecl.getQualifiedName == parentDecl.getQualifiedName
      )
    )
  }

  private def getLambdaBindingTable(lambdaBindingInfo: LambdaBindingInfo): BindingTable = {
    val fullName = lambdaBindingInfo.fullName

    bindingTableCache.getOrElseUpdate(
      fullName,
      createBindingTable[LambdaBindingInfo](
        fullName,
        lambdaBindingInfo,
        getBindingTable,
        methodSignature,
        new BindingTableAdapterForLambdas(),
        (parentDecl, thisInfo) => parentDecl.getQualifiedName == thisInfo.fullName
      )
    )
  }

  private def createBindingNodes(typeDeclNode: NewTypeDecl, bindingTable: BindingTable): Unit = {
    // We only sort to get stable output.
    val sortedEntries =
      bindingTable.getEntries.toBuffer.sortBy((entry: BindingTableEntry) => s"${entry.name}${entry.signature}")

    sortedEntries.foreach { entry =>
      val bindingNode = NewBinding()
        .name(entry.name)
        .signature(entry.signature)
        .methodFullName(entry.implementingMethodFullName)

      diffGraph.addNode(bindingNode)
      diffGraph.addEdge(typeDeclNode, bindingNode, EdgeTypes.BINDS)
    }
  }

  private def astForTypeDeclMember(member: BodyDeclaration[_], astParentFullName: String): AstWithStaticInit = {
    member match {
      case constructor: ConstructorDeclaration =>
        val ast = astForConstructor(constructor)

        AstWithStaticInit(ast)

      case method: MethodDeclaration =>
        val ast = astForMethod(method)

        AstWithStaticInit(ast)

      case typeDeclaration: TypeDeclaration[_] =>
        AstWithStaticInit(astForTypeDecl(typeDeclaration, NodeTypes.TYPE_DECL, astParentFullName))

      case fieldDeclaration: FieldDeclaration =>
        val memberAsts = fieldDeclaration.getVariables.asScala.toList.map { variable =>
          astForFieldVariable(variable, fieldDeclaration)
        }

        val assignments = assignmentsForVarDecl(
          fieldDeclaration.getVariables.asScala.toList,
          line(fieldDeclaration),
          column(fieldDeclaration)
        )

        val staticInitAsts = if (fieldDeclaration.isStatic) assignments else Nil
        if (!fieldDeclaration.isStatic) scopeStack.addMemberInitializersToScope(assignments)

        AstWithStaticInit(memberAsts, staticInitAsts)

      case initDeclaration: InitializerDeclaration =>
        val stmts = initDeclaration.getBody.getStatements
        val asts  = stmts.asScala.flatMap(astsForStatement).toList
        AstWithStaticInit(ast = Seq.empty, staticInits = asts)

      case unhandled =>
        // AnnotationMemberDeclarations and InitializerDeclarations as children of typeDecls are the
        // expected cases.
        logger.info(s"Found unhandled typeDecl member ${unhandled.getClass} in file $filename")
        AstWithStaticInit.empty
    }
  }

  private def identifierForTypeParameter(typeParameter: TypeParameter): NewIdentifier = {
    val name = typeParameter.getNameAsString
    val typeFullName = typeParameter.getTypeBound.asScala.headOption
      .flatMap(typeInfoCalc.fullName)
      .getOrElse(TypeConstants.Object)
    typeInfoCalc.registerType(typeFullName)

    identifierNode(name, Some(typeFullName))
  }

  private def identifierForResolvedTypeParameter(typeParameter: ResolvedTypeParameterDeclaration): NewIdentifier = {
    val name = typeParameter.getName
    val typeFullName = Try(typeParameter.getUpperBound).toOption
      .flatMap(typeInfoCalc.fullName)
      .getOrElse(TypeConstants.Object)
    typeInfoCalc.registerType(typeFullName)
    identifierNode(name, Some(typeFullName))
  }

  private def clinitAstFromStaticInits(staticInits: Seq[Ast]): Option[Ast] = {
    Option.when(staticInits.nonEmpty) {
      val signature = composeMethodLikeSignature(TypeConstants.Void, Nil)
      val fullName = scopeStack.getEnclosingTypeDecl
        .map(typeDecl =>
          composeMethodFullName(typeDecl.fullName, io.joern.x2cpg.Defines.StaticInitMethodName, signature)
        )
        .getOrElse("")
      staticInitMethodAst(staticInits.toList, fullName, Some(signature), TypeConstants.Void)
    }
  }

  private def codeForTypeDecl(typ: TypeDeclaration[_], isInterface: Boolean): String = {
    val codeBuilder = new mutable.StringBuilder()
    if (typ.isPublic) {
      codeBuilder.append("public ")
    } else if (typ.isPrivate) {
      codeBuilder.append("private ")
    } else if (typ.isProtected) {
      codeBuilder.append("protected ")
    }

    if (typ.isStatic) {
      codeBuilder.append("static ")
    }

    val classPrefix =
      if (isInterface)
        "interface "
      else if (typ.isEnumDeclaration)
        "enum "
      else
        "class "
    codeBuilder.append(classPrefix)
    codeBuilder.append(typ.getNameAsString)

    codeBuilder.toString()
  }

  private def modifiersForTypeDecl(typ: TypeDeclaration[_], isInterface: Boolean): List[NewModifier] = {
    val accessModifierType = if (typ.isPublic) {
      Some(ModifierTypes.PUBLIC)
    } else if (typ.isPrivate) {
      Some(ModifierTypes.PRIVATE)
    } else if (typ.isProtected) {
      Some(ModifierTypes.PROTECTED)
    } else {
      None
    }
    val accessModifier = accessModifierType.map(modifierNode)

    val abstractModifier =
      Option.when(isInterface || typ.getMethods.asScala.exists(_.isAbstract))(modifierNode(ModifierTypes.ABSTRACT))

    List(accessModifier, abstractModifier).flatten
  }

  private def createTypeDeclNode(
    typ: TypeDeclaration[_],
    astParentType: String,
    astParentFullName: String,
    isInterface: Boolean
  ): NewTypeDecl = {
    val baseTypeFullNames = if (typ.isClassOrInterfaceDeclaration) {
      val decl             = typ.asClassOrInterfaceDeclaration()
      val extendedTypes    = decl.getExtendedTypes.asScala
      val implementedTypes = decl.getImplementedTypes.asScala
      val inheritsFromTypeNames =
        (extendedTypes ++ implementedTypes).flatMap { typ =>
          typeInfoCalc.fullName(typ).orElse(scopeStack.lookupVariableType(typ.getNameAsString))
        }
      val maybeJavaObjectType = if (extendedTypes.isEmpty) {
        typeInfoCalc.registerType(TypeConstants.Object)
        Seq(TypeConstants.Object)
      } else {
        Seq()
      }
      maybeJavaObjectType ++ inheritsFromTypeNames
    } else {
      List.empty[String]
    }

    val resolvedType    = tryWithSafeStackOverflow(typ.resolve()).toOption
    val defaultFullName = s"${Defines.UnresolvedNamespace}.${typ.getNameAsString}"
    val name            = resolvedType.flatMap(typeInfoCalc.name).getOrElse(typ.getNameAsString)
    val typeFullName    = resolvedType.flatMap(typeInfoCalc.fullName).getOrElse(defaultFullName)

    val code = codeForTypeDecl(typ, isInterface)

    NewTypeDecl()
      .name(name)
      .fullName(typeFullName)
      .lineNumber(line(typ))
      .columnNumber(column(typ))
      .inheritsFromTypeFullName(baseTypeFullNames)
      .filename(filename)
      .code(code)
      .astParentType(astParentType)
      .astParentFullName(astParentFullName)
  }

  private def addTypeDeclTypeParamsToScope(typ: TypeDeclaration[_]): Unit = {
    tryWithSafeStackOverflow(typ.resolve()).map(_.getTypeParameters.asScala) match {
      case Success(resolvedTypeParams) =>
        resolvedTypeParams
          .map(identifierForResolvedTypeParameter)
          .foreach { identifier =>
            scopeStack.addToScope(identifier, identifier.name, Some(identifier.typeFullName))
          }

      case _ => // Nothing to do here
    }
  }

  private def astForTypeDecl(typ: TypeDeclaration[_], astParentType: String, astParentFullName: String): Ast = {
    val isInterface = typ match {
      case classDeclaration: ClassOrInterfaceDeclaration => classDeclaration.isInterface
      case _                                             => false
    }

    val typeDeclNode = createTypeDeclNode(typ, astParentType, astParentFullName, isInterface)

    scopeStack.pushNewScope(TypeDeclScope(typeDeclNode))
    addTypeDeclTypeParamsToScope(typ)

    val enumEntryAsts = if (typ.isEnumDeclaration) {
      typ.asEnumDeclaration().getEntries.asScala.map(astForEnumEntry).toList
    } else {
      List.empty
    }

    val staticInits: mutable.Buffer[Ast] = mutable.Buffer()
    val memberAsts = typ.getMembers.asScala.flatMap { member =>
      val astWithInits =
        astForTypeDeclMember(member, astParentFullName = NodeTypes.TYPE_DECL)
      staticInits.appendAll(astWithInits.staticInits)
      astWithInits.ast
    }

    val defaultConstructorAst = if (!isInterface && typ.getConstructors.isEmpty) {
      Some(astForDefaultConstructor())
    } else {
      None
    }

    val annotationAsts = typ.getAnnotations.asScala.map(astForAnnotationExpr)

    val clinitAst = clinitAstFromStaticInits(staticInits.toSeq)

    val lambdaMethods = scopeStack.getLambdaMethodsInScope.toSeq

    val modifiers = modifiersForTypeDecl(typ, isInterface)

    val typeDeclAst = Ast(typeDeclNode)
      .withChildren(enumEntryAsts)
      .withChildren(memberAsts)
      .withChildren(defaultConstructorAst.toList)
      .withChildren(annotationAsts)
      .withChildren(clinitAst.toSeq)
      .withChildren(lambdaMethods)
      .withChildren(modifiers.map(Ast(_)))

    val defaultConstructorBindingEntry =
      defaultConstructorAst
        .flatMap(_.root)
        .collect { case defaultConstructor: NewMethod =>
          BindingTableEntry(
            io.joern.x2cpg.Defines.ConstructorMethodName,
            defaultConstructor.signature,
            defaultConstructor.fullName
          )
        }

    // Annotation declarations need no binding table as objects of this
    // typ never get called from user code.
    // Furthermore the parser library throws an exception when trying to
    // access e.g. the declared methods of an annotation declaration.
    if (!typ.isInstanceOf[AnnotationDeclaration]) {
      tryWithSafeStackOverflow(typ.resolve()).toOption.foreach { resolvedTypeDecl =>
        val bindingTable = getBindingTable(resolvedTypeDecl)
        defaultConstructorBindingEntry.foreach(bindingTable.add)
        createBindingNodes(typeDeclNode, bindingTable)
      }
    }

    scopeStack.popScope()

    typeDeclAst
  }

  private def astForDefaultConstructor(): Ast = {
    val typeFullName = scopeStack.getEnclosingTypeDecl.map(_.fullName)
    val signature    = s"${TypeConstants.Void}()"
    val fullName = composeMethodFullName(
      typeFullName.getOrElse(Defines.UnresolvedNamespace),
      Defines.ConstructorMethodName,
      signature
    )
    val constructorNode = NewMethod()
      .name(io.joern.x2cpg.Defines.ConstructorMethodName)
      .fullName(fullName)
      .signature(signature)
      .filename(filename)
      .isExternal(false)

    val thisAst = Ast(thisNodeForMethod(typeFullName, lineNumber = None))
    val bodyAst = Ast(NewBlock()).withChildren(scopeStack.getMemberInitializers)

    val returnNode = methodReturnNode(TypeConstants.Void, line = None, column = None)

    val modifiers = List(modifierNode(ModifierTypes.CONSTRUCTOR), modifierNode(ModifierTypes.PUBLIC))

    methodAstWithAnnotations(constructorNode, Seq(thisAst), bodyAst, returnNode, modifiers)
  }

  private def astForEnumEntry(entry: EnumConstantDeclaration): Ast = {
    // TODO Fix enum entries in general
    val typeFullName =
      tryWithSafeStackOverflow(entry.resolve().getType).toOption.flatMap(typeInfoCalc.fullName)

    val entryNode = memberNode(entry.getNameAsString, entry.toString, typeFullName, line(entry), column(entry))

    val name = s"${typeFullName.getOrElse(Defines.UnresolvedNamespace)}.${Defines.ConstructorMethodName}"
    val args = entry.getArguments.asScala.map { argument =>
      val children = astsForExpression(argument, ExpectedType.empty)
      val callNode =
        NewCall()
          .name(name)
          .methodFullName(name)
          .dispatchType(DispatchTypes.STATIC_DISPATCH)
          .code(entry.toString)
          .lineNumber(line(entry))
          .columnNumber(column(entry))
      callAst(callNode, children)
    }

    Ast(entryNode).withChildren(args)
  }

  private def modifiersForFieldDeclaration(decl: FieldDeclaration): Seq[Ast] = {
    val staticModifier =
      Option.when(decl.isStatic)(modifierNode(ModifierTypes.STATIC))

    val accessModifierType =
      if (decl.isPublic)
        Some(ModifierTypes.PUBLIC)
      else if (decl.isPrivate)
        Some(ModifierTypes.PRIVATE)
      else if (decl.isProtected)
        Some(ModifierTypes.PROTECTED)
      else
        None

    val accessModifier = accessModifierType.map(modifierNode)

    List(staticModifier, accessModifier).flatten.map(Ast(_))
  }

  private def astForFieldVariable(v: VariableDeclarator, fieldDeclaration: FieldDeclaration): Ast = {
    // TODO: Should be able to find expected type here
    val annotations = fieldDeclaration.getAnnotations
    val typeFullName =
      typeInfoCalc
        .fullName(v.getType)
        .orElse(scopeStack.lookupVariableType(v.getTypeAsString, wildcardFallback = true))
        .getOrElse(s"${Defines.UnresolvedNamespace}.${v.getTypeAsString}")
    val name           = v.getName.toString
    val node           = memberNode(name, s"$typeFullName $name", Some(typeFullName), line(v), column(v))
    val memberAst      = Ast(node)
    val annotationAsts = annotations.asScala.map(astForAnnotationExpr)

    val fieldDeclModifiers = modifiersForFieldDeclaration(fieldDeclaration)

    val nodeTypeInfo = NodeTypeInfo(
      node,
      name = name,
      typeFullName = Some(typeFullName),
      isField = true,
      isStatic = fieldDeclaration.isStatic
    )
    scopeStack.addToScope(name, nodeTypeInfo)

    memberAst
      .withChildren(annotationAsts)
      .withChildren(fieldDeclModifiers)
  }

  private def astForConstructor(constructorDeclaration: ConstructorDeclaration): Ast = {
    scopeStack.pushNewScope(MethodScope(ExpectedType.Void))
    val maybeResolved = tryWithSafeStackOverflow(constructorDeclaration.resolve())

    val parameterAsts = astsForParameterList(constructorDeclaration.getParameters).toList
    val paramTypes    = argumentTypesForMethodLike(maybeResolved)
    val signature     = composeSignature(Some(TypeConstants.Void), paramTypes, parameterAsts.size)
    val typeFullName  = scopeStack.getEnclosingTypeDecl.map(_.fullName)
    val fullName =
      composeMethodFullName(
        typeFullName.getOrElse(Defines.UnresolvedNamespace),
        Defines.ConstructorMethodName,
        signature
      )

    val constructorNode = createPartialMethod(constructorDeclaration)
      .name(io.joern.x2cpg.Defines.ConstructorMethodName)
      .fullName(fullName)
      .signature(signature)

    parameterAsts.foreach { ast =>
      ast.root match {
        case Some(p: NewMethodParameterIn) => scopeStack.addToScope(p, p.name, Some(p.typeFullName))
        case _                             => // This should never happen
      }
    }

    val thisNode = thisNodeForMethod(typeFullName, line(constructorDeclaration))
    scopeStack.addToScope(thisNode, thisNode.name, typeFullName)
    val thisAst = Ast(thisNode)

    val bodyAst      = astForConstructorBody(Some(constructorDeclaration.getBody))
    val methodReturn = constructorReturnNode(constructorDeclaration)

    val annotationAsts = constructorDeclaration.getAnnotations.asScala.map(astForAnnotationExpr).toList

    val modifiers =
      NewModifier().modifierType(ModifierTypes.CONSTRUCTOR) :: modifiersForMethod(constructorDeclaration).filterNot(
        _.modifierType == ModifierTypes.VIRTUAL
      )

    scopeStack.popScope()

    methodAstWithAnnotations(
      constructorNode,
      thisAst :: parameterAsts,
      bodyAst,
      methodReturn,
      modifiers,
      annotationAsts
    )
  }

  private def thisNodeForMethod(
    maybeTypeFullName: Option[String],
    lineNumber: Option[Integer]
  ): NewMethodParameterIn = {
    val typeFullName = typeInfoCalc.registerType(maybeTypeFullName.getOrElse(TypeConstants.Any))
    NodeBuilders.thisParameterNode(typeFullName, maybeTypeFullName.toSeq, lineNumber)
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
        case literal: StringLiteralExpr    => annotationLiteralNode(literal.getValue)
        case literal: IntegerLiteralExpr   => annotationLiteralNode(literal.getValue)
        case literal: BooleanLiteralExpr   => annotationLiteralNode(java.lang.Boolean.toString(literal.getValue))
        case literal: CharLiteralExpr      => annotationLiteralNode(literal.getValue)
        case literal: DoubleLiteralExpr    => annotationLiteralNode(literal.getValue)
        case literal: LongLiteralExpr      => annotationLiteralNode(literal.getValue)
        case _: NullLiteralExpr            => annotationLiteralNode("null")
        case literal: TextBlockLiteralExpr => annotationLiteralNode(literal.getValue)
      }

    Ast(valueNode)
  }

  private def exprNameFromStack(expr: Expression): Option[String] = expr match {
    case annotation: AnnotationExpr =>
      scopeStack.lookupVariableType(annotation.getNameAsString, wildcardFallback = true)
    case namedExpr: NodeWithName[_] =>
      scopeStack.lookupVariableType(namedExpr.getNameAsString)
    case namedExpr: NodeWithSimpleName[_] =>
      scopeStack.lookupVariableType(namedExpr.getNameAsString)
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

  private def expressionReturnTypeFullName(expr: Expression): Option[String] = {
    tryWithSafeStackOverflow(expr.calculateResolvedType()).toOption
      .flatMap(typeInfoCalc.fullName)
      .orElse(exprNameFromStack(expr))
  }

  private def createAnnotationNode(annotationExpr: AnnotationExpr): NewAnnotation = {
    val fallbackType = s"${Defines.UnresolvedNamespace}.${annotationExpr.getNameAsString}"
    NewAnnotation()
      .code(annotationExpr.toString)
      .name(annotationExpr.getName.getIdentifier)
      .fullName(expressionReturnTypeFullName(annotationExpr).getOrElse(fallbackType))
      .lineNumber(line(annotationExpr))
      .columnNumber(column(annotationExpr))
  }

  private def astForAnnotationExpr(annotationExpr: AnnotationExpr): Ast = {
    annotationExpr match {
      case _: MarkerAnnotationExpr =>
        annotationAst(createAnnotationNode(annotationExpr), List.empty)
      case normal: NormalAnnotationExpr =>
        val annotationNode = createAnnotationNode(annotationExpr)
        val assignmentAsts = normal.getPairs.asScala.toList.map { pair =>
          annotationAssignmentAst(
            pair.getName.getIdentifier,
            pair.toString,
            convertAnnotationValueExpr(pair.getValue).getOrElse(Ast())
          )
        }
        annotationAst(annotationNode, assignmentAsts)
      case single: SingleMemberAnnotationExpr =>
        val annotationNode = createAnnotationNode(annotationExpr)
        val assignmentAsts = List(
          annotationAssignmentAst(
            "value",
            single.getMemberValue.toString,
            convertAnnotationValueExpr(single.getMemberValue).getOrElse(Ast())
          )
        )
        annotationAst(annotationNode, assignmentAsts)
    }
  }

  private def abstractModifierForCallable(
    callableDeclaration: CallableDeclaration[_],
    isInterfaceMethod: Boolean
  ): Option[NewModifier] = {
    callableDeclaration match {
      case methodDeclaration: MethodDeclaration =>
        Option.when(methodDeclaration.isAbstract || (isInterfaceMethod && !methodDeclaration.isDefault)) {
          modifierNode(ModifierTypes.ABSTRACT)
        }

      case _ => None
    }
  }

  private def modifiersForMethod(methodDeclaration: CallableDeclaration[_]): List[NewModifier] = {
    val isInterfaceMethod = scopeStack.getEnclosingTypeDecl.exists(_.code.contains("interface "))

    val abstractModifier = abstractModifierForCallable(methodDeclaration, isInterfaceMethod)

    val staticVirtualModifierType = if (methodDeclaration.isStatic) ModifierTypes.STATIC else ModifierTypes.VIRTUAL
    val staticVirtualModifier     = Some(modifierNode(staticVirtualModifierType))

    val accessModifierType = if (methodDeclaration.isPublic) {
      Some(ModifierTypes.PUBLIC)
    } else if (methodDeclaration.isPrivate) {
      Some(ModifierTypes.PRIVATE)
    } else if (methodDeclaration.isProtected) {
      Some(ModifierTypes.PROTECTED)
    } else if (isInterfaceMethod) {
      // TODO: more robust interface check
      Some(ModifierTypes.PUBLIC)
    } else {
      None
    }
    val accessModifier = accessModifierType.map(modifierNode)

    List(accessModifier, abstractModifier, staticVirtualModifier).flatten
  }

  private def addTypeParametersToScope(methodDeclaration: MethodDeclaration): Unit = {
    methodDeclaration.getTypeParameters.asScala.foreach { typeParam =>
      val identifier = identifierForTypeParameter(typeParam)
      scopeStack.addToScope(identifier, identifier.name, Some(identifier.typeFullName))
    }
  }

  private def astForMethod(methodDeclaration: MethodDeclaration): Ast = {

    val maybeResolved      = tryWithSafeStackOverflow(methodDeclaration.resolve())
    val expectedReturnType = Try(symbolSolver.toResolvedType(methodDeclaration.getType, classOf[ResolvedType])).toOption
    val returnTypeFullName = expectedReturnType
      .flatMap(typeInfoCalc.fullName)
      .orElse(scopeStack.lookupVariableType(methodDeclaration.getTypeAsString, wildcardFallback = true))

    scopeStack.pushNewScope(MethodScope(ExpectedType(returnTypeFullName, expectedReturnType)))

    addTypeParametersToScope(methodDeclaration)

    val parameterAsts  = astsForParameterList(methodDeclaration.getParameters)
    val parameterTypes = argumentTypesForMethodLike(maybeResolved)
    val signature      = composeSignature(returnTypeFullName, parameterTypes, parameterAsts.size)
    val namespaceName  = scopeStack.getEnclosingTypeDecl.map(_.fullName).getOrElse(Defines.UnresolvedNamespace)
    val methodFullName = composeMethodFullName(namespaceName, methodDeclaration.getNameAsString, signature)

    val methodNode = createPartialMethod(methodDeclaration)
      .fullName(methodFullName)
      .signature(signature)

    val thisNode = Option.when(!methodDeclaration.isStatic) {
      val typeFullName = scopeStack.getEnclosingTypeDecl.map(_.fullName)
      thisNodeForMethod(typeFullName, line(methodDeclaration))
    }
    val thisAst = thisNode.map(Ast(_)).toList

    thisNode.foreach { node =>
      scopeStack.addToScope(node, node.name, Some(node.typeFullName))
    }

    val bodyAst = methodDeclaration.getBody.toScala.map(astForBlockStatement(_)).getOrElse(Ast(NewBlock()))
    val methodReturn = methodReturnNode(
      returnTypeFullName.getOrElse(TypeConstants.Any),
      None,
      line(methodDeclaration.getType),
      column(methodDeclaration.getType)
    )

    val annotationAsts = methodDeclaration.getAnnotations.asScala.map(astForAnnotationExpr).toSeq

    val modifiers = modifiersForMethod(methodDeclaration)

    scopeStack.popScope()

    methodAstWithAnnotations(methodNode, thisAst ++ parameterAsts, bodyAst, methodReturn, modifiers, annotationAsts)
  }

  private def constructorReturnNode(constructorDeclaration: ConstructorDeclaration): NewMethodReturn = {
    val line   = constructorDeclaration.getEnd.map(x => Integer.valueOf(x.line)).toScala
    val column = constructorDeclaration.getEnd.map(x => Integer.valueOf(x.column)).toScala
    methodReturnNode(TypeConstants.Void, None, line, column)
  }

  /** Constructor and Method declarations share a lot of fields, so this method adds the fields they have in common.
    * `fullName` and `signature` are omitted
    */
  private def createPartialMethod(declaration: CallableDeclaration[_]): NewMethod = {
    val code         = declaration.getDeclarationAsString.trim
    val columnNumber = declaration.getBegin.map(x => Integer.valueOf(x.column)).toScala
    val endLine      = declaration.getEnd.map(x => Integer.valueOf(x.line)).toScala
    val endColumn    = declaration.getEnd.map(x => Integer.valueOf(x.column)).toScala

    val methodNode = NewMethod()
      .name(declaration.getNameAsString)
      .code(code)
      .isExternal(false)
      .filename(filename)
      .lineNumber(line(declaration))
      .columnNumber(columnNumber)
      .lineNumberEnd(endLine)
      .columnNumberEnd(endColumn)

    methodNode
  }

  private def astForConstructorBody(body: Option[BlockStmt]): Ast = {
    val containsThisInvocation =
      body
        .flatMap(_.getStatements.asScala.headOption)
        .collect { case e: ExplicitConstructorInvocationStmt => e }
        .exists(_.isThis)

    val memberInitializers =
      if (containsThisInvocation)
        Seq.empty
      else
        scopeStack.getMemberInitializers

    body match {
      case Some(b) => astForBlockStatement(b, prefixAsts = memberInitializers)

      case None => Ast(NewBlock()).withChildren(memberInitializers)
    }
  }

  private def astsForLabeledStatement(stmt: LabeledStmt): Seq[Ast] = {
    val jumpTargetAst = Ast(NewJumpTarget().name(stmt.getLabel.toString))
    val stmtAst       = astsForStatement(stmt.getStatement).toList

    jumpTargetAst :: stmtAst
  }

  private def astForThrow(stmt: ThrowStmt): Ast = {
    val throwNode = NewCall()
      .name("<operator>.throw")
      .methodFullName("<operator>.throw")
      .lineNumber(line(stmt))
      .columnNumber(column(stmt))
      .code(stmt.toString())
      .dispatchType(DispatchTypes.STATIC_DISPATCH)

    val args = astsForExpression(stmt.getExpression, ExpectedType.empty)

    callAst(throwNode, args)
  }

  private def astForCatchClause(catchClause: CatchClause): Ast = {
    astForBlockStatement(catchClause.getBody)
  }

  private def astsForTry(stmt: TryStmt): Seq[Ast] = {
    val tryNode = NewControlStructure()
      .controlStructureType(ControlStructureTypes.TRY)
      .code("try")
      .lineNumber(line(stmt))
      .columnNumber(column(stmt))

    val resources = stmt.getResources.asScala.flatMap(astsForExpression(_, expectedType = ExpectedType.empty)).toList
    val tryAst    = astForBlockStatement(stmt.getTryBlock, codeStr = "try")
    val catchAsts = stmt.getCatchClauses.asScala.map(astForCatchClause)
    val catchBlock = Option
      .when(catchAsts.nonEmpty) {
        Ast(NewBlock().code("catch")).withChildren(catchAsts)
      }
      .toList
    val finallyAst =
      stmt.getFinallyBlock.toScala.map(astForBlockStatement(_, "finally")).toList

    val controlStructureAst = Ast(tryNode)
      .withChild(tryAst)
      .withChildren(catchBlock)
      .withChildren(finallyAst)

    resources.appended(controlStructureAst)
  }

  private def astsForStatement(statement: Statement): Seq[Ast] = {
    // TODO: Implement missing handlers
    // case _: LocalClassDeclarationStmt  => Seq()
    // case _: LocalRecordDeclarationStmt => Seq()
    // case _: YieldStmt                  => Seq()
    statement match {
      case x: ExplicitConstructorInvocationStmt =>
        Seq(astForExplicitConstructorInvocation(x))
      case x: AssertStmt       => Seq(astForAssertStatement(x))
      case x: BlockStmt        => Seq(astForBlockStatement(x))
      case x: BreakStmt        => Seq(astForBreakStatement(x))
      case x: ContinueStmt     => Seq(astForContinueStatement(x))
      case x: DoStmt           => Seq(astForDo(x))
      case _: EmptyStmt        => Seq() // Intentionally skipping this
      case x: ExpressionStmt   => astsForExpression(x.getExpression, ExpectedType.Void)
      case x: ForEachStmt      => astForForEach(x)
      case x: ForStmt          => Seq(astForFor(x))
      case x: IfStmt           => Seq(astForIf(x))
      case x: LabeledStmt      => astsForLabeledStatement(x)
      case x: ReturnStmt       => Seq(astForReturnNode(x))
      case x: SwitchStmt       => Seq(astForSwitchStatement(x))
      case x: SynchronizedStmt => Seq(astForSynchronizedStatement(x))
      case x: ThrowStmt        => Seq(astForThrow(x))
      case x: TryStmt          => astsForTry(x)
      case x: WhileStmt        => Seq(astForWhile(x))
      case x =>
        logger.warn(s"Attempting to generate AST for unknown statement of type ${x.getClass}")
        Seq(unknownAst(x))
    }
  }

  private def astForElse(maybeStmt: Option[Statement]): Option[Ast] = {
    maybeStmt.map { stmt =>
      val elseAsts = astsForStatement(stmt)

      val elseNode =
        NewControlStructure()
          .controlStructureType(ControlStructureTypes.ELSE)
          .lineNumber(line(stmt))
          .columnNumber(column(stmt))
          .code("else")

      Ast(elseNode).withChildren(elseAsts)
    }
  }

  def astForIf(stmt: IfStmt): Ast = {
    val ifNode =
      NewControlStructure()
        .controlStructureType(ControlStructureTypes.IF)
        .lineNumber(line(stmt))
        .columnNumber(column(stmt))
        .code(s"if (${stmt.getCondition.toString})")

    val conditionAst =
      astsForExpression(stmt.getCondition, ExpectedType.Boolean).headOption.toList

    val thenAsts = astsForStatement(stmt.getThenStmt)
    val elseAst  = astForElse(stmt.getElseStmt.toScala).toList

    val ast = Ast(ifNode)
      .withChildren(conditionAst)
      .withChildren(thenAsts)
      .withChildren(elseAst)

    conditionAst.flatMap(_.root.toList) match {
      case r :: Nil =>
        ast.withConditionEdge(ifNode, r)
      case _ =>
        ast
    }
  }

  def astForWhile(stmt: WhileStmt): Ast = {
    val conditionAst = astsForExpression(stmt.getCondition, ExpectedType.Boolean).headOption
    val stmtAsts     = astsForStatement(stmt.getBody)
    val code         = s"while (${stmt.getCondition.toString})"
    val lineNumber   = line(stmt)
    val columnNumber = column(stmt)

    whileAst(conditionAst, stmtAsts, Some(code), lineNumber, columnNumber)
  }

  private def astForDo(stmt: DoStmt): Ast = {
    val conditionAst = astsForExpression(stmt.getCondition, ExpectedType.Boolean).headOption
    val stmtAsts     = astsForStatement(stmt.getBody)
    val code         = s"do {...} while (${stmt.getCondition.toString})"
    val lineNumber   = line(stmt)
    val columnNumber = column(stmt)

    doWhileAst(conditionAst, stmtAsts, Some(code), lineNumber, columnNumber)
  }

  private def astForBreakStatement(stmt: BreakStmt): Ast = {
    val node = NewControlStructure()
      .controlStructureType(ControlStructureTypes.BREAK)
      .lineNumber(line(stmt))
      .columnNumber(column(stmt))
      .code(stmt.toString)
    Ast(node)
  }

  private def astForContinueStatement(stmt: ContinueStmt): Ast = {
    val node = NewControlStructure()
      .controlStructureType(ControlStructureTypes.CONTINUE)
      .lineNumber(line(stmt))
      .columnNumber(column(stmt))
      .code(stmt.toString)
    Ast(node)
  }

  private def getForCode(stmt: ForStmt): String = {
    val init    = stmt.getInitialization.asScala.map(_.toString).mkString(", ")
    val compare = stmt.getCompare.toScala.map(_.toString)
    val update  = stmt.getUpdate.asScala.map(_.toString).mkString(", ")
    s"for ($init; $compare; $update)"
  }
  def astForFor(stmt: ForStmt): Ast = {
    val forNode =
      NewControlStructure()
        .controlStructureType(ControlStructureTypes.FOR)
        .code(getForCode(stmt))
        .lineNumber(line(stmt))
        .columnNumber(column(stmt))

    val initAsts =
      stmt.getInitialization.asScala.flatMap(astsForExpression(_, expectedType = ExpectedType.empty))

    val compareAsts = stmt.getCompare.toScala.toList.flatMap {
      astsForExpression(_, ExpectedType.Boolean)
    }

    val updateAsts = stmt.getUpdate.asScala.toList.flatMap {
      astsForExpression(_, ExpectedType.empty)
    }

    val stmtAsts =
      astsForStatement(stmt.getBody)

    val ast = Ast(forNode)
      .withChildren(initAsts)
      .withChildren(compareAsts)
      .withChildren(updateAsts)
      .withChildren(stmtAsts)

    compareAsts.flatMap(_.root) match {
      case c :: Nil =>
        ast.withConditionEdge(forNode, c)
      case _ => ast
    }
  }

  private def iterableAssignAstsForNativeForEach(
    iterableExpression: Expression,
    iterableType: Option[String]
  ): (NodeTypeInfo, Seq[Ast]) = {
    val lineNo       = line(iterableExpression)
    val expectedType = ExpectedType(iterableType)

    val iterableAst = astsForExpression(iterableExpression, expectedType = expectedType) match {
      case Nil =>
        logger.error(s"Could not create AST for iterable expr $iterableExpression: $filename:l$lineNo")
        Ast()
      case iterableAstHead :: Nil => iterableAstHead
      case iterableAsts =>
        logger.warn(
          s"Found multiple ASTS for iterable expr $iterableExpression: $filename:l$lineNo\nDropping all but the first!"
        )
        iterableAsts.head
    }

    val iterableName = nextIterableName()
    val iterableLocalNode =
      NewLocal()
        .name(iterableName)
        .code(iterableName)
        .lineNumber(lineNo)
    iterableType.foreach(iterableLocalNode.typeFullName(_))
    val iterableLocalAst = Ast(iterableLocalNode)

    val iterableAssignNode =
      operatorCallNode(Operators.assignment, code = "", line = lineNo, typeFullName = iterableType)
    val iterableAssignIdentifier = identifierNode(iterableName, iterableType, lineNo)
    val iterableAssignArgs       = List(Ast(iterableAssignIdentifier), iterableAst)
    val iterableAssignAst =
      callAst(iterableAssignNode, iterableAssignArgs)
        .withRefEdge(iterableAssignIdentifier, iterableLocalNode)

    (
      NodeTypeInfo(iterableLocalNode, iterableLocalNode.name, Some(iterableLocalNode.typeFullName)),
      List(iterableLocalAst, iterableAssignAst)
    )
  }

  private def nativeForEachIdxLocalNode(lineNo: Option[Integer]): NewLocal = {
    val idxName      = nextIndexName()
    val typeFullName = TypeConstants.Int
    val idxLocal =
      NewLocal()
        .name(idxName)
        .typeFullName(typeFullName)
        .code(idxName)
        .lineNumber(lineNo)
    scopeStack.addToScope(idxLocal, idxName, Some(typeFullName))
    idxLocal
  }

  private def nativeForEachIdxInitializerAst(lineNo: Option[Integer], idxLocal: NewLocal): Ast = {
    val idxName = idxLocal.name
    val idxInitializerCallNode = operatorCallNode(
      Operators.assignment,
      code = s"int $idxName = 0",
      line = lineNo,
      typeFullName = Some(TypeConstants.Int)
    )
    val idxIdentifierArg = identifierNode(idxName, Some(idxLocal.typeFullName), lineNo)
    val zeroLiteral =
      NewLiteral()
        .code("0")
        .typeFullName(TypeConstants.Int)
        .lineNumber(lineNo)
    val idxInitializerArgAsts = List(Ast(idxIdentifierArg), Ast(zeroLiteral))
    callAst(idxInitializerCallNode, idxInitializerArgAsts)
      .withRefEdge(idxIdentifierArg, idxLocal)
  }

  private def nativeForEachCompareAst(
    lineNo: Option[Integer],
    iterableSource: NodeTypeInfo,
    idxLocal: NewLocal
  ): Ast = {
    val idxName = idxLocal.name

    val compareNode = operatorCallNode(
      Operators.lessThan,
      code = s"$idxName < ${iterableSource.name}.length",
      typeFullName = Some(TypeConstants.Boolean),
      line = lineNo
    )
    val comparisonIdxIdentifier = identifierNode(idxName, Some(idxLocal.typeFullName), lineNo)
    val comparisonFieldAccess = operatorCallNode(
      Operators.fieldAccess,
      code = s"${iterableSource.name}.length",
      typeFullName = Some(TypeConstants.Int),
      line = lineNo
    )
    val fieldAccessIdentifier      = identifierNode(iterableSource.name, iterableSource.typeFullName, lineNo)
    val fieldAccessFieldIdentifier = fieldIdentifierNode("length", lineNo)
    val fieldAccessArgs            = List(fieldAccessIdentifier, fieldAccessFieldIdentifier).map(Ast(_))
    val fieldAccessAst             = callAst(comparisonFieldAccess, fieldAccessArgs)
    val compareArgs                = List(Ast(comparisonIdxIdentifier), fieldAccessAst)

    // TODO: This is a workaround for a crash when looping over statically imported members. Handle those properly.
    val iterableSourceNode = localParamOrMemberFromNode(iterableSource)

    callAst(compareNode, compareArgs)
      .withRefEdge(comparisonIdxIdentifier, idxLocal)
      .withRefEdges(fieldAccessIdentifier, iterableSourceNode.toList)
  }

  private def nativeForEachIncrementAst(lineNo: Option[Integer], idxLocal: NewLocal): Ast = {
    val incrementNode = operatorCallNode(
      Operators.postIncrement,
      code = s"${idxLocal.name}++",
      typeFullName = Some(TypeConstants.Int),
      line = lineNo
    )
    val incrementArg    = identifierNode(idxLocal.name, Some(idxLocal.typeFullName), lineNo)
    val incrementArgAst = Ast(incrementArg)
    callAst(incrementNode, List(incrementArgAst))
      .withRefEdge(incrementArg, idxLocal)
  }

  private def variableLocalForForEachBody(stmt: ForEachStmt): NewLocal = {
    val lineNo = line(stmt)
    // Create item local
    val maybeVariable = stmt.getVariable.getVariables.asScala.toList match {
      case Nil =>
        logger.error(s"ForEach statement has empty variable list: $filename$lineNo")
        None
      case variable :: Nil => Some(variable)
      case variable :: _ =>
        logger.warn(s"ForEach statement defines multiple variables. Dropping all but the first: $filename$lineNo")
        Some(variable)
    }

    val partialLocalNode = NewLocal().lineNumber(lineNo)

    maybeVariable match {
      case Some(variable) =>
        val name         = variable.getNameAsString
        val typeFullName = typeInfoCalc.fullName(variable.getType).getOrElse("ANY")
        val localNode = partialLocalNode
          .name(name)
          .code(variable.getNameAsString)
          .typeFullName(typeFullName)

        scopeStack.addToScope(localNode, name, Some(typeFullName))
        localNode

      case None =>
        // Returning partialLocalNode here is fine since getting to this case means everything is broken anyways :)
        partialLocalNode
    }
  }

  private def localParamOrMemberFromNode(nodeTypeInfo: NodeTypeInfo): Option[NewNode] = {
    nodeTypeInfo.node match {
      case localNode: NewLocal                 => Some(localNode)
      case memberNode: NewMember               => Some(memberNode)
      case parameterNode: NewMethodParameterIn => Some(parameterNode)
      case _                                   => None
    }
  }
  private def variableAssignForNativeForEachBody(
    variableLocal: NewLocal,
    idxLocal: NewLocal,
    iterable: NodeTypeInfo
  ): Ast = {
    // Everything will be on the same line as the `for` statement, but this is the most useful
    // solution for debugging.
    val lineNo = variableLocal.lineNumber
    val varAssignNode =
      operatorCallNode(Operators.assignment, PropertyDefaults.Code, Some(variableLocal.typeFullName), lineNo)

    val targetNode = identifierNode(variableLocal.name, Some(variableLocal.typeFullName), lineNo)

    val indexAccessTypeFullName = iterable.typeFullName.map(_.replaceAll(raw"\[]", ""))
    val indexAccess =
      operatorCallNode(Operators.indexAccess, PropertyDefaults.Code, indexAccessTypeFullName, lineNo)

    val indexAccessIdentifier = identifierNode(iterable.name, iterable.typeFullName, lineNo)
    val indexAccessIndex      = identifierNode(idxLocal.name, Some(idxLocal.typeFullName), lineNo)

    val indexAccessArgsAsts = List(indexAccessIdentifier, indexAccessIndex).map(Ast(_))
    val indexAccessAst      = callAst(indexAccess, indexAccessArgsAsts)

    val iterableSourceNode = localParamOrMemberFromNode(iterable)

    val assignArgsAsts = List(Ast(targetNode), indexAccessAst)
    callAst(varAssignNode, assignArgsAsts)
      .withRefEdge(targetNode, variableLocal)
      .withRefEdges(indexAccessIdentifier, iterableSourceNode.toList)
      .withRefEdge(indexAccessIndex, idxLocal)
  }

  private def nativeForEachBodyAst(stmt: ForEachStmt, idxLocal: NewLocal, iterable: NodeTypeInfo): Ast = {
    val variableLocal     = variableLocalForForEachBody(stmt)
    val variableLocalAst  = Ast(variableLocal)
    val variableAssignAst = variableAssignForNativeForEachBody(variableLocal, idxLocal, iterable)

    stmt.getBody match {
      case block: BlockStmt =>
        astForBlockStatement(block, prefixAsts = List(variableLocalAst, variableAssignAst))
      case statement =>
        val stmtAsts  = astsForStatement(statement)
        val blockNode = NewBlock().lineNumber(variableLocal.lineNumber)
        Ast(blockNode)
          .withChild(variableLocalAst)
          .withChild(variableAssignAst)
          .withChildren(stmtAsts)
    }
  }

  private def astsForNativeForEach(stmt: ForEachStmt, iterableType: Option[String]): Seq[Ast] = {

    // This is ugly, but for a case like `for (int x : new int[] { ... })` this creates a new LOCAL
    // with the assignment `int[] $iterLocal0 = new int[] { ... }` before the FOR loop.
    val (iterableSource: NodeTypeInfo, tempIterableInitAsts) = stmt.getIterable match {
      case nameExpr: NameExpr =>
        scopeStack.lookupVariable(nameExpr.getNameAsString) match {
          // If this is not the case, then the code is broken (iterable not in scope).
          case Some(nodeTypeInfo) => (nodeTypeInfo, Nil)
          case _                  => iterableAssignAstsForNativeForEach(nameExpr, iterableType)
        }
      case iterableExpr => iterableAssignAstsForNativeForEach(iterableExpr, iterableType)
    }

    val forNode = NewControlStructure()
      .controlStructureType(ControlStructureTypes.FOR)

    val lineNo = line(stmt)

    val idxLocal          = nativeForEachIdxLocalNode(lineNo)
    val idxInitializerAst = nativeForEachIdxInitializerAst(lineNo, idxLocal)
    // TODO next: pass NodeTypeInfo around
    val compareAst   = nativeForEachCompareAst(lineNo, iterableSource, idxLocal)
    val incrementAst = nativeForEachIncrementAst(lineNo, idxLocal)
    val bodyAst      = nativeForEachBodyAst(stmt, idxLocal, iterableSource)

    val forAst = Ast(forNode)
      .withChild(Ast(idxLocal))
      .withChild(idxInitializerAst)
      .withChild(compareAst)
      .withChild(incrementAst)
      .withChild(bodyAst)
      .withConditionEdges(forNode, compareAst.root.toList)

    tempIterableInitAsts ++ Seq(forAst)
  }

  private def iteratorLocalForForEach(lineNumber: Option[Integer]): NewLocal = {
    val iteratorLocalName = nextIterableName()
    NewLocal()
      .name(iteratorLocalName)
      .code(iteratorLocalName)
      .typeFullName(TypeConstants.Iterator)
      .lineNumber(lineNumber)
  }

  private def iteratorAssignAstForForEach(
    iterExpr: Expression,
    iteratorLocalNode: NewLocal,
    iterableType: Option[String],
    lineNo: Option[Integer]
  ): Ast = {
    val iteratorAssignNode =
      operatorCallNode(Operators.assignment, code = "", typeFullName = Some(TypeConstants.Iterator), line = lineNo)
    val iteratorAssignIdentifier = identifierNode(iteratorLocalNode.name, Some(iteratorLocalNode.typeFullName), lineNo)

    val iteratorCallNode =
      callNode("iterator", iterableType, TypeConstants.Iterator, DispatchTypes.DYNAMIC_DISPATCH, lineNumber = lineNo)

    val actualIteratorAst = astsForExpression(iterExpr, expectedType = ExpectedType.empty).toList match {
      case Nil =>
        logger.warn(s"Could not create receiver ast for iterator $iterExpr")
        None

      case ast :: Nil => Some(ast)

      case ast :: _ =>
        logger.warn(s"Created multiple receiver asts for $iterExpr. Dropping all but the first.")
        Some(ast)
    }

    val iteratorCallAst =
      callAst(iteratorCallNode, base = actualIteratorAst)

    callAst(iteratorAssignNode, List(Ast(iteratorAssignIdentifier), iteratorCallAst))
      .withRefEdge(iteratorAssignIdentifier, iteratorLocalNode)
  }

  private def hasNextCallAstForForEach(iteratorLocalNode: NewLocal, lineNo: Option[Integer]): Ast = {
    val iteratorHasNextCallNode =
      callNode(
        "hasNext",
        Some(TypeConstants.Iterator),
        TypeConstants.Boolean,
        DispatchTypes.DYNAMIC_DISPATCH,
        lineNumber = lineNo
      )
    val iteratorHasNextCallReceiver =
      identifierNode(iteratorLocalNode.name, Some(iteratorLocalNode.typeFullName), lineNo)

    callAst(iteratorHasNextCallNode, base = Some(Ast(iteratorHasNextCallReceiver)))
      .withRefEdge(iteratorHasNextCallReceiver, iteratorLocalNode)
  }

  private def astForIterableForEachItemAssign(iteratorLocalNode: NewLocal, variableLocal: NewLocal): Ast = {
    val lineNo          = variableLocal.lineNumber
    val forVariableType = variableLocal.typeFullName
    val varLocalAssignNode =
      operatorCallNode(Operators.assignment, PropertyDefaults.Code, Some(forVariableType), lineNo)
    val varLocalAssignIdentifier = identifierNode(variableLocal.name, Some(variableLocal.typeFullName), lineNo)

    val iterNextCallNode =
      callNode(
        "next",
        Some(TypeConstants.Iterator),
        TypeConstants.Object,
        DispatchTypes.DYNAMIC_DISPATCH,
        lineNumber = lineNo
      )
    val iterNextCallReceiver = identifierNode(iteratorLocalNode.name, Some(iteratorLocalNode.typeFullName), lineNo)
    val iterNextCallAst =
      callAst(iterNextCallNode, base = Some(Ast(iterNextCallReceiver)))
        .withRefEdge(iterNextCallReceiver, iteratorLocalNode)

    callAst(varLocalAssignNode, List(Ast(varLocalAssignIdentifier), iterNextCallAst))
      .withRefEdge(varLocalAssignIdentifier, variableLocal)
  }

  private def astForIterableForEach(stmt: ForEachStmt, iterableType: Option[String]): Seq[Ast] = {
    val lineNo = line(stmt)

    val iteratorLocalNode = iteratorLocalForForEach(lineNo)
    val iteratorAssignAst =
      iteratorAssignAstForForEach(stmt.getIterable, iteratorLocalNode, iterableType, lineNo)
    val iteratorHasNextCallAst = hasNextCallAstForForEach(iteratorLocalNode, lineNo)
    val variableLocal          = variableLocalForForEachBody(stmt)
    val variableAssignAst      = astForIterableForEachItemAssign(iteratorLocalNode, variableLocal)

    val bodyPrefixAsts = Seq(Ast(variableLocal), variableAssignAst)
    val bodyAst = stmt.getBody match {
      case block: BlockStmt =>
        astForBlockStatement(block, prefixAsts = bodyPrefixAsts)

      case bodyStmt =>
        val bodyBlockNode = NewBlock().lineNumber(lineNo)
        val bodyStmtAsts  = astsForStatement(bodyStmt)
        Ast(bodyBlockNode)
          .withChildren(bodyPrefixAsts)
          .withChildren(bodyStmtAsts)
    }

    val forNode =
      NewControlStructure()
        .controlStructureType(ControlStructureTypes.WHILE)
        .code(ControlStructureTypes.FOR)
        .lineNumber(lineNo)
        .columnNumber(column(stmt))

    val forAst = controlStructureAst(forNode, Some(iteratorHasNextCallAst), List(bodyAst))

    Seq(Ast(iteratorLocalNode), iteratorAssignAst, forAst)
  }

  private def astForForEach(stmt: ForEachStmt): Seq[Ast] = {
    scopeStack.pushNewScope(BlockScope)

    val ast = expressionReturnTypeFullName(stmt.getIterable) match {
      case Some(typeFullName) if typeFullName.endsWith("[]") =>
        astsForNativeForEach(stmt, Some(typeFullName))

      case maybeType =>
        astForIterableForEach(stmt, maybeType)
    }

    scopeStack.popScope()
    ast
  }

  private def astForSwitchStatement(stmt: SwitchStmt): Ast = {
    val switchNode =
      NewControlStructure()
        .controlStructureType(ControlStructureTypes.SWITCH)
        .code(s"switch(${stmt.getSelector.toString})")

    val selectorAsts = astsForExpression(stmt.getSelector, ExpectedType.empty)
    val selectorNode = selectorAsts.head.root.get

    val entryAsts = stmt.getEntries.asScala.flatMap(astForSwitchEntry)

    val switchBodyAst = Ast(NewBlock()).withChildren(entryAsts)

    Ast(switchNode)
      .withChildren(selectorAsts)
      .withChild(switchBodyAst)
      .withConditionEdge(switchNode, selectorNode)
  }

  private def astForSynchronizedStatement(stmt: SynchronizedStmt): Ast = {
    val parentNode =
      NewBlock()
        .lineNumber(line(stmt))
        .columnNumber(column(stmt))

    val modifier = Ast(modifierNode("SYNCHRONIZED"))

    val exprAsts = astsForExpression(stmt.getExpression, ExpectedType.empty)
    val bodyAst  = astForBlockStatement(stmt.getBody)

    Ast(parentNode)
      .withChild(modifier)
      .withChildren(exprAsts)
      .withChild(bodyAst)
  }

  private def astsForSwitchCases(entry: SwitchEntry): Seq[Ast] = {
    entry.getLabels.asScala.toList match {
      case Nil =>
        val target = NewJumpTarget()
          .name("default")
          .code("default")
        Seq(Ast(target))

      case labels =>
        labels.flatMap { label =>
          val jumpTarget = NewJumpTarget()
            .name("case")
            .code(label.toString)
          val labelAsts = astsForExpression(label, ExpectedType.empty).toList

          Ast(jumpTarget) :: labelAsts
        }
    }
  }

  private def astForSwitchEntry(entry: SwitchEntry): Seq[Ast] = {
    val labelAsts = astsForSwitchCases(entry)

    val statementAsts = entry.getStatements.asScala.flatMap(astsForStatement)

    labelAsts ++ statementAsts
  }

  private def astForAssertStatement(stmt: AssertStmt): Ast = {
    val callNode = NewCall()
      .name("assert")
      .methodFullName("assert")
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .code(stmt.toString)
      .lineNumber(line(stmt))
      .columnNumber(column(stmt))

    val args = astsForExpression(stmt.getCheck, ExpectedType.Boolean)
    callAst(callNode, args)
  }

  private def astForBlockStatement(
    stmt: BlockStmt,
    codeStr: String = "<empty>",
    prefixAsts: Seq[Ast] = Seq.empty
  ): Ast = {
    scopeStack.pushNewScope(BlockScope)

    val block = NewBlock()
      .code(codeStr)
      .lineNumber(line(stmt))
      .columnNumber(column(stmt))

    val stmtAsts = stmt.getStatements.asScala.flatMap(astsForStatement)

    scopeStack.popScope()
    Ast(block)
      .withChildren(prefixAsts)
      .withChildren(stmtAsts)
  }

  private def astForReturnNode(ret: ReturnStmt): Ast = {
    val returnNode = NewReturn()
      .lineNumber(line(ret))
      .columnNumber(column(ret))
      .code(ret.toString)
    if (ret.getExpression.isPresent) {
      val expectedType = scopeStack.getEnclosingMethodReturnType.getOrElse(ExpectedType.empty)
      val exprAsts     = astsForExpression(ret.getExpression.get(), expectedType)
      returnAst(returnNode, exprAsts)
    } else {
      Ast(returnNode)
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

    val callNode = operatorCallNode(
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
    val callNode = operatorCallNode(
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
      val callNode     = operatorCallNode(Operators.alloc, code = expr.toString, typeFullName = Some(typeFullName))
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
    val callNode = operatorCallNode(
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
      val resolvedType = Try(value.calculateResolvedType()).toOption
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

    val callNode = operatorCallNode(
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

    val callNode = operatorCallNode(
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

    val callNode = operatorCallNode(operatorName, code, Some(typeFullName), line(expr), column(expr))

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
      val typeFullName = typeInfoCalc
        .fullName(variable.getType)
        .orElse(scopeStack.lookupVariable(variable.getTypeAsString).flatMap(_.typeFullName))
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

  private def assignmentsForVarDecl(
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
        typeInfoCalc
          .fullName(variable.getType)
          .orElse(scopeStack.lookupVariableType(name))
          .orElse(scopeStack.lookupVariableType(javaParserVarType, wildcardFallback = true))

      val typeFullName =
        variableTypeFullName.orElse(initializerTypeFullName)

      // Need the actual resolvedType here for when the RHS is a lambda expression.
      val resolvedExpectedType = Try(symbolSolver.toResolvedType(variable.getType, classOf[ResolvedType])).toOption
      val initializerAsts      = astsForExpression(initializer, ExpectedType(typeFullName, resolvedExpectedType))

      val typeName = typeFullName
        .map(TypeNodePass.fullToShortName)
        .getOrElse(s"${Defines.UnresolvedNamespace}.${variable.getTypeAsString}")
      val code = s"$typeName $name = ${initializerAsts.rootCodeOrEmpty}"

      val callNode = operatorCallNode(Operators.assignment, code, typeFullName, lineNumber, columnNumber)

      val targetAst = scopeStack.lookupVariable(name) match {
        case Some(nodeTypeInfo) if nodeTypeInfo.isField && !nodeTypeInfo.isStatic =>
          val thisType = scopeStack.getEnclosingTypeDecl.map(_.fullName)
          fieldAccessAst(NameConstants.This, thisType, name, typeFullName, line(variable), column(variable))

        case maybeCorrespNode =>
          val identifier =
            identifierNode(name, Some(typeFullName.getOrElse(TypeConstants.Any)), line(variable), column(variable))
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
        scopeStack.lookupVariable(identifier.name).foreach { nodeInfo =>
          diffGraph.addEdge(identifier, nodeInfo.node, EdgeTypes.REF)
        }

      case _ => // Nothing to do in this case
    }

    callAst(initNode, args.toList, Some(targetAst))
  }

  private def astsForVariableDecl(varDecl: VariableDeclarationExpr): Seq[Ast] = {
    val locals    = localsForVarDecl(varDecl)
    val localAsts = locals.map { Ast(_) }

    locals.foreach { local =>
      scopeStack.addToScope(local, local.name, Some(local.typeFullName))
    }

    val assignments =
      assignmentsForVarDecl(varDecl.getVariables.asScala, line(varDecl), column(varDecl))

    localAsts ++ assignments
  }

  private def astForClassExpr(expr: ClassExpr): Ast = {
    val someTypeFullName = Some(TypeConstants.Class)
    val callNode = operatorCallNode(Operators.fieldAccess, expr.toString, someTypeFullName, line(expr), column(expr))

    val identifierType = typeInfoCalc.fullName(expr.getType)
    val identifier     = identifierNode(expr.getTypeAsString, identifierType, line(expr), column(expr))
    val idAst          = Ast(identifier)

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

    val callNode = operatorCallNode(Operators.conditional, expr.toString, Some(typeFullName), line(expr), column(expr))

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

    val callNode = operatorCallNode(Operators.fieldAccess, expr.toString, Some(typeFullName), line(expr), column(expr))

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
    val callNode = operatorCallNode(Operators.instanceOf, expr.toString, booleanTypeFullName, line(expr), column(expr))

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
    val identifier       = identifierNode(identifierName, typeFullName, lineNo, columnNo)
    val maybeCorrespNode = scopeStack.lookupVariable(identifierName).map(_.node)

    val fieldIdentifier = NewFieldIdentifier()
      .code(fieldIdentifierName)
      .canonicalName(fieldIdentifierName)
      .lineNumber(lineNo)
      .columnNumber(columnNo)

    val fieldAccessCode = s"$identifierName.$fieldIdentifierName"
    val fieldAccess =
      operatorCallNode(
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
          scopeStack.getEnclosingTypeDecl.map(_.name).getOrElse(s"${Defines.UnresolvedNamespace}.$name")
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
        val identifier =
          identifierNode(name, typeFullName.orElse(Some(TypeConstants.Any)), line(nameExpr), column(nameExpr))

        val variableOption = scopeStack
          .lookupVariable(name)
          .filter(variableInfo =>
            variableInfo.node.isInstanceOf[NewMethodParameterIn] || variableInfo.node.isInstanceOf[NewLocal]
          )

        variableOption.foldLeft(Ast(identifier))((ast, variableInfo) => ast.withRefEdge(identifier, variableInfo.node))
    }

  }

  private def argumentTypesForMethodLike(
    maybeResolvedMethodLike: Try[ResolvedMethodLikeDeclaration]
  ): Option[List[String]] = {
    maybeResolvedMethodLike.toOption
      .flatMap(calcParameterTypes(_, ResolvedTypeParametersMap.empty()))
  }

  private def initNode(
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
      .orElse(scopeStack.lookupVariableType(expr.getTypeAsString))
      .orElse(expectedType.fullName)

    val argumentTypes = argumentTypesForMethodLike(maybeResolvedExpr)

    val allocNode = operatorCallNode(
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
    val identifier    = identifierNode(tempName, Some(allocNode.typeFullName))
    val identifierAst = Ast(identifier)

    val allocAst = Ast(allocNode)

    val assignmentNode = operatorCallNode(Operators.assignment, PropertyDefaults.Code, Some(allocNode.typeFullName))

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

  private def astForThisExpr(expr: ThisExpr, expectedType: ExpectedType): Ast = {
    val typeFullName =
      expressionReturnTypeFullName(expr)
        .orElse(expectedType.fullName)

    val identifier = identifierNode(expr.toString, typeFullName, line(expr), column(expr))
    val thisParam  = scopeStack.lookupVariable(NameConstants.This)

    thisParam.foreach { nodeTypeInfo =>
      diffGraph.addEdge(identifier, nodeTypeInfo.node, EdgeTypes.REF)
    }

    Ast(identifier)
  }

  private def astForExplicitConstructorInvocation(stmt: ExplicitConstructorInvocationStmt): Ast = {
    val maybeResolved = tryWithSafeStackOverflow(stmt.resolve())
    val args          = argAstsForCall(stmt, maybeResolved, stmt.getArguments)
    val argTypes      = argumentTypesForMethodLike(maybeResolved)

    val typeFullName = maybeResolved.toOption
      .map(_.declaringType())
      .flatMap(typeInfoCalc.fullName)

    val callRoot = initNode(
      typeFullName.orElse(Some(TypeConstants.Any)),
      argTypes,
      args.size,
      stmt.toString,
      line(stmt),
      column(stmt)
    )

    val thisNode = identifierNode(NameConstants.This, typeFullName.orElse(Some(TypeConstants.Any)))
    scopeStack.lookupVariable(NameConstants.This).foreach { thisParam =>
      diffGraph.addEdge(thisNode, thisParam.node, EdgeTypes.REF)
    }
    val thisAst = Ast(thisNode)

    callAst(callRoot, args, Some(thisAst))
  }

  private def astsForExpression(expression: Expression, expectedType: ExpectedType): Seq[Ast] = {
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

  private def unknownAst(node: Node): Ast = {
    val unknownNode =
      NewUnknown()
        .code(node.toString)
        .lineNumber(line(node))
        .columnNumber(column(node))

    Ast(unknownNode)
  }

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
      identifierNode(name, typeFullName, line(call), column(call))
    }
  }

  private def nextLambdaName(): String = {
    s"$LambdaNamePrefix${lambdaKeyPool.next}"
  }

  private def nextIndexName(): String = {
    s"$IndexNamePrefix${indexKeyPool.next}"
  }

  private def nextIterableName(): String = {
    s"$IterableNamePrefix${iterableKeyPool.next}"
  }

  private def genericParamTypeMapForLambda(expectedType: ExpectedType): ResolvedTypeParametersMap = {
    expectedType.resolvedType
      // This should always be true for correct code
      .collect { case r: ResolvedReferenceType => r }
      .map(_.typeParametersMap())
      .getOrElse(new ResolvedTypeParametersMap.Builder().build())
  }

  private def buildParamListForLambda(
    expr: LambdaExpr,
    maybeBoundMethod: Option[ResolvedMethodDeclaration],
    expectedTypeParamTypes: ResolvedTypeParametersMap
  ): Seq[Ast] = {
    val lambdaParameters = expr.getParameters.asScala.toList
    val paramTypesList = maybeBoundMethod match {
      case Some(resolvedMethod) =>
        val resolvedParameters = (0 until resolvedMethod.getNumberOfParams).map(resolvedMethod.getParam)

        // Substitute generic typeParam with the expected type if it can be found; leave unchanged otherwise.
        resolvedParameters.map(param => Try(param.getType)).map {
          case Success(resolvedType: ResolvedTypeVariable) =>
            val typ = expectedTypeParamTypes.getValue(resolvedType.asTypeParameter)
            typeInfoCalc.fullName(typ)

          case Success(resolvedType) => typeInfoCalc.fullName(resolvedType)

          case Failure(_) => None
        }

      case None =>
        // Unless types are explicitly specified in the lambda definition,
        // this will yield the erased types which is why the actual lambda
        // expression parameters are only used as a fallback.
        lambdaParameters
          .map(_.getType)
          .map(typeInfoCalc.fullName)
    }

    if (paramTypesList.sizeIs != lambdaParameters.size) {
      logger.error(s"Found different number lambda params and param types for $expr. Some parameters will be missing.")
    }

    val parameterNodes = lambdaParameters
      .zip(paramTypesList)
      .zipWithIndex
      .map { case ((param, maybeType), idx) =>
        val name         = param.getNameAsString
        val typeFullName = maybeType.getOrElse(s"${Defines.UnresolvedNamespace}")
        val code         = s"$typeFullName $name"
        val evalStrat =
          if (param.getType.isPrimitiveType) EvaluationStrategies.BY_VALUE else EvaluationStrategies.BY_SHARING
        val paramNode = NewMethodParameterIn()
          .name(name)
          .index(idx + 1)
          .order(idx + 1)
          .code(code)
          .evaluationStrategy(evalStrat)
          .typeFullName(typeFullName)
          .lineNumber(line(expr))
          .columnNumber(column(expr))
        typeInfoCalc.registerType(typeFullName)
        paramNode
      }

    parameterNodes.foreach { paramNode =>
      scopeStack.addToScope(paramNode, paramNode.name, Some(paramNode.typeFullName))
    }

    parameterNodes.map(Ast(_))
  }

  private def getLambdaReturnType(
    maybeResolvedLambdaType: Option[ResolvedType],
    maybeBoundMethod: Option[ResolvedMethodDeclaration],
    expectedTypeParamTypes: ResolvedTypeParametersMap
  ): Option[String] = {
    val maybeBoundMethodReturnType = maybeBoundMethod.flatMap { boundMethod =>
      Try(boundMethod.getReturnType).collect {
        case returnType: ResolvedTypeVariable => expectedTypeParamTypes.getValue(returnType.asTypeParameter)
        case other                            => other
      }.toOption
    }

    val returnType = maybeBoundMethodReturnType.orElse(maybeResolvedLambdaType)
    returnType.flatMap(typeInfoCalc.fullName)
  }

  def closureBinding(closureBindingId: String, originalName: String): NewClosureBinding = {
    NewClosureBinding()
      .closureBindingId(closureBindingId)
      .closureOriginalName(originalName)
      .evaluationStrategy(EvaluationStrategies.BY_SHARING)
  }

  private def closureBindingsForCapturedNodes(
    captured: List[NodeTypeInfo],
    lambdaMethodName: String
  ): List[ClosureBindingEntry] = {
    captured.map { capturedNode =>
      val closureBindingId   = s"$filename:$lambdaMethodName:${capturedNode.name}"
      val closureBindingNode = closureBinding(closureBindingId, capturedNode.name)
      ClosureBindingEntry(capturedNode, closureBindingNode)
    }
  }

  private def localsForCapturedNodes(closureBindingEntries: List[ClosureBindingEntry]): List[NewLocal] = {
    val localsForCaptured =
      closureBindingEntries.map { case ClosureBindingEntry(nodeTypeInfo, binding) =>
        val typeFullName = nodeTypeInfo.typeFullName.getOrElse(TypeConstants.Any)
        val local = NewLocal()
          .name(nodeTypeInfo.name)
          .code(nodeTypeInfo.name)
          .closureBindingId(binding.closureBindingId)
          .typeFullName(typeFullName)
        local
      }
    localsForCaptured.foreach { local => scopeStack.addToScope(local, local.name, Some(local.typeFullName)) }
    localsForCaptured
  }

  private def astForLambdaBody(
    body: Statement,
    localsForCapturedVars: Seq[NewLocal],
    returnType: Option[String]
  ): Ast = {
    body match {
      case block: BlockStmt => astForBlockStatement(block, prefixAsts = localsForCapturedVars.map(Ast(_)))

      case stmt =>
        val blockAst = Ast(NewBlock().lineNumber(line(body)))
        val bodyAst = if (returnType.contains(TypeConstants.Void)) {
          astsForStatement(stmt)
        } else {
          val returnNode =
            NewReturn()
              .code(s"return ${body.toString}")
              .lineNumber(line(body))
          val returnArgs = astsForStatement(stmt)
          Seq(returnAst(returnNode, returnArgs))
        }

        blockAst
          .withChildren(localsForCapturedVars.map(Ast(_)))
          .withChildren(bodyAst)
    }
  }

  private def lambdaMethodSignature(returnType: Option[String], parameters: Seq[Ast]): String = {
    val maybeParameterTypes = toOptionList(parameters.map(_.rootType))
    val containsEmptyType   = maybeParameterTypes.exists(_.contains(ParameterDefaults.TypeFullName))

    (returnType, maybeParameterTypes) match {
      case (Some(returnTpe), Some(parameterTpes)) if !containsEmptyType =>
        composeMethodLikeSignature(returnTpe, parameterTpes)

      case _ => composeUnresolvedSignature(parameters.size)
    }
  }

  private def createLambdaMethodNode(
    lambdaName: String,
    parameters: Seq[Ast],
    returnType: Option[String]
  ): NewMethod = {
    val enclosingTypeName = scopeStack.getEnclosingTypeDecl.map(_.fullName).getOrElse(Defines.UnresolvedNamespace)
    val signature         = lambdaMethodSignature(returnType, parameters)
    val lambdaFullName    = composeMethodFullName(enclosingTypeName, lambdaName, signature)

    NewMethod()
      .name(lambdaName)
      .fullName(lambdaFullName)
      .signature(signature)
      .filename(filename)
      .code("<lambda>")
  }

  private def addClosureBindingsToDiffGraph(
    bindingEntries: Iterable[ClosureBindingEntry],
    methodRef: NewMethodRef
  ): Unit = {
    bindingEntries.foreach { case ClosureBindingEntry(nodeTypeInfo, closureBinding) =>
      diffGraph.addNode(closureBinding)
      diffGraph.addEdge(closureBinding, nodeTypeInfo.node, EdgeTypes.REF)
      diffGraph.addEdge(methodRef, closureBinding, EdgeTypes.CAPTURE)
    }
  }

  private def createAndPushLambdaMethod(
    expr: LambdaExpr,
    lambdaMethodName: String,
    implementedInfo: LambdaImplementedInfo,
    localsForCaptured: Seq[NewLocal],
    expectedLambdaType: ExpectedType
  ): NewMethod = {
    val implementedMethod    = implementedInfo.implementedMethod
    val implementedInterface = implementedInfo.implementedInterface

    // We need to get this information from the expected type as the JavaParser
    // symbol solver returns the erased types when resolving the lambda itself.
    val expectedTypeParamTypes = genericParamTypeMapForLambda(expectedLambdaType)
    val parametersWithoutThis  = buildParamListForLambda(expr, implementedMethod, expectedTypeParamTypes)

    val returnType = getLambdaReturnType(implementedInterface, implementedMethod, expectedTypeParamTypes)

    val lambdaMethodBody = astForLambdaBody(expr.getBody, localsForCaptured, returnType)

    val thisParam = lambdaMethodBody.nodes
      .collect { case identifier: NewIdentifier => identifier }
      .find { identifier => identifier.name == NameConstants.This || identifier.name == NameConstants.Super }
      .map { _ =>
        val typeFullName = scopeStack.getEnclosingTypeDecl.map(_.fullName)
        Ast(thisNodeForMethod(typeFullName, line(expr)))
      }
      .toList

    val parameters = thisParam ++ parametersWithoutThis

    val lambdaMethodNode = createLambdaMethodNode(lambdaMethodName, parametersWithoutThis, returnType)
    val returnNode       = methodReturnNode(returnType.getOrElse(TypeConstants.Any), None, line(expr), column(expr))
    val virtualModifier  = Some(modifierNode(ModifierTypes.VIRTUAL))
    val staticModifier   = Option.when(thisParam.isEmpty)(modifierNode(ModifierTypes.STATIC))
    val privateModifier  = Some(modifierNode(ModifierTypes.PRIVATE))

    val modifiers = List(virtualModifier, staticModifier, privateModifier).flatten.map(Ast(_))

    val lambdaParameterNamesToNodes =
      parameters
        .flatMap(_.root)
        .collect { case param: NewMethodParameterIn => param }
        .map { param => param.name -> param }
        .toMap

    val identifiersMatchingParams = lambdaMethodBody.nodes
      .collect { case identifier: NewIdentifier => identifier }
      .filter { identifier => lambdaParameterNamesToNodes.contains(identifier.name) }

    val lambdaMethodAstWithoutRefs =
      Ast(lambdaMethodNode)
        .withChildren(parameters)
        .withChild(lambdaMethodBody)
        .withChild(Ast(returnNode))
        .withChildren(modifiers)

    val lambdaMethodAst = identifiersMatchingParams.foldLeft(lambdaMethodAstWithoutRefs)((ast, identifier) =>
      ast.withRefEdge(identifier, lambdaParameterNamesToNodes(identifier.name))
    )

    scopeStack.addLambdaMethod(lambdaMethodAst)

    lambdaMethodNode
  }

  private def createAndPushLambdaTypeDecl(
    lambdaMethodNode: NewMethod,
    implementedInfo: LambdaImplementedInfo
  ): NewTypeDecl = {
    val inheritsFromTypeFullName =
      implementedInfo.implementedInterface
        .flatMap(typeInfoCalc.fullName)
        .orElse(Some(TypeConstants.Object))
        .toList

    typeInfoCalc.registerType(lambdaMethodNode.fullName)
    val lambdaTypeDeclNode =
      NewTypeDecl()
        .fullName(lambdaMethodNode.fullName)
        .name(lambdaMethodNode.name)
        .inheritsFromTypeFullName(inheritsFromTypeFullName)
    scopeStack.addLambdaDecl(Ast(lambdaTypeDeclNode))

    lambdaTypeDeclNode
  }

  private def getLambdaImplementedInfo(expr: LambdaExpr, expectedType: ExpectedType): LambdaImplementedInfo = {
    val maybeImplementedType = {
      val maybeResolved = tryWithSafeStackOverflow(expr.calculateResolvedType())
      maybeResolved.toOption
        .orElse(expectedType.resolvedType)
        .collect { case refType: ResolvedReferenceType => refType }
    }

    val maybeImplementedInterface = maybeImplementedType.flatMap(_.getTypeDeclaration.toScala)

    if (maybeImplementedInterface.isEmpty) {
      val location = s"$filename:${line(expr)}:${column(expr)}"
      logger.debug(
        s"Could not resolve the interface implemented by a lambda. Type info may be missing: $location. Type info may be missing."
      )
    }

    val maybeBoundMethod = maybeImplementedInterface.flatMap { interface =>
      interface.getDeclaredMethods.asScala
        .filter(_.isAbstract)
        .filterNot { method =>
          // Filter out java.lang.Object methods re-declared by the interface as these are also considered abstract.
          // See https://docs.oracle.com/javase/8/docs/api/java/lang/FunctionalInterface.html for details.
          Try(method.getSignature) match {
            case Success(signature) => ObjectMethodSignatures.contains(signature)
            case Failure(_) =>
              false // If the signature could not be calculated, it's probably not a standard object method.
          }
        }
        .headOption
    }

    LambdaImplementedInfo(maybeImplementedType, maybeBoundMethod)
  }

  private def astForLambdaExpr(expr: LambdaExpr, expectedType: ExpectedType): Ast = {
    scopeStack.pushNewScope(MethodScope(expectedType))

    val lambdaMethodName = nextLambdaName()

    val capturedVariables              = scopeStack.getCapturedVariables
    val closureBindingsForCapturedVars = closureBindingsForCapturedNodes(capturedVariables, lambdaMethodName)
    val localsForCaptured              = localsForCapturedNodes(closureBindingsForCapturedVars)
    val implementedInfo                = getLambdaImplementedInfo(expr, expectedType)
    val lambdaMethodNode =
      createAndPushLambdaMethod(expr, lambdaMethodName, implementedInfo, localsForCaptured, expectedType)

    val methodRef =
      NewMethodRef()
        .methodFullName(lambdaMethodNode.fullName)
        .typeFullName(lambdaMethodNode.fullName)
        .code(lambdaMethodNode.fullName)

    addClosureBindingsToDiffGraph(closureBindingsForCapturedVars, methodRef)

    val interfaceBinding = implementedInfo.implementedMethod.map { implementedMethod =>
      NewBinding()
        .name(implementedMethod.getName)
        .methodFullName(lambdaMethodNode.fullName)
        .signature(lambdaMethodNode.signature)
    }

    val bindingTable = getLambdaBindingTable(
      LambdaBindingInfo(lambdaMethodNode.fullName, implementedInfo.implementedInterface, interfaceBinding)
    )

    val lambdaTypeDeclNode = createAndPushLambdaTypeDecl(lambdaMethodNode, implementedInfo)
    createBindingNodes(lambdaTypeDeclNode, bindingTable)

    scopeStack.popScope()
    Ast(methodRef)
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
      case Some(scope: ThisExpr) =>
        expressionReturnTypeFullName(scope)
          .orElse(scopeStack.getEnclosingTypeDecl.map(_.fullName))

      case Some(scope: SuperExpr) =>
        expressionReturnTypeFullName(scope)
          .orElse(scopeStack.getEnclosingTypeDecl.flatMap(_.inheritsFromTypeFullName.headOption))

      case Some(scope) => expressionReturnTypeFullName(scope)

      case None =>
        tryWithSafeStackOverflow(callExpr.resolve()).toOption
          .flatMap { methodDeclOption =>
            if (methodDeclOption.isStatic) typeInfoCalc.fullName(methodDeclOption.declaringType())
            else scopeStack.getEnclosingTypeDecl.map(_.fullName)
          }
          .orElse(scopeStack.getEnclosingTypeDecl.map(_.fullName))
    }

    maybeType.map(typeInfoCalc.registerType)
  }

  private def argAstsForCall(
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
        val arrayInitializer = operatorCallNode(
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
          thisParam <- scopeStack.lookupVariable(NameConstants.This)
        } diffGraph.addEdge(obj, thisParam.node, EdgeTypes.REF)
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

    val identifier =
      identifierNode(NameConstants.Super, Some(typeFullName), line(superExpr), column(superExpr))
        .name(NameConstants.This) // Necessary for closed source dataflow

    Ast(identifier)
  }

  private def astsForParameterList(parameters: NodeList[Parameter]): Seq[Ast] = {
    parameters.asScala.toList.zipWithIndex.map { case (param, idx) =>
      astForParameter(param, idx + 1)
    }
  }

  private def astForParameter(parameter: Parameter, childNum: Int): Ast = {
    val maybeArraySuffix = if (parameter.isVarArgs) "[]" else ""
    val typeFullName =
      typeInfoCalc
        .fullName(parameter.getType)
        .orElse(scopeStack.lookupVariableType(parameter.getTypeAsString, wildcardFallback = true))
        .map(_ ++ maybeArraySuffix)
        .getOrElse(s"${Defines.UnresolvedNamespace}.${parameter.getTypeAsString}")
    val evalStrat =
      if (parameter.getType.isPrimitiveType) EvaluationStrategies.BY_VALUE else EvaluationStrategies.BY_SHARING
    typeInfoCalc.registerType(typeFullName)

    val parameterNode = NewMethodParameterIn()
      .name(parameter.getName.toString)
      .code(parameter.toString)
      .lineNumber(line(parameter))
      .columnNumber(column(parameter))
      .evaluationStrategy(evalStrat)
      .typeFullName(typeFullName)
      .index(childNum)
      .order(childNum)
    val annotationAsts = parameter.getAnnotations.asScala.map(astForAnnotationExpr)
    val ast            = Ast(parameterNode)

    scopeStack.addToScope(parameterNode, parameter.getNameAsString, Some(parameterNode.typeFullName))
    ast.withChildren(annotationAsts)
  }

}

object AstCreator {
  def line(node: Node): Option[Integer] = {
    node.getBegin.map(x => Integer.valueOf(x.line)).toScala
  }

  def column(node: Node): Option[Integer] = {
    node.getBegin.map(x => Integer.valueOf(x.column)).toScala
  }

}
