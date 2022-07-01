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
import com.github.javaparser.resolution.{SymbolResolver, UnsolvedSymbolException}
import com.github.javaparser.resolution.declarations.{
  ResolvedConstructorDeclaration,
  ResolvedFieldDeclaration,
  ResolvedMethodDeclaration,
  ResolvedMethodLikeDeclaration,
  ResolvedReferenceTypeDeclaration
}
import com.github.javaparser.resolution.types.parametrization.ResolvedTypeParametersMap
import com.github.javaparser.resolution.types.{ResolvedReferenceType, ResolvedType, ResolvedTypeVariable}
import io.joern.javasrc2cpg.util.BindingTable.createBindingTable
import io.joern.javasrc2cpg.util.NodeBuilders.{assignmentNode, indexAccessNode}
import io.joern.javasrc2cpg.util.Scope.ScopeTypes.{BlockScope, MethodScope, NamespaceScope, TypeDeclScope}
import io.joern.javasrc2cpg.util.Scope.{NamedVariableNodeType, WildcardImportName}
import io.joern.javasrc2cpg.util.{
  BindingTable,
  BindingTableAdapterForJavaparser,
  BindingTableAdapterForLambdas,
  BindingTableEntry,
  LambdaBindingInfo,
  NodeTypeInfo,
  Scope,
  TypeInfoCalculator
}
import io.joern.javasrc2cpg.util.TypeInfoCalculator.TypeConstants
import io.joern.javasrc2cpg.util.Util.{
  composeMethodFullName,
  composeMethodLikeSignature,
  fieldIdentifierNode,
  operatorCallNode,
  rootCode,
  rootType
}
import io.shiftleft.codepropertygraph.generated.{
  ControlStructureTypes,
  DispatchTypes,
  EdgeTypes,
  EvaluationStrategies,
  ModifierTypes,
  NodeTypes,
  Operators,
  PropertyNames
}
import io.shiftleft.codepropertygraph.generated.nodes.{
  HasFullName,
  HasSignature,
  NewAnnotation,
  NewAnnotationLiteral,
  NewAnnotationParameter,
  NewAnnotationParameterAssign,
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
import io.joern.x2cpg.{Ast, AstCreatorBase}
import io.joern.x2cpg.datastructures.Global
import io.joern.x2cpg.passes.frontend.TypeNodePass
import io.shiftleft.passes.IntervalKeyPool
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate.DiffGraphBuilder

import java.util.UUID.randomUUID
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters.RichOptional
import scala.language.{existentials, implicitConversions}
import scala.util.{Failure, Success, Try}

case class ClosureBindingEntry(node: NamedVariableNodeType, binding: NewClosureBinding)

case class LambdaImplementedInfo(
  implementedInterface: Option[ResolvedReferenceType],
  implementedMethod: Option[ResolvedMethodDeclaration]
)

case class PartialConstructor(initNode: NewCall, initArgs: Seq[Ast], blockAst: Ast)

case class ExpectedType(fullName: String, resolvedType: Option[ResolvedType] = None)
object ExpectedType {
  val default: ExpectedType = ExpectedType(TypeConstants.UnresolvedType)
  val Int: ExpectedType     = ExpectedType(TypeConstants.Int)
  val Boolean: ExpectedType = ExpectedType(TypeConstants.Boolean)
  val Void: ExpectedType    = ExpectedType(TypeConstants.Void)
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
class AstCreator(filename: String, javaParserAst: CompilationUnit, global: Global, symbolResolver: SymbolResolver)
    extends AstCreatorBase(filename) {

  private val logger = LoggerFactory.getLogger(this.getClass)
  import AstCreator._

  private val scopeStack                       = Scope()
  private val typeInfoCalc: TypeInfoCalculator = TypeInfoCalculator(global, symbolResolver)
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
      val name = importStmt.getName.getIdentifier
      val typeFullName = importStmt.getNameAsString // fully qualified name
      val importNode = NewIdentifier().name(name).typeFullName(typeFullName)
      scopeStack.addToScope(importNode, name, typeFullName)
    }

    asteriskImports match {
      case imp :: Nil =>
        val name = WildcardImportName
        val typeFullName = imp.getNameAsString
        val importNode = NewIdentifier().name(name).typeFullName(typeFullName)
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

      val typeDeclAsts = withOrder(compilationUnit.getTypes) { (typ, order) =>
        astForTypeDecl(
          typ,
          order,
          astParentType = NodeTypes.NAMESPACE_BLOCK,
          astParentFullName = namespaceBlockFullName
        )
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
        throw t
    }
  }

  /** Translate package declaration into AST consisting of a corresponding namespace block.
    */
  private def astForPackageDeclaration(packageDecl: Option[PackageDeclaration]): Ast = {

    val namespaceBlock = packageDecl match {
      case Some(decl) =>
        val packageName = decl.getName.toString
        val fullName    = filename + ":" + packageName
        NewNamespaceBlock()
          .name(packageName)
          .fullName(fullName)
      case None =>
        globalNamespaceBlock()
    }
    Ast(namespaceBlock.filename(absolutePath(filename)).order(1))
  }

  private def constructorSignature(
    constructor: ResolvedConstructorDeclaration,
    typeParamValues: ResolvedTypeParametersMap
  ): String = {
    val parameterTypes = calcParameterTypes(constructor, typeParamValues)

    composeMethodLikeSignature(TypeConstants.Void, parameterTypes)
  }

  private def methodSignature(method: ResolvedMethodDeclaration, typeParamValues: ResolvedTypeParametersMap): String = {
    val parameterTypes = calcParameterTypes(method, typeParamValues)

    val returnType =
      Try(method.getReturnType).toOption
        .map(returnType => typeInfoCalc.fullName(returnType, typeParamValues))
        .getOrElse(TypeConstants.UnresolvedType)

    composeMethodLikeSignature(returnType, parameterTypes)
  }

  private def calcParameterTypes(
    methodLike: ResolvedMethodLikeDeclaration,
    typeParamValues: ResolvedTypeParametersMap
  ): collection.Seq[String] = {
    val parameterTypes =
      Range(0, methodLike.getNumberOfParams)
        .flatMap { index =>
          Try(methodLike.getParam(index)).toOption
        }
        .map { param =>
          Try(param.getType).toOption
            .map(paramType => typeInfoCalc.fullName(paramType, typeParamValues))
            .getOrElse(TypeConstants.UnresolvedType)
        }

    parameterTypes
  }

  def getBindingTable(typeDecl: ResolvedReferenceTypeDeclaration): BindingTable = {
    val fullName = typeInfoCalc.fullName(typeDecl)
    bindingTableCache.getOrElseUpdate(
      fullName,
      createBindingTable(
        fullName,
        typeDecl,
        getBindingTable,
        methodSignature,
        new BindingTableAdapterForJavaparser(methodSignature)
      )
    )
  }

  def getLambdaBindingTable(lambdaBindingInfo: LambdaBindingInfo): BindingTable = {
    val fullName = lambdaBindingInfo.fullName

    bindingTableCache.getOrElseUpdate(
      fullName,
      createBindingTable(
        fullName,
        lambdaBindingInfo,
        getBindingTable,
        methodSignature,
        new BindingTableAdapterForLambdas()
      )
    )
  }

  def createBindingNodes(typeDeclNode: NewTypeDecl, bindingTable: BindingTable): Unit = {
    // We only sort to get stable output.
    val sortedEntries =
      bindingTable.getEntries.toBuffer.sortBy((entry: BindingTableEntry) => entry.name + entry.signature)

    sortedEntries.foreach { entry =>
      val bindingNode = NewBinding()
        .name(entry.name)
        .signature(entry.signature)
        .methodFullName(entry.implementingMethodFullName)

      diffGraph.addNode(bindingNode)
      diffGraph.addEdge(typeDeclNode, bindingNode, EdgeTypes.BINDS)
    }
  }

  private def astForTypeDeclMember(
    member: BodyDeclaration[_],
    order: Int,
    clinitOrder: Int,
    astParentFullName: String
  ): AstWithStaticInit = {
    member match {
      case constructor: ConstructorDeclaration =>
        val ast = astForConstructor(constructor)

        AstWithStaticInit(ast)

      case method: MethodDeclaration =>
        val ast = astForMethod(method)

        AstWithStaticInit(ast)

      case typeDeclaration: TypeDeclaration[_] =>
        AstWithStaticInit(astForTypeDecl(typeDeclaration, order, NodeTypes.TYPE_DECL, astParentFullName))

      case fieldDeclaration: FieldDeclaration =>
        val memberAsts = withOrder(fieldDeclaration.getVariables) { (variable, idx) =>
          astForFieldVariable(variable, fieldDeclaration, order + idx - 1)
        }

        val staticInitAsts = if (fieldDeclaration.isStatic) {
          val assignments = assignmentsForVarDecl(
            fieldDeclaration.getVariables.asScala.toList,
            line(fieldDeclaration),
            column(fieldDeclaration),
            clinitOrder
          )
          assignments
        } else {
          Nil
        }

        AstWithStaticInit(memberAsts, staticInitAsts)

      case initDeclaration: InitializerDeclaration =>
        val stmts = initDeclaration.getBody.getStatements
        val asts = withOrder(stmts) { case (stmt, order) =>
          astsForStatement(stmt, order + clinitOrder)
        }.flatten
        AstWithStaticInit(ast = Seq.empty, staticInits = asts)

      case unhandled =>
        // AnnotationMemberDeclarations and InitializerDeclarations as children of typeDecls are the
        // expected cases.
        logger.info(s"Found unhandled typeDecl member ${unhandled.getClass} in file $filename")
        AstWithStaticInit.empty
    }
  }

  private def getTypeParameterMap(typeParameters: Iterable[TypeParameter]): Map[String, NewIdentifier] = {
    typeParameters.map { typeParam =>
      val name = typeParam.getNameAsString
      val typeFullName = typeParam.getTypeBound.asScala.headOption
        .flatMap { bound =>
          typeInfoCalc.fullName(bound)
        }
        .getOrElse(TypeConstants.Object)

      name -> NewIdentifier()
        .name(name)
        .typeFullName(typeFullName)
    }.toMap
  }

  private def getTypeParameterMap(node: Try[ResolvedReferenceTypeDeclaration]): Map[String, NewIdentifier] = {
    node match {
      case Success(resolved) =>
        resolved.getTypeParameters.asScala.map { typeParam =>
          val name = typeParam.getName
          val typeFullName = Try(typeParam.getUpperBound) match {
            case Success(upperBound) =>
              typeInfoCalc.fullName(upperBound)
            case Failure(_) =>
              TypeConstants.Object
          }
          // Incomplete identifier since these are never added to the AST. They're merely
          // used for the type info.
          name -> NewIdentifier()
            .name(name)
            .typeFullName(typeFullName)
        }.toMap

      case Failure(_) => Map.empty
    }
  }

  private def clinitAstsFromStaticInits(staticInits: Seq[Ast], order: Int): Option[Ast] = {
    if (staticInits.isEmpty) {
      None
    } else {
      // TODO: Get rid of magic strings
      val signature = s"${TypeConstants.Void}()"
      val fullName = scopeStack.getEnclosingTypeDecl
        .map { typeDecl =>
          s"${typeDecl.fullName}.<clinit>:$signature"
        }
        .getOrElse("")

      val methodNode = NewMethod()
        .name("<clinit>")
        .fullName(fullName)
        .signature(s"${TypeConstants.Void}()")
        .order(order)

      val staticModifier = NewModifier()
        .modifierType(ModifierTypes.STATIC)
        .code(ModifierTypes.STATIC)

      val body = Ast(NewBlock().order(1)).withChildren(staticInits)

      val methodReturn = methodReturnNode(TypeConstants.Void, None, None, None)

      val methodAst =
        Ast(methodNode)
          .withChild(Ast(staticModifier))
          .withChild(body)
          .withChild(Ast(methodReturn))

      Some(methodAst)
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

    val classPrefix = if (isInterface) "interface " else "class "
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
    val accessModifier = accessModifierType.map(NewModifier().modifierType(_))

    val abstractModifier = Option.when(isInterface || typ.getMethods.asScala.exists(_.isAbstract))(
      NewModifier().modifierType(ModifierTypes.ABSTRACT)
    )

    List(accessModifier, abstractModifier).flatten
  }

  private def createTypeDeclNode(
    typ: TypeDeclaration[_],
    order: Int,
    astParentType: String,
    astParentFullName: String,
    isInterface: Boolean
  ): NewTypeDecl = {
    val baseTypeFullNames = if (typ.isClassOrInterfaceDeclaration) {
      val decl             = typ.asClassOrInterfaceDeclaration()
      val extendedTypes    = decl.getExtendedTypes.asScala
      val implementedTypes = decl.getImplementedTypes.asScala
      val maybeJavaObjectType = if (extendedTypes.isEmpty) {
        typeInfoCalc.registerType(TypeConstants.Object)
        Seq(TypeConstants.Object)
      } else {
        Seq()
      }
      maybeJavaObjectType ++ (extendedTypes ++ implementedTypes)
        .map(typ => typeInfoCalc.fullName(typ).getOrElse(TypeConstants.UnresolvedType))
        .toList
    } else {
      List.empty[String]
    }

    val resolvedType = Try(typ.resolve()).toOption
    val name         = resolvedType.map(typeInfoCalc.name).getOrElse(typ.getNameAsString)
    val typeFullName = resolvedType.map(typeInfoCalc.fullName).getOrElse(typ.getNameAsString)

    val code = codeForTypeDecl(typ, isInterface)

    NewTypeDecl()
      .name(name)
      .fullName(typeFullName)
      .lineNumber(line(typ))
      .columnNumber(column(typ))
      .inheritsFromTypeFullName(baseTypeFullNames)
      .order(order)
      .filename(filename)
      .code(code)
      .astParentType(astParentType)
      .astParentFullName(astParentFullName)
  }

  private def astForTypeDecl(
    typ: TypeDeclaration[_],
    order: Int,
    astParentType: String,
    astParentFullName: String
  ): Ast = {
    val isInterface = typ match {
      case classDeclaration: ClassOrInterfaceDeclaration => classDeclaration.isInterface
      case _                                             => false
    }

    val typeDeclNode = createTypeDeclNode(typ, order, astParentType, astParentFullName, isInterface)

    scopeStack.pushNewScope(TypeDeclScope(typeDeclNode))

    val typeParameterMap = getTypeParameterMap(Try(typ.resolve()))
    typeParameterMap.foreach { case (identifier, node) =>
      scopeStack.addToScope(node, node.name, node.typeFullName)
    }

    val enumEntryAsts = if (typ.isEnumDeclaration) {
      withOrder(typ.asEnumDeclaration().getEntries) { case (entry, order) =>
        astForEnumEntry(entry, order)
      }
    } else {
      List.empty
    }

    var clinitOrder                      = 1
    val staticInits: mutable.Buffer[Ast] = mutable.Buffer()
    val memberAsts = withOrder(typ.getMembers) { (member, idx) =>
      val astWithInits =
        astForTypeDeclMember(
          member,
          order + enumEntryAsts.size + idx - 1,
          clinitOrder,
          astParentFullName = NodeTypes.TYPE_DECL
        )
      clinitOrder += astWithInits.staticInits.size
      staticInits.appendAll(astWithInits.staticInits)
      astWithInits.ast
    }.flatten

    val defaultConstructorAst = if (typ.getConstructors.isEmpty) {
      val order = memberAsts.size + 1
      Some(astForDefaultConstructor(order))
    } else {
      None
    }

    val annotationAsts = typ.getAnnotations.asScala.map(astForAnnotationExpr)

    val clinitAst =
      clinitAstsFromStaticInits(staticInits.toSeq, memberAsts.size + defaultConstructorAst.size + 1)

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
        .collect { case defaultConstructor: NewNode with HasFullName with HasSignature =>
          defaultConstructor
        }
        .map { defaultConstructor =>
          BindingTableEntry("<init>", defaultConstructor.signature, defaultConstructor.fullName)
        }

    // Annotation declarations need no binding table as objects of this
    // typ never get called from user code.
    // Furthermore the parser library throws an exception when trying to
    // access e.g. the declared methods of an annotation declaration.
    if (!typ.isInstanceOf[AnnotationDeclaration]) {
      Try(typ.resolve()).toOption.foreach { resolvedTypeDecl =>
        val bindingTable = getBindingTable(resolvedTypeDecl)
        defaultConstructorBindingEntry.foreach(bindingTable.add)
        createBindingNodes(typeDeclNode, bindingTable)
      }
    }

    scopeStack.popScope()

    typeDeclAst
  }

  private def astForDefaultConstructor(order: Int): Ast = {
    val typeFullName = scopeStack.getEnclosingTypeDecl.map(_.fullName).getOrElse("<empty>")
    val constructorNode = NewMethod()
      .name("<init>")
      .fullName(s"$typeFullName.<init>:${TypeConstants.Void}()")
      .signature(s"${TypeConstants.Void}()")
      .order(order)
      .filename(filename)
      .isExternal(false)

    val thisAst = thisAstForMethod(typeFullName, lineNumber = None)
    val bodyAst = Ast(NewBlock().order(1).argumentIndex(1))

    val returnNode = methodReturnNode(TypeConstants.Void, None, None, None)
    val returnAst  = Ast(returnNode)

    val modifiers = List(
      Ast(NewModifier().modifierType(ModifierTypes.CONSTRUCTOR)),
      Ast(NewModifier().modifierType(ModifierTypes.PUBLIC))
    )

    Ast(constructorNode)
      .withChildren(modifiers)
      .withChild(thisAst)
      .withChild(bodyAst)
      .withChild(returnAst)
  }

  private def astForEnumEntry(entry: EnumConstantDeclaration, order: Int): Ast = {
    val typeFullName =
      Try(entry.resolve().getType).toOption.map(typeInfoCalc.fullName).getOrElse(TypeConstants.UnresolvedType)
    val entryNode = NewMember()
      .lineNumber(line(entry))
      .columnNumber(column(entry))
      .code(entry.toString)
      .order(order)
      .name(entry.getName.toString)
      .typeFullName(typeFullName)

    val args = withOrder(entry.getArguments) { case (x, o) =>
      val children = astsForExpression(x, o, None)
      val callNode =
        NewCall()
          .name(s"$typeFullName.<init>")
          .methodFullName(s"$typeFullName.<init>")
          .dispatchType(DispatchTypes.STATIC_DISPATCH)
          .code(entry.toString)
          .lineNumber(line(entry))
          .columnNumber(column(entry))
          .argumentIndex(o)
          .order(o)
      callAst(callNode, children)
    }

    Ast(entryNode).withChildren(args)
  }

  private def modifiersForFieldDeclaration(decl: FieldDeclaration): Seq[Ast] = {
    val staticModifier =
      Option.when(decl.isStatic)(NewModifier().modifierType(ModifierTypes.STATIC).code(ModifierTypes.STATIC))

    val accessModifier = if (decl.isPublic) {
      Some(NewModifier().modifierType(ModifierTypes.PUBLIC).code(ModifierTypes.PUBLIC))
    } else if (decl.isPrivate) {
      Some(NewModifier().modifierType(ModifierTypes.PRIVATE).code(ModifierTypes.PRIVATE))
    } else if (decl.isProtected) {
      Some(NewModifier().modifierType(ModifierTypes.PROTECTED).code(ModifierTypes.PROTECTED))
    } else {
      None
    }

    List(staticModifier, accessModifier).flatten.map(Ast(_))
  }

  private def astForFieldVariable(v: VariableDeclarator, fieldDeclaration: FieldDeclaration, order: Int): Ast = {
    // TODO: Should be able to find expected type here
    val annotations = fieldDeclaration.getAnnotations
    val typeFullName =
      typeInfoCalc
        .fullName(v.getType)
        .orElse(scopeStack.getWildcardType(v.getTypeAsString))
        .getOrElse(TypeConstants.UnresolvedType)
    val name = v.getName.toString
    val memberNode =
      NewMember()
        .name(name)
        .typeFullName(typeFullName)
        .order(order)
        .code(s"$typeFullName $name")
    val memberAst      = Ast(memberNode)
    val annotationAsts = annotations.asScala.map(astForAnnotationExpr)

    val fieldDeclModifiers = modifiersForFieldDeclaration(fieldDeclaration)

    val nodeTypeInfo = NodeTypeInfo(memberNode, name = name, typeFullName = typeFullName, isField = true, isStatic = fieldDeclaration.isStatic)
    scopeStack.addToScope(name, nodeTypeInfo)

    memberAst
      .withChildren(annotationAsts)
      .withChildren(fieldDeclModifiers)
  }

  private def astForConstructor(constructorDeclaration: ConstructorDeclaration): Ast = {
    scopeStack.pushNewScope(MethodScope(ExpectedType.Void))

    val parameterAsts  = astsForParameterList(constructorDeclaration.getParameters)
    val parameterTypes = parameterAsts.map(rootType(_).getOrElse(TypeConstants.UnresolvedType))
    val signature      = s"${TypeConstants.Void}(${parameterTypes.mkString(",")})"
    val fullName       = constructorFullName(scopeStack.getEnclosingTypeDecl, signature)

    val constructorNode = createPartialMethod(constructorDeclaration)
      .fullName(fullName)
      .signature(signature)

    parameterAsts.foreach { ast =>
      ast.root match {
        case Some(p: NewMethodParameterIn) => scopeStack.addToScope(p, p.name, p.typeFullName)
        case _                             => // This should never happen
      }
    }

    val typeFullName = scopeStack.getEnclosingTypeDecl.map(_.fullName).getOrElse(TypeConstants.UnresolvedType)
    val thisAst      = thisAstForMethod(typeFullName, line(constructorDeclaration))

    val lastOrder = 2 + parameterAsts.size

    val bodyAst   = astForMethodBody(Some(constructorDeclaration.getBody), lastOrder)
    val returnAst = astForConstructorReturn(constructorDeclaration)

    val annotationAsts = constructorDeclaration.getAnnotations.asScala.map(astForAnnotationExpr)

    scopeStack.popScope()

    Ast(constructorNode)
      .withChild(thisAst)
      .withChildren(parameterAsts)
      .withChild(bodyAst)
      .withChild(returnAst)
      .withChildren(annotationAsts)
  }

  private def thisAstForMethod(typeFullName: String, lineNumber: Option[Integer]): Ast = {
    val node = NewMethodParameterIn()
      .name("this")
      .lineNumber(lineNumber)
      .code("this")
      .order(0)
      .typeFullName(typeFullName)
      .dynamicTypeHintFullName(Seq(typeFullName))
      .evaluationStrategy(EvaluationStrategies.BY_SHARING)

    Ast(node)
  }

  private def convertAnnotationValueExpr(expr: Expression, order: Int): Option[Ast] = {
    expr match {
      case arrayInit: ArrayInitializerExpr =>
        val arrayInitNode = NewArrayInitializer()
          .code(arrayInit.toString)
          .order(order)
          .argumentIndex(order)
        val initElementAsts = withOrder(arrayInit.getValues) { case (value, order) =>
          convertAnnotationValueExpr(value, order)
        }

        val returnAst = initElementAsts.foldLeft(Ast(arrayInitNode)) {
          case (ast, Some(elementAst)) =>
            ast.withChild(elementAst)
          case (ast, _) => ast
        }
        Some(returnAst)

      case annotationExpr: AnnotationExpr =>
        Some(astForAnnotationExpr(annotationExpr, order))

      case literalExpr: LiteralExpr =>
        Some(astForAnnotationLiteralExpr(literalExpr, order))

      case _ =>
        logger.info(s"convertAnnotationValueExpr not yet implemented for ${expr.getClass}")
        None
    }
  }

  private def astForAnnotationLiteralExpr(literalExpr: LiteralExpr, order: Int): Ast = {
    val valueNode =
      literalExpr match {
        case literal: StringLiteralExpr =>
          NewAnnotationLiteral()
            .code(literal.getValue)
            .name(literal.getValue)
        case literal: IntegerLiteralExpr =>
          NewAnnotationLiteral()
            .code(literal.getValue)
            .name(literal.getValue)
        case literal: BooleanLiteralExpr =>
          NewAnnotationLiteral()
            .code(java.lang.Boolean.toString(literal.getValue))
            .name(java.lang.Boolean.toString(literal.getValue))
        case literal: CharLiteralExpr =>
          NewAnnotationLiteral()
            .code(literal.getValue)
            .name(literal.getValue)
        case literal: DoubleLiteralExpr =>
          NewAnnotationLiteral()
            .code(literal.getValue)
            .name(literal.getValue)
        case literal: LongLiteralExpr =>
          NewAnnotationLiteral()
            .code(literal.getValue)
            .name(literal.getValue)
        case _: NullLiteralExpr =>
          NewAnnotationLiteral()
            .code("null")
            .name("null")
        case literal: TextBlockLiteralExpr =>
          NewAnnotationLiteral()
            .code(literal.getValue)
            .name(literal.getValue)
      }

    Ast(
      valueNode
        .order(order)
        .argumentIndex(order)
    )
  }

  private def createAnnotationAssignmentAst(name: String, value: Expression, code: String, order: Int): Ast = {
    val parameter = NewAnnotationParameter()
      .code(name)
      .order(1)
    val rhs = convertAnnotationValueExpr(value, 2)

    val assign = NewAnnotationParameterAssign()
      .code(code)
      .order(order)

    Ast(assign)
      .withChild(Ast(parameter))
      .withChildren(rhs.toSeq)
  }

  private def expressionReturnTypeFullName(expr: Expression): Option[String] = {
    Try(expr.calculateResolvedType()) match {
      case Success(resolveType) =>
        Some(typeInfoCalc.fullName(resolveType))
      case Failure(_) =>
        expr match {
          case namedExpr: NodeWithName[_] => scopeStack.lookupVariableType(namedExpr.getNameAsString)

          case namedExpr: NodeWithSimpleName[_] => scopeStack.lookupVariableType(namedExpr.getNameAsString)

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
    }
  }

  private def createAnnotationNode(annotationExpr: AnnotationExpr, order: Int): NewAnnotation = {
    NewAnnotation()
      .code(annotationExpr.toString)
      .name(annotationExpr.getName.getIdentifier)
      .fullName(expressionReturnTypeFullName(annotationExpr).getOrElse(TypeConstants.UnresolvedType))
      .order(order)
  }

  private def astForAnnotationExpr(annotationExpr: AnnotationExpr): Ast = {
    astForAnnotationExpr(annotationExpr, -1)
  }

  private def astForAnnotationExpr(annotationExpr: AnnotationExpr, order: Int): Ast = {
    annotationExpr match {
      case _: MarkerAnnotationExpr =>
        Ast(createAnnotationNode(annotationExpr, order))
      case normal: NormalAnnotationExpr =>
        val annotationAst = Ast(createAnnotationNode(annotationExpr, order))
        val assignmentAsts =
          withOrder(normal.getPairs) { case (pair, order) =>
            createAnnotationAssignmentAst(pair.getName.getIdentifier, pair.getValue, pair.toString, order)
          }
        assignmentAsts.foldLeft(annotationAst) { case (ast, assignmentAst) =>
          ast.withChild(assignmentAst)
        }
      case single: SingleMemberAnnotationExpr =>
        val annotationAst = Ast(createAnnotationNode(annotationExpr, order))
        annotationAst.withChild(
          createAnnotationAssignmentAst("value", single.getMemberValue, single.getMemberValue.toString, 1)
        )
    }
  }

  private def getMethodFullName(typeDecl: Option[NewTypeDecl], methodName: String, maybeSignature: Option[String]) = {
    val typeName  = typeDecl.map(_.fullName).getOrElse(TypeConstants.UnresolvedType)
    val signature = maybeSignature.getOrElse(TypeConstants.UnresolvedSignature)

    composeMethodFullName(typeName, methodName, signature)
  }

  private def modifierAstsForMethod(methodDeclaration: MethodDeclaration): Seq[Ast] = {
    val isInterfaceMethod         = scopeStack.getEnclosingTypeDecl.exists(_.code.contains("interface "))
    val isAbstractMethod          = methodDeclaration.isAbstract || (isInterfaceMethod && !methodDeclaration.isDefault)
    val abstractModifier          = Option.when(isAbstractMethod)(NewModifier().modifierType(ModifierTypes.ABSTRACT))
    val staticVirtualModifierType = if (methodDeclaration.isStatic) ModifierTypes.STATIC else ModifierTypes.VIRTUAL
    val staticVirtualModifier     = Some(NewModifier().modifierType(staticVirtualModifierType))
    val accessModifierType = if (methodDeclaration.isPublic) {
      Some(ModifierTypes.PUBLIC)
    } else if (methodDeclaration.isPrivate) {
      Some(ModifierTypes.PRIVATE)
    } else if (isInterfaceMethod) {
      // TODO: more robust interface check
      Some(ModifierTypes.PUBLIC)
    } else {
      None
    }
    val accessModifier = accessModifierType.map(NewModifier().modifierType(_))

    List(accessModifier, abstractModifier, staticVirtualModifier).flatten.map(Ast(_))
  }

  private def astForMethod(methodDeclaration: MethodDeclaration): Ast = {

    val expectedReturnType = Try(
      symbolResolver.toResolvedType(methodDeclaration.getType, classOf[ResolvedType])
    ).toOption
    val expectedReturnTypeName = expectedReturnType.map(typeInfoCalc.fullName)

    scopeStack.pushNewScope(
      MethodScope(ExpectedType(expectedReturnTypeName.getOrElse(TypeConstants.UnresolvedType), expectedReturnType))
    )

    val typeParamMap = getTypeParameterMap(methodDeclaration.getTypeParameters.asScala)
    typeParamMap.foreach { case (identifier, typeParam) =>
      scopeStack.addToScope(typeParam, typeParam.name, typeParam.typeFullName)
    }

    val parameterAsts = astsForParameterList(methodDeclaration.getParameters)

    val returnType =
      expectedReturnTypeName
        // This duplicates some code from TypeInfoCalculator.nameOrFullName, but provides a way to calculate
        // the expected return type above, re-use that here and avoid attempting to resolve unresolvable
        // types twice.
        .orElse(scopeStack.lookupVariableType(methodDeclaration.getTypeAsString))
        .orElse(scopeStack.getWildcardType(methodDeclaration.getTypeAsString))

    val parameterTypes = parameterAsts.map(rootType(_).getOrElse(TypeConstants.UnresolvedType))
    val signature = returnType map { typ =>
      s"$typ(${parameterTypes.mkString(",")})"
    }
    val methodFullName =
      getMethodFullName(scopeStack.getEnclosingTypeDecl, methodDeclaration.getNameAsString, signature)

    val methodNode = createPartialMethod(methodDeclaration)
      .fullName(methodFullName)
      .signature(signature.getOrElse(""))

    val thisAst = if (methodDeclaration.isStatic) {
      Seq()
    } else {
      val typeFullName = scopeStack.getEnclosingTypeDecl.map(_.fullName).getOrElse(TypeConstants.UnresolvedType)
      Seq(thisAstForMethod(typeFullName, line(methodDeclaration)))
    }
    val lastOrder = 1 + parameterAsts.size

    val bodyAst =
      astForMethodBody(methodDeclaration.getBody.toScala, lastOrder)
    val returnAst = astForMethodReturn(methodDeclaration)

    val annotationAsts = methodDeclaration.getAnnotations.asScala.map(astForAnnotationExpr)

    val modifierAsts = modifierAstsForMethod(methodDeclaration)

    scopeStack.popScope()

    Ast(methodNode)
      .withChildren(thisAst)
      .withChildren(parameterAsts)
      .withChild(bodyAst)
      .withChildren(annotationAsts)
      .withChildren(modifierAsts)
      .withChild(returnAst)
  }

  private def astForMethodReturn(methodDeclaration: MethodDeclaration): Ast = {
    val typeFullName = typeInfoCalc.fullName(methodDeclaration.getType).getOrElse(TypeConstants.UnresolvedType)
    Ast(methodReturnNode(typeFullName, None, line(methodDeclaration.getType), column(methodDeclaration.getType)))
  }

  private def astForConstructorReturn(constructorDeclaration: ConstructorDeclaration): Ast = {
    val line   = constructorDeclaration.getEnd.map(x => Integer.valueOf(x.line)).toScala
    val column = constructorDeclaration.getEnd.map(x => Integer.valueOf(x.column)).toScala
    val node   = methodReturnNode(TypeConstants.Void, None, line, column)
    Ast(node)
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

  private def astForMethodBody(body: Option[BlockStmt], order: Int): Ast = {
    body match {
      case Some(b) => astForBlockStatement(b, order)
      case None    => Ast(NewBlock())
    }
  }

  def astsForLabeledStatement(stmt: LabeledStmt, order: Int): Seq[Ast] = {
    val jumpTargetAst = Ast(NewJumpTarget().name(stmt.getLabel.toString).order(order))
    val stmtAst       = astsForStatement(stmt.getStatement, order = order + 1).toList

    jumpTargetAst :: stmtAst
  }

  def astForThrow(stmt: ThrowStmt, order: Int): Ast = {
    val throwNode = NewCall()
      .name("<operator>.throw")
      .methodFullName("<operator>.throw")
      .lineNumber(line(stmt))
      .columnNumber(column(stmt))
      .code(stmt.toString())
      .order(order)
      .argumentIndex(order)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)

    val args = astsForExpression(stmt.getExpression, order = 1, None)

    callAst(throwNode, args)
  }

  def astForCatchClause(catchClause: CatchClause, order: Int): Ast = {
    astForBlockStatement(catchClause.getBody, order)
  }

  def astForTry(stmt: TryStmt, order: Int): Ast = {
    val tryNode = NewControlStructure()
      .controlStructureType(ControlStructureTypes.TRY)
      .code("try")
      .order(order)
      .argumentIndex(order)
      .lineNumber(line(stmt))
      .columnNumber(column(stmt))

    val tryAst = astForBlockStatement(stmt.getTryBlock, 1, "try")
    // Catch order must be 2 for CFG generation
    val catchAsts = withOrder(stmt.getCatchClauses) { (s, o) =>
      astForCatchClause(s, o)
    }
    val catchBlock = Ast(NewBlock().order(2).argumentIndex(2).code("catch"))
      .withChildren(catchAsts)
    // Finally order must be 3 for CFG generation
    val finallyAst =
      stmt.getFinallyBlock.toScala.map(astForBlockStatement(_, 3, "finally")).toList

    Ast(tryNode)
      .withChild(tryAst)
      .withChild(catchBlock)
      .withChildren(finallyAst)
  }

  private def astsForStatement(statement: Statement, order: Int): Seq[Ast] = {
    // TODO: Implement missing handlers
    // case _: LocalClassDeclarationStmt  => Seq()
    // case _: LocalRecordDeclarationStmt => Seq()
    // case _: YieldStmt                  => Seq()
    statement match {
      case x: ExplicitConstructorInvocationStmt =>
        Seq(astForExplicitConstructorInvocation(x, order))
      case x: AssertStmt       => Seq(astForAssertStatement(x, order))
      case x: BlockStmt        => Seq(astForBlockStatement(x, order))
      case x: BreakStmt        => Seq(astForBreakStatement(x, order))
      case x: ContinueStmt     => Seq(astForContinueStatement(x, order))
      case x: DoStmt           => Seq(astForDo(x, order))
      case _: EmptyStmt        => Seq() // Intentionally skipping this
      case x: ExpressionStmt   => astsForExpression(x.getExpression, order, Some(ExpectedType.Void))
      case x: ForEachStmt      => astForForEach(x, order)
      case x: ForStmt          => Seq(astForFor(x, order))
      case x: IfStmt           => Seq(astForIf(x, order))
      case x: LabeledStmt      => astsForLabeledStatement(x, order)
      case x: ReturnStmt       => Seq(astForReturnNode(x, order))
      case x: SwitchStmt       => Seq(astForSwitchStatement(x, order))
      case x: SynchronizedStmt => Seq(astForSynchronizedStatement(x, order))
      case x: ThrowStmt        => Seq(astForThrow(x, order))
      case x: TryStmt          => Seq(astForTry(x, order))
      case x: WhileStmt        => Seq(astForWhile(x, order))
      case x =>
        logger.warn(s"Attempting to generate AST for unknown statement $x")
        Seq(unknownAst(x, order))
    }
  }

  private def astForElse(maybeStmt: Option[Statement]): Option[Ast] = {
    maybeStmt.map { stmt =>
      val elseAsts = astsForStatement(stmt, 1)

      val elseNode =
        NewControlStructure()
          .controlStructureType(ControlStructureTypes.ELSE)
          .order(3)
          .argumentIndex(3)
          .lineNumber(line(stmt))
          .columnNumber(column(stmt))
          .code("else")

      Ast(elseNode).withChildren(elseAsts)
    }
  }

  def astForIf(stmt: IfStmt, order: Int): Ast = {
    val ifNode =
      NewControlStructure()
        .controlStructureType(ControlStructureTypes.IF)
        .order(order)
        .argumentIndex(order)
        .lineNumber(line(stmt))
        .columnNumber(column(stmt))
        .code(s"if (${stmt.getCondition.toString})")

    val conditionAst =
      astsForExpression(stmt.getCondition, order = 1, Some(ExpectedType.Boolean)).headOption.toList

    val thenAsts = astsForStatement(stmt.getThenStmt, order = 2)
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

  def astForWhile(stmt: WhileStmt, order: Int): Ast = {
    val whileNode =
      NewControlStructure()
        .controlStructureType(ControlStructureTypes.WHILE)
        .order(order)
        .argumentIndex(order)
        .lineNumber(line(stmt))
        .columnNumber(column(stmt))
        .code(s"while (${stmt.getCondition.toString})")

    val conditionAst =
      astsForExpression(stmt.getCondition, order = 1, Some(ExpectedType.Boolean)).headOption.toList
    val stmtAsts = astsForStatement(stmt.getBody, order = 2)

    val ast = Ast(whileNode)
      .withChildren(conditionAst)
      .withChildren(stmtAsts)

    conditionAst.flatMap(_.root.toList) match {
      case r :: Nil =>
        ast.withConditionEdge(whileNode, r)
      case _ =>
        ast
    }
  }

  def astForDo(stmt: DoStmt, order: Int): Ast = {
    val doNode =
      NewControlStructure().controlStructureType(ControlStructureTypes.DO).order(order)
    val conditionAst =
      astsForExpression(stmt.getCondition, order = 0, Some(ExpectedType.Boolean)).headOption.toList
    val stmtAsts = astsForStatement(stmt.getBody, order = 1)
    val ast = Ast(doNode)
      .withChildren(conditionAst)
      .withChildren(stmtAsts)

    conditionAst.flatMap(_.root.toList) match {
      case r :: Nil =>
        ast.withConditionEdge(doNode, r)
      case _ =>
        ast
    }
  }

  def astForBreakStatement(stmt: BreakStmt, order: Int): Ast = {
    val node = NewControlStructure()
      .controlStructureType(ControlStructureTypes.BREAK)
      .lineNumber(line(stmt))
      .columnNumber(column(stmt))
      .code(stmt.toString)
      .order(order)
    Ast(node)
  }

  def astForContinueStatement(stmt: ContinueStmt, order: Int): Ast = {
    val node = NewControlStructure()
      .controlStructureType(ControlStructureTypes.CONTINUE)
      .lineNumber(line(stmt))
      .columnNumber(column(stmt))
      .code(stmt.toString)
      .order(order)
    Ast(node)
  }

  private def getForCode(stmt: ForStmt): String = {
    val init    = stmt.getInitialization.asScala.map(_.toString).mkString(", ")
    val compare = stmt.getCompare.toScala.map(_.toString)
    val update  = stmt.getUpdate.asScala.map(_.toString).mkString(", ")
    s"for ($init; $compare; $update)"
  }
  def astForFor(stmt: ForStmt, order: Int): Ast = {
    val forNode =
      NewControlStructure()
        .controlStructureType(ControlStructureTypes.FOR)
        .order(order)
        .argumentIndex(order)
        .code(getForCode(stmt))
        .lineNumber(line(stmt))
        .columnNumber(column(stmt))

    val initAsts =
      withOrder(stmt.getInitialization) { (s, o) =>
        astsForExpression(s, o, None)
      }.flatten

    val compareAsts = stmt.getCompare.toScala.toList.flatMap {
      astsForExpression(_, initAsts.size + 1, Some(ExpectedType.Boolean))
    }

    val newOrder = initAsts.size + compareAsts.size
    val updateAsts = stmt.getUpdate.asScala.toList.flatMap {
      astsForExpression(_, newOrder + 1, None)
    }

    val stmtAsts =
      astsForStatement(stmt.getBody, newOrder + compareAsts.size + 1)

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
    iterableType: String,
    order: Int
  ): (NewLocal, String, String, Seq[Ast]) = {
    val lineNo       = line(iterableExpression)
    val expectedType = Some(ExpectedType(iterableType))

    val iterableAst = astsForExpression(iterableExpression, order = 2, expectedType = expectedType) match {
      case Nil =>
        logger.error(s"Could not create AST for iterable expr $iterableExpression: $filename:l$lineNo")
        Ast()

      case iterableAst :: Nil => iterableAst

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
        .typeFullName(iterableType)
        .order(order)
        .lineNumber(lineNo)
    val iterableLocalAst = Ast(iterableLocalNode)

    val iterableAssignNode = operatorCallNode(
      Operators.assignment,
      code = "",
      order = order + 1,
      line = lineNo,
      typeFullName = Some(iterableType)
    )
    val iterableAssignIdentifier =
      NewIdentifier()
        .name(iterableName)
        .code(iterableName)
        .typeFullName(iterableType)
        .order(1)
        .argumentIndex(1)
        .lineNumber(lineNo)
    val iterableAssignArgs = List(Ast(iterableAssignIdentifier), iterableAst)
    val iterableAssignAst =
      callAst(iterableAssignNode, iterableAssignArgs)
        .withRefEdge(iterableAssignIdentifier, iterableLocalNode)

    (iterableLocalNode, iterableLocalNode.name, iterableLocalNode.typeFullName, List(iterableLocalAst, iterableAssignAst))
  }

  private def nativeForEachIdxLocalNode(lineNo: Option[Integer]): NewLocal = {
    val idxName = nextIndexName()
    val typeFullName = TypeConstants.Int
    val idxLocal =
      NewLocal()
        .name(idxName)
        .typeFullName(typeFullName)
        .code(idxName)
        .lineNumber(lineNo)
        .order(1)
    scopeStack.addToScope(idxLocal, idxName, typeFullName)
    idxLocal
  }

  private def nativeForEachIdxInitializerAst(lineNo: Option[Integer], idxLocal: NewLocal): Ast = {
    val idxName = idxLocal.name
    val idxInitializerCallNode = operatorCallNode(
      Operators.assignment,
      code = s"int $idxName = 0",
      order = 2,
      line = lineNo,
      typeFullName = Some(TypeConstants.Int)
    )
    val idxIdentifierArg = identifierFromNamedVarType(idxName, idxLocal.typeFullName, lineNo, 1)
    val zeroLiteral =
      NewLiteral()
        .code("0")
        .typeFullName(TypeConstants.Int)
        .order(2)
        .argumentIndex(2)
        .lineNumber(lineNo)
    val idxInitializerArgAsts = List(Ast(idxIdentifierArg), Ast(zeroLiteral))
    callAst(idxInitializerCallNode, idxInitializerArgAsts)
      .withRefEdge(idxIdentifierArg, idxLocal)
  }

  private def nativeForEachCompareAst(
    lineNo: Option[Integer],
    iterableLocal: NewNode,
    iterableName: String,
    iterableTypeFullName: String,
    idxLocal: NewLocal
  ): Ast = {
    val idxName      = idxLocal.name

    val compareNode = operatorCallNode(
      Operators.lessThan,
      code = s"$idxName < $iterableName.length",
      order = 3,
      typeFullName = Some(TypeConstants.Boolean),
      line = lineNo
    )
    val comparisonIdxIdentifier = identifierFromNamedVarType(idxName, idxLocal.typeFullName, lineNo, 1)
    val comparisonFieldAccess = operatorCallNode(
      Operators.fieldAccess,
      code = s"$iterableName.length",
      order = 2,
      typeFullName = Some(TypeConstants.Int),
      line = lineNo
    )
    val fieldAccessIdentifier      = identifierFromNamedVarType(iterableName, iterableTypeFullName, lineNo, 1)
    val fieldAccessFieldIdentifier = fieldIdentifierNode("length", lineNo)
    val fieldAccessArgs            = List(fieldAccessIdentifier, fieldAccessFieldIdentifier).map(Ast(_))
    val fieldAccessAst             = callAst(comparisonFieldAccess, fieldAccessArgs)
    val compareArgs                = List(Ast(comparisonIdxIdentifier), fieldAccessAst)

    callAst(compareNode, compareArgs)
      .withRefEdge(comparisonIdxIdentifier, idxLocal)
      .withRefEdge(fieldAccessIdentifier, iterableLocal)
  }

  private def nativeForEachIncrementAst(lineNo: Option[Integer], idxLocal: NewLocal): Ast = {
    val incrementNode = operatorCallNode(
      Operators.postIncrement,
      code = s"${idxLocal.name}++",
      typeFullName = Some(TypeConstants.Int),
      order = 4,
      line = lineNo
    )
    val incrementArg    = identifierFromNamedVarType(idxLocal.name, idxLocal.typeFullName, lineNo, 1)
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

    val partialLocalNode = NewLocal().lineNumber(lineNo).order(1)

    maybeVariable match {
      case Some(variable) =>
        val name = variable.getNameAsString
        val typeFullName = typeInfoCalc.fullName(variable.getType).getOrElse(TypeConstants.UnresolvedType)
        val localNode = partialLocalNode
          .name(name)
          .code(variable.getNameAsString)
          .typeFullName(typeFullName)
        scopeStack.addToScope(localNode, name, typeFullName)
        localNode

      case None =>
        // Returning partialLocalNode here is fine since getting to this case means everything is broken anyways :)
        partialLocalNode
    }
  }

  private def variableAssignForNativeForEachBody(
    variableLocal: NewLocal,
    idxLocal: NewLocal,
    iterableNode: NewNode,
    iterableName: String,
    iterableTypeFullName: String,
  ): Ast = {
    // Everything will be on the same line as the `for` statement, but this is the most useful
    // solution for debugging.
    val lineNo = variableLocal.lineNumber
    val varAssignNode = assignmentNode()
      .order(2)
      .argumentIndex(2)
      .lineNumber(lineNo)
      .typeFullName(variableLocal.typeFullName)

    val targetNode = identifierFromNamedVarType(variableLocal.name, variableLocal.typeFullName, lineNo, 1)

    val indexAccess = indexAccessNode()
      .order(2)
      .argumentIndex(2)
      .lineNumber(lineNo)
      .typeFullName(iterableTypeFullName.replaceAll(raw"\[\]", ""))

    val indexAccessIdentifier = identifierFromNamedVarType(iterableName, iterableTypeFullName, lineNo, 1)
    val indexAccessIndex      = identifierFromNamedVarType(idxLocal.name, idxLocal.typeFullName, lineNo, 2)

    val indexAccessArgsAsts = List(indexAccessIdentifier, indexAccessIndex).map(Ast(_))
    val indexAccessAst      = callAst(indexAccess, indexAccessArgsAsts)

    val assignArgsAsts = List(Ast(targetNode), indexAccessAst)
    callAst(varAssignNode, assignArgsAsts)
      .withRefEdge(targetNode, variableLocal)
      .withRefEdge(indexAccessIdentifier, iterableNode)
      .withRefEdge(indexAccessIndex, idxLocal)
  }

  private def nativeForEachBodyAst(stmt: ForEachStmt, idxLocal: NewLocal, iterableNode: NewNode, iterableName: String, iterableTypeFullName: String): Ast = {
    val variableLocal     = variableLocalForForEachBody(stmt)
    val variableLocalAst  = Ast(variableLocal)
    val variableAssignAst = variableAssignForNativeForEachBody(variableLocal, idxLocal, iterableNode, iterableName, iterableTypeFullName)

    stmt.getBody match {
      case block: BlockStmt =>
        astForBlockStatement(block, order = 5, prefixAsts = List(variableLocalAst, variableAssignAst))

      case stmt =>
        val stmtAsts  = astsForStatement(stmt, 3)
        val blockNode = NewBlock().order(5).lineNumber(variableLocal.lineNumber)
        Ast(blockNode)
          .withChild(variableLocalAst)
          .withChild(variableAssignAst)
          .withChildren(stmtAsts)
    }
  }

  private def identifierFromNamedVarType(
    localName: String,
    localTypeFullName: String,
    lineNumber: Option[Integer],
    order: Int
  ): NewIdentifier = {
    NewIdentifier()
      .name(localName)
      .code(localName)
      .typeFullName(localTypeFullName)
      .lineNumber(lineNumber)
      .order(order)
      .argumentIndex(order)
  }

  private def astsForNativeForEach(stmt: ForEachStmt, iterableType: String, order: Int): Seq[Ast] = {

    // This is ugly, but for a case like `for (int x : new int[] { ... })` this creates a new LOCAL
    // with the assignment `int[] $iterLocal0 = new int[] { ... }` before the FOR loop.
    val (iterableSourceNode, iterableName, iterableTypeFullName, tempIterableInitAsts) = stmt.getIterable match {
      case nameExpr: NameExpr =>
        scopeStack.lookupVariable(nameExpr.getNameAsString) match {
          // If this is not the case, then the code is broken (iterable not in scope).
          case Some(nodeTypeInfo) => (nodeTypeInfo.node, nodeTypeInfo.name, nodeTypeInfo.typeFullName, Nil)
          case _ => iterableAssignAstsForNativeForEach(nameExpr, iterableType, order)
        }
      case iterableExpr => iterableAssignAstsForNativeForEach(iterableExpr, iterableType, order)
    }

    val forNode = NewControlStructure()
      .controlStructureType(ControlStructureTypes.FOR)
      .order(order + tempIterableInitAsts.size)

    val lineNo = line(stmt)

    val idxLocal          = nativeForEachIdxLocalNode(lineNo)
    val idxInitializerAst = nativeForEachIdxInitializerAst(lineNo, idxLocal)
    val compareAst        = nativeForEachCompareAst(lineNo, iterableSourceNode, iterableName, iterableTypeFullName, idxLocal)
    val incrementAst      = nativeForEachIncrementAst(lineNo, idxLocal)
    val bodyAst           = nativeForEachBodyAst(stmt, idxLocal, iterableSourceNode, iterableName, iterableTypeFullName)

    val forAst = Ast(forNode)
      .withChild(Ast(idxLocal))
      .withChild(idxInitializerAst)
      .withChild(compareAst)
      .withChild(incrementAst)
      .withChild(bodyAst)
      .withConditionEdges(forNode, compareAst.root.toList)

    tempIterableInitAsts ++ Seq(forAst)
  }

  private def iteratorLocalForForEach(order: Int, lineNumber: Option[Integer]): NewLocal = {
    val iteratorLocalName = nextIterableName()
    NewLocal()
      .name(iteratorLocalName)
      .code(iteratorLocalName)
      .typeFullName(TypeConstants.Iterator)
      .order(order)
      .lineNumber(lineNumber)
  }

  private def iteratorAssignAstForForEach(
    iterExpr: Expression,
    iteratorLocalNode: NewLocal,
    iterableType: String,
    lineNo: Option[Integer],
    order: Int
  ): Ast = {
    val iteratorAssignNode = operatorCallNode(
      Operators.assignment,
      code = "",
      order = order,
      typeFullName = Some(TypeConstants.Iterator),
      line = lineNo
    )
    val iteratorAssignIdentifier = identifierFromNamedVarType(iteratorLocalNode.name, iteratorLocalNode.typeFullName, lineNo, 1)
    val iteratorMethodName       = "iterator"
    val iteratorMethodSignature  = composeMethodLikeSignature(TypeConstants.Iterator, parameterTypes = Nil)
    val iteratorMethodFullName   = composeMethodFullName(iterableType, iteratorMethodName, iteratorMethodSignature)
    // TODO: This is the only section that needs to be updated for unified native/collection foreach representations.
    val iteratorCallNode =
      NewCall()
        .name(iteratorMethodName)
        .methodFullName(iteratorMethodFullName)
        .signature(iteratorMethodSignature)
        .typeFullName(TypeConstants.Iterator)
        .dispatchType(DispatchTypes.DYNAMIC_DISPATCH)
        .code(iteratorMethodFullName)
        .order(2)
        .argumentIndex(2)
        .lineNumber(lineNo)
    val actualIteratorAst = astsForExpression(iterExpr, 0, expectedType = None) // TODO: Expected type here?
    val iteratorCallAst =
      callAst(iteratorCallNode, actualIteratorAst)
        .withReceiverEdges(iteratorCallNode, actualIteratorAst.headOption.flatMap(_.root).toList)

    callAst(iteratorAssignNode, Seq(Ast(iteratorAssignIdentifier), iteratorCallAst))
      .withRefEdge(iteratorAssignIdentifier, iteratorLocalNode)
  }

  private def hasNextCallAstForForEach(iteratorLocalNode: NewLocal, lineNo: Option[Integer]): Ast = {
    val hasNextCallName      = "hasNext"
    val hasNextCallSignature = composeMethodLikeSignature(TypeConstants.Boolean, parameterTypes = Nil)
    val hasNextCallFullName  = composeMethodFullName(TypeConstants.Iterator, hasNextCallName, hasNextCallSignature)
    val iteratorHasNextCallNode =
      NewCall()
        .name(hasNextCallName)
        .methodFullName(hasNextCallFullName)
        .signature(hasNextCallSignature)
        .typeFullName(TypeConstants.Boolean)
        .dispatchType(DispatchTypes.DYNAMIC_DISPATCH)
        .code(hasNextCallFullName)
        .order(1)
        .argumentIndex(1)
        .lineNumber(lineNo)
    val iteratorHasNextCallReceiver = identifierFromNamedVarType(iteratorLocalNode.name, iteratorLocalNode.typeFullName, lineNo, 1).argumentIndex(0)

    callAst(iteratorHasNextCallNode, Seq(Ast(iteratorHasNextCallReceiver)))
      .withRefEdge(iteratorHasNextCallReceiver, iteratorLocalNode)
      .withReceiverEdge(iteratorHasNextCallNode, iteratorHasNextCallReceiver)
  }

  private def astForIterableForEachItemAssign(iteratorLocalNode: NewLocal, variableLocal: NewLocal): Ast = {
    val lineNo          = variableLocal.lineNumber
    val forVariableType = variableLocal.typeFullName
    val varLocalAssignNode =
      assignmentNode()
        .typeFullName(forVariableType)
        .order(2)
        .argumentIndex(2)
        .lineNumber(lineNo)
    val varLocalAssignIdentifier = identifierFromNamedVarType(variableLocal.name, variableLocal.typeFullName, lineNo, 1)

    val iterNextCallName      = "next"
    val iterNextCallSignature = composeMethodLikeSignature(TypeConstants.Object, parameterTypes = Nil)
    val iterNextCallFullName  = composeMethodFullName(TypeConstants.Iterator, iterNextCallName, iterNextCallSignature)
    val iterNextCallNode =
      NewCall()
        .name(iterNextCallName)
        .methodFullName(iterNextCallFullName)
        .signature(iterNextCallSignature)
        .typeFullName(TypeConstants.Object)
        .dispatchType(DispatchTypes.DYNAMIC_DISPATCH)
        .code(iterNextCallFullName)
        .order(2)
        .argumentIndex(2)
        .lineNumber(lineNo)
    val iterNextCallReceiver = identifierFromNamedVarType(iteratorLocalNode.name, iteratorLocalNode.typeFullName, lineNo, 1).argumentIndex(0)
    val iterNextCallAst =
      callAst(iterNextCallNode, Seq(Ast(iterNextCallReceiver)))
        .withRefEdge(iterNextCallReceiver, iteratorLocalNode)
        .withReceiverEdge(iterNextCallNode, iterNextCallReceiver)

    callAst(varLocalAssignNode, Seq(Ast(varLocalAssignIdentifier), iterNextCallAst))
      .withRefEdge(varLocalAssignIdentifier, variableLocal)
  }

  private def astForIterableForEach(stmt: ForEachStmt, maybeTypeFullName: Option[String], order: Int): Seq[Ast] = {
    val lineNo       = line(stmt)
    val iterableType = maybeTypeFullName.getOrElse(TypeConstants.UnresolvedType)

    val iteratorLocalNode = iteratorLocalForForEach(order, lineNo)
    val iteratorAssignAst =
      iteratorAssignAstForForEach(stmt.getIterable, iteratorLocalNode, iterableType, lineNo, order + 1)
    val iteratorHasNextCallAst = hasNextCallAstForForEach(iteratorLocalNode, lineNo)
    val variableLocal          = variableLocalForForEachBody(stmt)
    val variableAssignAst      = astForIterableForEachItemAssign(iteratorLocalNode, variableLocal)

    val bodyPrefixAsts = Seq(Ast(variableLocal), variableAssignAst)
    val bodyAst = stmt.getBody match {
      case block: BlockStmt =>
        astForBlockStatement(block, 2, prefixAsts = bodyPrefixAsts)

      case bodyStmt =>
        val bodyBlockNode = NewBlock().order(2).argumentIndex(2).lineNumber(lineNo)
        val bodyStmtAsts  = astsForStatement(bodyStmt, bodyPrefixAsts.size + 1)
        Ast(bodyBlockNode)
          .withChildren(bodyPrefixAsts)
          .withChildren(bodyStmtAsts)
    }

    val forNode =
      NewControlStructure()
        .controlStructureType(ControlStructureTypes.WHILE)
        .order(order + 2)
        .argumentIndex(order + 3)
        .code(ControlStructureTypes.FOR)
        .lineNumber(lineNo)
        .columnNumber(column(stmt))

    val forAst = controlStructureAst(forNode, Some(iteratorHasNextCallAst), List(bodyAst))

    Seq(Ast(iteratorLocalNode), iteratorAssignAst, forAst)
  }

  def astForForEach(stmt: ForEachStmt, order: Int): Seq[Ast] = {
    scopeStack.pushNewScope(BlockScope)

    val ast = expressionReturnTypeFullName(stmt.getIterable) match {
      case Some(typeFullName) if typeFullName.endsWith("[]") =>
        astsForNativeForEach(stmt, typeFullName, order)

      case maybeType =>
        astForIterableForEach(stmt, maybeType, order)
    }

    scopeStack.popScope()
    ast
  }

  def astForSwitchStatement(stmt: SwitchStmt, order: Int): Ast = {
    val switchNode =
      NewControlStructure()
        .controlStructureType(ControlStructureTypes.SWITCH)
        .order(order)
        .argumentIndex(order)
        .code(s"switch(${stmt.getSelector.toString})")

    val selectorAsts = astsForExpression(stmt.getSelector, 1, None)
    val selectorNode = selectorAsts.head.root.get

    var orderOffset = 0
    val entryAsts = withOrder(stmt.getEntries) { (e, o) =>
      val asts = astForSwitchEntry(e, o + orderOffset)
      orderOffset += asts.size - 1
      asts
    }.flatten

    val switchBodyAst =
      Ast(NewBlock().order(2).argumentIndex(2)).withChildren(entryAsts)

    Ast(switchNode)
      .withChildren(selectorAsts)
      .withChild(switchBodyAst)
      .withConditionEdge(switchNode, selectorNode)
  }

  private def astForSynchronizedStatement(stmt: SynchronizedStmt, order: Int): Ast = {
    val parentNode =
      NewBlock()
        .lineNumber(line(stmt))
        .columnNumber(column(stmt))
        .order(order)
        .argumentIndex(order)

    val modifier = Ast(NewModifier().modifierType("SYNCHRONIZED"))

    val exprAsts = astsForExpression(stmt.getExpression, 1, None)
    val bodyAst  = astForBlockStatement(stmt.getBody, 1 + exprAsts.size)

    Ast(parentNode)
      .withChild(modifier)
      .withChildren(exprAsts)
      .withChild(bodyAst)
  }

  private def astsForSwitchCases(entry: SwitchEntry, order: Int): Seq[Ast] = {
    entry.getLabels.asScala.toList match {
      case Nil =>
        val target = NewJumpTarget()
          .name("default")
          .code("default")
          .order(order)
          .argumentIndex(order)
        Seq(Ast(target))

      case labels =>
        labels.zipWithIndex.flatMap { case (label, idx) =>
          val labelOrder = order + idx
          val jumpTarget = NewJumpTarget()
            .name("case")
            .code(label.toString)
            .order(labelOrder)
            .argumentIndex(labelOrder)
          val labelAsts = astsForExpression(label, labelOrder, None).toList

          Ast(jumpTarget) :: labelAsts
        }
    }
  }

  def astForSwitchEntry(entry: SwitchEntry, order: Int): Seq[Ast] = {
    val labelAsts = astsForSwitchCases(entry, order)

    val statementOrder = order + entry.getLabels.size
    val statementAsts =
      withOrder(entry.getStatements) { (s, o) =>
        astsForStatement(s, o + statementOrder)
      }.flatten

    labelAsts ++ statementAsts
  }

  private def astForAssertStatement(stmt: AssertStmt, order: Int): Ast = {
    val callNode = NewCall()
      .name("assert")
      .methodFullName("assert")
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .code(stmt.toString)
      .argumentIndex(order)
      .order(order)
      .lineNumber(line(stmt))
      .columnNumber(column(stmt))

    val args = astsForExpression(stmt.getCheck, 1, Some(ExpectedType.Boolean))
    callAst(callNode, args)
  }

  private def astForBlockStatement(
    stmt: BlockStmt,
    order: Int,
    codeStr: String = "<empty>",
    prefixAsts: Seq[Ast] = Seq.empty
  ): Ast = {
    scopeStack.pushNewScope(BlockScope)

    val block = NewBlock()
      .order(order)
      .code(codeStr)
      .argumentIndex(order)
      .lineNumber(line(stmt))
      .columnNumber(column(stmt))

    var orderOffset = prefixAsts.size
    val stmtAsts = withOrder(stmt.getStatements) { (x, o) =>
      val asts = astsForStatement(x, o + orderOffset)
      orderOffset += asts.size - 1
      asts
    }.flatten

    scopeStack.popScope()
    Ast(block)
      .withChildren(prefixAsts)
      .withChildren(stmtAsts)
  }

  private def astForReturnNode(ret: ReturnStmt, order: Int): Ast = {
    val returnNode = NewReturn()
      .lineNumber(line(ret))
      .columnNumber(column(ret))
      .argumentIndex(order)
      .order(order)
      .code(ret.toString)
    if (ret.getExpression.isPresent) {
      val expectedType = scopeStack.getEnclosingMethodReturnType
      val exprAsts     = astsForExpression(ret.getExpression.get(), order + 1, expectedType)
      val returnAst = Ast(returnNode)
        .withChildren(exprAsts)
        .withArgEdges(returnNode, exprAsts.flatMap(_.root))
      returnAst
    } else {
      Ast(returnNode)
    }
  }

  def astForUnaryExpr(expr: UnaryExpr, order: Int, expectedType: Option[ExpectedType]): Ast = {
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

    val argsAsts = astsForExpression(expr.getExpression, 1, expectedType)

    val typeFullName =
      expressionReturnTypeFullName(expr)
        .orElse(argsAsts.headOption.flatMap(rootType))
        .orElse(expectedType.map(_.fullName))
        .getOrElse(TypeConstants.UnresolvedType)

    val callNode = operatorCallNode(
      operatorName,
      code = expr.toString,
      order = order,
      typeFullName = Some(typeFullName),
      line = line(expr),
      column = column(expr)
    )

    callAst(callNode, argsAsts)
  }

  def astForArrayAccessExpr(expr: ArrayAccessExpr, order: Int, expectedType: Option[ExpectedType]): Ast = {
    val typeFullName =
      expressionReturnTypeFullName(expr)
        .orElse(expectedType.map(_.fullName))
        .getOrElse(TypeConstants.UnresolvedType)
    val callNode = operatorCallNode(
      Operators.indexAccess,
      code = expr.toString,
      order = order,
      typeFullName = Some(typeFullName),
      line = line(expr),
      column = column(expr)
    )

    val arrayExpectedType = expectedType.map(exp => ExpectedType(exp.fullName ++ "[]", exp.resolvedType))
    val args = {
      astsForExpression(expr.getName, 1, arrayExpectedType) ++ astsForExpression(
        expr.getIndex,
        2,
        Some(ExpectedType.Int)
      )
    }
    callAst(callNode, args)
  }

  def astForArrayCreationExpr(expr: ArrayCreationExpr, order: Int, expectedType: Option[ExpectedType]): Ast = {
    val name         = Operators.alloc
    val typeFullName = expressionReturnTypeFullName(expr).orElse(expectedType.map(_.fullName))
    val callNode = operatorCallNode(Operators.alloc, code = expr.toString, order = order, typeFullName = typeFullName)

    val levelAsts = expr.getLevels.asScala.zipWithIndex.flatMap { case (lvl, idx) =>
      lvl.getDimension.toScala match {
        case Some(dimension) => astsForExpression(dimension, idx + 1, Some(ExpectedType.Int))

        case None => Seq.empty
      }
    }

    val initializerAst =
      expr.getInitializer.toScala
        .map(astForArrayInitializerExpr(_, levelAsts.size + 1, expectedType))

    val args = (levelAsts ++ initializerAst.toList).toSeq

    callAst(callNode, args)
  }

  def astForArrayInitializerExpr(expr: ArrayInitializerExpr, order: Int, expectedType: Option[ExpectedType]): Ast = {
    val typeFullName =
      expressionReturnTypeFullName(expr)
        .orElse(expectedType.map(_.fullName))
    val callNode = operatorCallNode(
      Operators.arrayInitializer,
      code = expr.toString,
      order = order,
      typeFullName = typeFullName,
      line = line(expr),
      column = column(expr)
    )

    val MAX_INITIALIZERS = 1000

    val expectedValueType = expr.getValues.asScala.headOption.flatMap { value =>
      // typeName and resolvedType may represent different types since typeName can fall
      // back to known information or primitive types. While this certainly isn't ideal,
      // it shouldn't cause issues since resolvedType is only used where the extra type
      // information not available in typeName is necessary.
      val typeName     = expressionReturnTypeFullName(value)
      val resolvedType = Try(value.calculateResolvedType()).toOption
      typeName.map(ExpectedType(_, resolvedType))
    }
    val args = expr.getValues.asScala
      .slice(0, MAX_INITIALIZERS)
      .zipWithIndex
      .flatMap { case (c, o) =>
        astsForExpression(c, o, expectedValueType)
      }
      .toSeq

    val ast = callAst(callNode, args)

    if (expr.getValues.size() > MAX_INITIALIZERS) {
      val placeholder = NewLiteral()
        .typeFullName("ANY")
        .code("<too-many-initializers>")
        .order(MAX_INITIALIZERS)
        .argumentIndex(MAX_INITIALIZERS)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
      ast.withChild(Ast(placeholder)).withArgEdge(callNode, placeholder)
    } else {
      ast
    }
  }

  def astForBinaryExpr(expr: BinaryExpr, order: Int, expectedType: Option[ExpectedType]): Ast = {
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
      astsForExpression(expr.getLeft, 1, expectedType) ++ astsForExpression(expr.getRight, 2, expectedType)

    val typeFullName =
      expressionReturnTypeFullName(expr)
        .orElse(args.headOption.flatMap(rootType))
        .orElse(args.lastOption.flatMap(rootType))
        .orElse(expectedType.map(_.fullName))
        .getOrElse(TypeConstants.UnresolvedType)

    val callNode = operatorCallNode(
      operatorName,
      code = expr.toString,
      order = order,
      typeFullName = Some(typeFullName),
      line = line(expr),
      column = column(expr)
    )

    callAst(callNode, args)
  }

  def astForCastExpr(expr: CastExpr, order: Int, expectedType: Option[ExpectedType]): Ast = {
    val typeFullName =
      typeInfoCalc
        .fullName(expr.getType)
        .orElse(expectedType.map(_.fullName))
        .getOrElse(TypeConstants.UnresolvedType)

    val callNode = operatorCallNode(
      Operators.cast,
      code = expr.toString,
      order = order,
      typeFullName = Some(typeFullName),
      line = line(expr),
      column = column(expr)
    )

    val typeNode = NewTypeRef()
      .code(expr.getType.toString)
      .order(1)
      .argumentIndex(1)
      .typeFullName(typeFullName)
      .lineNumber(line(expr))
      .columnNumber(column(expr))
    val typeAst = Ast(typeNode)

    val exprAst = astsForExpression(expr.getExpression, 2, None)

    callAst(callNode, Seq(typeAst) ++ exprAst)
  }

  def astsForAssignExpr(expr: AssignExpr, order: Int, expectedExprType: Option[ExpectedType]): Seq[Ast] = {
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
      .orElse(expectedExprType) // resolved target type should be more accurate
    val targetAst = astsForExpression(expr.getTarget, 1, expectedType)
    val argsAsts  = astsForExpression(expr.getValue, 2, expectedType)
    val valueType = argsAsts.headOption.flatMap(rootType)

    val typeFullName =
      targetAst.headOption
        .flatMap(rootType)
        .orElse(valueType)
        .orElse(expectedType.map(_.fullName))
        .getOrElse(TypeConstants.UnresolvedType)

    val code = s"${rootCode(targetAst)} ${expr.getOperator.asString} ${rootCode(argsAsts)}"

    val callNode = operatorCallNode(operatorName, code, order, Some(typeFullName), line(expr), column(expr))

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
          val initAst = completeInitForConstructor(partialConstructor, identifier, 2)
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

  private def localsForVarDecl(varDecl: VariableDeclarationExpr, order: Int): List[NewLocal] = {
    varDecl.getVariables.asScala.zipWithIndex.map { case (variable, idx) =>
      val name = variable.getName.toString
      val typeFullName = typeInfoCalc
        .fullName(variable.getType)
        .orElse(scopeStack.lookupVariable(variable.getTypeAsString).map(_.typeFullName)) // TODO: TYPE_CLEANUP
        .getOrElse(TypeConstants.UnresolvedType)
      val code = s"${variable.getType} $name"

      val local = NewLocal().name(name).code(code).typeFullName(typeFullName).order(order + idx)
      local
    }.toList
  }

  private def assignmentsForVarDecl(
    variables: Iterable[VariableDeclarator],
    lineNumber: Option[Integer],
    columnNumber: Option[Integer],
    order: Int
  ): Seq[Ast] = {
    var constructorCount = 0
    val variablesWithInitializers =
      variables.filter(_.getInitializer.toScala.isDefined)
    val assignments = variablesWithInitializers.zipWithIndex flatMap { case (variable, idx) =>
      val name                    = variable.getName.toString
      val initializer             = variable.getInitializer.toScala.get // Won't crash because of filter
      val initializerTypeFullName = variable.getInitializer.toScala.flatMap(expressionReturnTypeFullName)
      val javaParserVarType       = variable.getTypeAsString
      val variableTypeFullName =
        typeInfoCalc
          .fullName(variable.getType)
          .orElse(scopeStack.lookupVariableType(name))
          .orElse(scopeStack.lookupVariableType(javaParserVarType))
          .orElse(scopeStack.getWildcardType(javaParserVarType))

      val typeFullName =
        variableTypeFullName.orElse(initializerTypeFullName).getOrElse(TypeConstants.UnresolvedType)

      // Need the actual resolvedType here for when the RHS is a lambda expression.
      val resolvedExpectedType = Try(symbolResolver.toResolvedType(variable.getType, classOf[ResolvedType])).toOption
      val initializerAsts = astsForExpression(initializer, 2, Some(ExpectedType(typeFullName, resolvedExpectedType)))

      val typeName = TypeNodePass.fullToShortName(typeFullName)
      val code     = s"$typeName $name = ${rootCode(initializerAsts)}"

      val callNode = NewCall()
        .name(Operators.assignment)
        .methodFullName(Operators.assignment)
        .code(code)
        .order(order + idx + constructorCount)
        .argumentIndex(order + idx + constructorCount)
        .lineNumber(lineNumber)
        .columnNumber(columnNumber)
        .typeFullName(typeFullName)
        .dispatchType(DispatchTypes.STATIC_DISPATCH)

      val identifier = NewIdentifier()
        .name(name)
        .order(1)
        .argumentIndex(1)
        .code(name)
        .typeFullName(typeFullName)
        .lineNumber(line(variable))
        .columnNumber(column(variable))
      val localCorrespToIdent = scopeStack.lookupVariable(name).map(_.node)
      val targetAst           = Ast(identifier).withRefEdges(identifier, localCorrespToIdent.toList)

      // Since all partial constructors will be dealt with here, don't pass them up.
      val declAst = callAst(callNode, Seq(targetAst) ++ initializerAsts)

      val constructorAsts = partialConstructorQueue
        .map { partialConstructor =>
          constructorCount += 1
          completeInitForConstructor(partialConstructor, identifier, order + idx + constructorCount)
        }
      partialConstructorQueue.clear()

      Seq(declAst) ++ constructorAsts
    }

    assignments.toList
  }

  private def completeInitForConstructor(
    partialConstructor: PartialConstructor,
    identifier: NewIdentifier,
    order: Int
  ): Ast = {
    val initNode = partialConstructor.initNode
      .order(order)
      .argumentIndex(order)

    val objectNode = identifier.copy
      .order(0)
      .argumentIndex(0)

    val args = partialConstructor.initArgs

    Ast(initNode)
      .withChild(Ast(objectNode))
      .withReceiverEdge(initNode, objectNode)
      .withChildren(args)
      .withArgEdge(initNode, objectNode)
      .withArgEdges(initNode, args.flatMap(_.root))
  }

  def astsForVariableDecl(varDecl: VariableDeclarationExpr, order: Int): Seq[Ast] = {
    val locals    = localsForVarDecl(varDecl, order)
    val localAsts = locals.map { Ast(_) }

    locals.foreach { local =>
      scopeStack.addToScope(local, local.name, local.typeFullName)
    }

    val assignOrder = order + locals.size
    val assignments =
      assignmentsForVarDecl(varDecl.getVariables.asScala, line(varDecl), column(varDecl), assignOrder)

    localAsts ++ assignments
  }

  def callAst(rootNode: NewCall, args: Seq[Ast]): Ast = {
    Ast(rootNode)
      .withChildren(args)
      .withArgEdges(rootNode, args.flatMap(_.root))
  }

  def astForClassExpr(expr: ClassExpr, order: Int): Ast = {
    val callNode = NewCall()
      .name(Operators.fieldAccess)
      .typeFullName(TypeConstants.Class)
      .methodFullName(Operators.fieldAccess)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .code(expr.toString)
      .argumentIndex(order)
      .order(order)

    val identifier = NewIdentifier()
      .typeFullName(typeInfoCalc.fullName(expr.getType).getOrElse(TypeConstants.UnresolvedType))
      .code(expr.getTypeAsString)
      .lineNumber(line(expr))
      .columnNumber(column(expr))
      .argumentIndex(1)
      .order(1)
    val idAst = Ast(identifier)

    val fieldIdentifier = NewFieldIdentifier()
      .canonicalName("class")
      .code("class")
      .lineNumber(line(expr))
      .columnNumber(column(expr))
      .argumentIndex(2)
      .order(2)
    val fieldIdAst = Ast(fieldIdentifier)

    callAst(callNode, Seq(idAst, fieldIdAst))
  }

  def astForConditionalExpr(expr: ConditionalExpr, order: Int, expectedType: Option[ExpectedType]): Ast = {
    val condAst = astsForExpression(expr.getCondition, 1, Some(ExpectedType.Boolean))
    val thenAst = astsForExpression(expr.getThenExpr, 2, expectedType)
    val elseAst = astsForExpression(expr.getElseExpr, 3, expectedType)

    val typeFullName =
      expressionReturnTypeFullName(expr)
        .orElse(thenAst.headOption.flatMap(rootType))
        .orElse(elseAst.headOption.flatMap(rootType))
        .orElse(expectedType.map(_.fullName))
        .getOrElse(TypeConstants.UnresolvedType)

    val callNode = NewCall()
      .name(Operators.conditional)
      .methodFullName(Operators.conditional)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .code(expr.toString)
      .argumentIndex(order)
      .order(order)
      .lineNumber(line(expr))
      .columnNumber(column(expr))
      .typeFullName(typeFullName)

    callAst(callNode, condAst ++ thenAst ++ elseAst)
  }

  def astForEnclosedExpression(expr: EnclosedExpr, order: Int, expectedType: Option[ExpectedType]): Seq[Ast] = {
    astsForExpression(expr.getInner, order, expectedType)
  }

  def astForFieldAccessExpr(expr: FieldAccessExpr, order: Int, expectedType: Option[ExpectedType]): Ast = {
    val typeFullName =
      expressionReturnTypeFullName(expr)
        .orElse(expectedType.map(_.fullName))
        .getOrElse(TypeConstants.UnresolvedType)

    val callNode = NewCall()
      .name(Operators.fieldAccess)
      .methodFullName(Operators.fieldAccess)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .code(expr.toString)
      .argumentIndex(order)
      .order(order)
      .lineNumber(line(expr))
      .columnNumber(column(expr))
      .typeFullName(typeFullName)

    val fieldIdentifier = expr.getName
    val identifierAsts  = astsForExpression(expr.getScope, 1, None)
    val fieldIdentifierNode = NewFieldIdentifier()
      .canonicalName(fieldIdentifier.toString)
      .argumentIndex(2)
      .order(2)
      .lineNumber(line(fieldIdentifier))
      .columnNumber(column(fieldIdentifier))
      .code(fieldIdentifier.toString)
    val fieldIdAst = Ast(fieldIdentifierNode)

    callAst(callNode, identifierAsts ++ Seq(fieldIdAst))
  }

  def astForInstanceOfExpr(expr: InstanceOfExpr, order: Int): Ast = {
    val callNode = NewCall()
      .name(Operators.instanceOf)
      .methodFullName(Operators.instanceOf)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .code(expr.toString)
      .argumentIndex(order)
      .order(order)
      .lineNumber(line(expr))
      .columnNumber(column(expr))
      .typeFullName(TypeConstants.Boolean)

    val exprAst      = astsForExpression(expr.getExpression, order = 1, None)
    val typeFullName = typeInfoCalc.fullName(expr.getType).getOrElse(TypeConstants.UnresolvedType)
    val typeNode =
      NewTypeRef()
        .code(expr.getType.toString)
        .order(exprAst.size + 1)
        .argumentIndex(exprAst.size + 1)
        .lineNumber(line(expr))
        .columnNumber(column(expr.getType))
        .typeFullName(typeFullName)
    val typeAst = Ast(typeNode)

    callAst(callNode, exprAst ++ Seq(typeAst))
  }

  def astForNameExpr(x: NameExpr, order: Int, expectedType: Option[ExpectedType]): Ast = {
    val name = x.getName.toString
    val typeFullName = expressionReturnTypeFullName(x)
      .orElse(expectedType.map(_.fullName))
      .getOrElse(TypeConstants.UnresolvedType)

    Try(x.resolve()) match {
      case Success(value) if value.isField =>
        val identifierName = if (value.asField.isStatic) {
          // A static field represented by a NameExpr must belong to the class in which it's used. Static fields
          // from other classes are represented by a FieldAccessExpr instead.
          scopeStack.getEnclosingTypeDecl.map(_.name).getOrElse(TypeConstants.UnresolvedType)
        } else {
          "this"
        }

        val identifierTypeFullName =
          value match {
            case fieldDecl: ResolvedFieldDeclaration =>
              // TODO It is not quite correct to use the declaring classes type.
              // Instead we should take the using classes type which is either the same or a
              // sub class of the declaring class.
              typeInfoCalc.fullName(fieldDecl.declaringType())
          }

        val identifier = NewIdentifier()
          .name(identifierName)
          .typeFullName(identifierTypeFullName)
          .order(1)
          .argumentIndex(1)
          .lineNumber(line(x))
          .columnNumber(column(x))
          .code(identifierName)

        val fieldIdentifier = NewFieldIdentifier()
          .code(x.toString)
          .canonicalName(name)
          .order(2)
          .argumentIndex(2)
          .lineNumber(line(x))
          .columnNumber(column(x))

        val fieldAccess = NewCall()
          .name(Operators.fieldAccess)
          .methodFullName(Operators.fieldAccess)
          .dispatchType(DispatchTypes.STATIC_DISPATCH)
          .code(name)
          .argumentIndex(order)
          .order(order)
          .typeFullName(typeFullName)
          .lineNumber(line(x))
          .columnNumber(column(x))

        val identifierAst = Ast(identifier)
        val fieldIdentAst = Ast(fieldIdentifier)

        callAst(fieldAccess, Seq(identifierAst, fieldIdentAst))

      case _ =>
        val identifier = NewIdentifier()
          .name(name)
          .order(order)
          .argumentIndex(order)
          .code(name)
          .typeFullName(typeFullName)
          .lineNumber(line(x.getName))
          .columnNumber(column(x.getName))

        val variableOption = scopeStack
          .lookupVariable(name)
          .filter(variableInfo =>
            variableInfo.node.isInstanceOf[NewMethodParameterIn] || variableInfo.node.isInstanceOf[NewLocal]
          )

        variableOption.foldLeft(Ast(identifier))((ast, variableInfo) => ast.withRefEdge(identifier, variableInfo.node))
    }

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
  def astForObjectCreationExpr(expr: ObjectCreationExpr, order: Int, expectedType: Option[ExpectedType]): Ast = {
    val maybeResolvedExpr = Try(expr.resolve())
    val argumentAsts      = argAstsForCall(expr, maybeResolvedExpr, expr.getArguments)

    val allocFullName = Operators.alloc
    val typeFullName = typeInfoCalc
      .fullName(expr.getType)
      .orElse(expectedType.map(_.fullName))
      .getOrElse(TypeConstants.UnresolvedType)

    val signature =
      maybeResolvedExpr match {
        case Success(constructor) =>
          constructorSignature(constructor, ResolvedTypeParametersMap.empty())
        case _ =>
          // Fallback. Method could not be resolved. So we fall back to using
          // expressionTypeFullName and the argument types to approximate the method
          // signature.
          val argumentTypes = argumentAsts.map(arg => rootType(arg).getOrElse(TypeConstants.UnresolvedType))
          composeMethodLikeSignature(TypeConstants.Void, argumentTypes)
      }

    val initFullName = composeMethodFullName(typeFullName, "<init>", signature)

    val allocNode = NewCall()
      .name(allocFullName)
      .methodFullName(allocFullName)
      .code(expr.toString)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .order(order)
      .argumentIndex(order)
      .typeFullName(typeFullName)
      .lineNumber(line(expr))
      .columnNumber(column(expr))
      .signature(s"$typeFullName()")

    val initNode = NewCall()
      .name("<init>")
      .methodFullName(initFullName)
      .lineNumber(line(expr))
      .typeFullName(TypeConstants.Void)
      .code(expr.toString)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .signature(signature)

    // Assume that a block ast is required, since there isn't enough information to decide otherwise.
    // This simplifies logic elsewhere, and unnecessary blocks will be garbage collected soon.
    val blockAst = blockAstForConstructorInvocation(line(expr), column(expr), allocNode, initNode, argumentAsts, order)

    expr.getParentNode.toScala match {
      case Some(parent) if parent.isInstanceOf[VariableDeclarator] || parent.isInstanceOf[AssignExpr] =>
        val partialConstructor = PartialConstructor(initNode, argumentAsts, blockAst)
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
    args: Seq[Ast],
    order: Int
  ): Ast = {
    val blockNode = NewBlock()
      .order(order)
      .argumentIndex(order)
      .lineNumber(lineNumber)
      .columnNumber(columnNumber)
      .typeFullName(allocNode.typeFullName)

    val tempName = "$obj" ++ tempConstCount.toString
    tempConstCount += 1
    val identifier = NewIdentifier()
      .name(tempName)
      .code(tempName)
      .order(1)
      .argumentIndex(1)
      .typeFullName(allocNode.typeFullName)
    val identifierAst = Ast(identifier)

    val allocAst = Ast(allocNode.order(2).argumentIndex(2))

    val assignmentNode = NewCall()
      .name(Operators.assignment)
      .methodFullName(Operators.assignment)
      .typeFullName(allocNode.typeFullName)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .order(1)
      .argumentIndex(1)

    val assignmentAst =
      Ast(assignmentNode)
        .withChild(identifierAst)
        .withChild(allocAst)
        .withArgEdge(assignmentNode, identifierAst.root.get)
        .withArgEdge(assignmentNode, allocAst.root.get)

    val identifierForInit = identifier.copy.order(0).argumentIndex(0)
    val initAst = Ast(
      initNode
        .order(2)
        .argumentIndex(2)
    ).withChild(Ast(identifierForInit))
      .withReceiverEdge(initNode, identifierForInit)
      .withChildren(args)
      .withArgEdge(initNode, identifierForInit)
      .withArgEdges(initNode, args.flatMap(_.root))

    val returnAst = Ast(identifier.copy.order(3).argumentIndex(3))

    Ast(blockNode)
      .withChild(assignmentAst)
      .withChild(initAst)
      .withChild(returnAst)
  }

  def astForThisExpr(expr: ThisExpr, order: Int, expectedType: Option[ExpectedType]): Ast = {
    val typeFullName =
      expressionReturnTypeFullName(expr)
        .orElse(expectedType.map(_.fullName))
        .getOrElse(TypeConstants.UnresolvedType)

    val identifier =
      NewIdentifier()
        .name("this")
        .typeFullName(typeFullName)
        .code(expr.toString)
        .order(order)
        .argumentIndex(order)
        .lineNumber(line(expr))
        .columnNumber(column(expr))

    Ast(identifier)
  }

  private def astForExplicitConstructorInvocation(stmt: ExplicitConstructorInvocationStmt, order: Int): Ast = {
    val maybeResolved = Try(stmt.resolve())
    val args          = argAstsForCall(stmt, maybeResolved, stmt.getArguments)

    val typeFullName = Try(stmt.resolve())
      .map(_.declaringType())
      .map(typeInfoCalc.fullName)
      .getOrElse(TypeConstants.UnresolvedType)
    val argTypes = argumentTypesForCall(Try(stmt.resolve()), args)

    val signature = s"${TypeConstants.Void}(${argTypes.mkString(",")})"
    val callNode = NewCall()
      .name("<init>")
      .methodFullName(composeMethodFullName(typeFullName, "<init>", signature))
      .argumentIndex(order)
      .order(order)
      .code(stmt.toString)
      .lineNumber(line(stmt))
      .columnNumber(column(stmt))
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .signature(signature)
      .typeFullName(TypeConstants.Void)

    val thisNode = NewIdentifier()
      .name("this")
      .code("this")
      .order(0)
      .argumentIndex(0)
      .typeFullName(typeFullName)
    val thisAst = Ast(thisNode)

    val ast = callAst(callNode, Seq(thisAst) ++ args)

    // ast.root should just be `callNode`, but do a sanity check in any case.
    ast.root match {
      case None =>
        logger.warn("Attempting to create constructor invocation without root")
        ast

      case Some(root) =>
        ast.withReceiverEdge(root, thisNode)
    }
  }

  private def astsForExpression(expression: Expression, order: Int, expectedType: Option[ExpectedType]): Seq[Ast] = {
    // TODO: Implement missing handlers
    // case _: MethodReferenceExpr     => Seq()
    // case _: PatternExpr             => Seq()
    // case _: SuperExpr               => Seq()
    // case _: SwitchExpr              => Seq()
    // case _: TypeExpr                => Seq()
    expression match {
      case _: AnnotationExpr          => Seq()
      case x: ArrayAccessExpr         => Seq(astForArrayAccessExpr(x, order, expectedType))
      case x: ArrayCreationExpr       => Seq(astForArrayCreationExpr(x, order, expectedType))
      case x: ArrayInitializerExpr    => Seq(astForArrayInitializerExpr(x, order, expectedType))
      case x: AssignExpr              => astsForAssignExpr(x, order, expectedType)
      case x: BinaryExpr              => Seq(astForBinaryExpr(x, order, expectedType))
      case x: CastExpr                => Seq(astForCastExpr(x, order, expectedType))
      case x: ClassExpr               => Seq(astForClassExpr(x, order))
      case x: ConditionalExpr         => Seq(astForConditionalExpr(x, order, expectedType))
      case x: EnclosedExpr            => astForEnclosedExpression(x, order, expectedType)
      case x: FieldAccessExpr         => Seq(astForFieldAccessExpr(x, order, expectedType))
      case x: InstanceOfExpr          => Seq(astForInstanceOfExpr(x, order))
      case x: LambdaExpr              => Seq(astForLambdaExpr(x, order, expectedType))
      case x: LiteralExpr             => Seq(astForLiteralExpr(x, order))
      case x: MethodCallExpr          => Seq(astForMethodCall(x, order, expectedType))
      case x: NameExpr                => Seq(astForNameExpr(x, order, expectedType))
      case x: ObjectCreationExpr      => Seq(astForObjectCreationExpr(x, order, expectedType))
      case x: SuperExpr               => Seq(astForSuperExpr(x, order, expectedType))
      case x: ThisExpr                => Seq(astForThisExpr(x, order, expectedType))
      case x: UnaryExpr               => Seq(astForUnaryExpr(x, order, expectedType))
      case x: VariableDeclarationExpr => astsForVariableDecl(x, order)
      case x                          => Seq(unknownAst(x, order))
    }
  }

  private def unknownAst(node: Node, order: Int): Ast = {
    val unknownNode =
      NewUnknown()
        .code(node.toString)
        .lineNumber(line(node))
        .columnNumber(column(node))
        .order(order)
        .argumentIndex(order)

    Ast(unknownNode)
  }

  private def codeForScopeExpr(scopeExpr: Expression, isScopeForStaticCall: Boolean): Option[String] = {
    scopeExpr match {
      case scope: NameExpr => Some(s"${scope.getNameAsString}.")

      case fieldAccess: FieldAccessExpr =>
        val maybeScopeString = codeForScopeExpr(fieldAccess.getScope, isScopeForStaticCall = false)
        val name             = fieldAccess.getNameAsString
        maybeScopeString
          .map { scopeString =>
            s"$scopeString$name"
          }
          .orElse(Some(name))
          .map(result => s"${result}.")

      case _: SuperExpr => Some("super.")

      case _: ThisExpr => Some("this.")

      case scopeMethodCall: MethodCallExpr =>
        codePrefixForMethodCall(scopeMethodCall) match {
          case "" => Some("")
          case prefix =>
            val argumentsCode = getArgumentCodeString(scopeMethodCall.getArguments)
            Some(s"$prefix${scopeMethodCall.getNameAsString}($argumentsCode).")
        }

      case objectCreationExpr: ObjectCreationExpr =>
        val typeName        = objectCreationExpr.getTypeAsString
        val argumentsString = getArgumentCodeString(objectCreationExpr.getArguments)
        Some(s"new $typeName($argumentsString).")

      case _ => None
    }
  }

  private def codePrefixForMethodCall(call: MethodCallExpr): String = {
    Try(call.resolve()) match {
      case Success(resolvedCall) =>
        call.getScope.toScala
          .flatMap(codeForScopeExpr(_, resolvedCall.isStatic))
          .getOrElse(if (resolvedCall.isStatic) "" else "this.")

      case _ =>
        // If the call is unresolvable, we cannot make a good guess about what the prefix should be
        ""
    }
  }

  private def createObjectNode(typeFullName: String, call: MethodCallExpr, callNode: NewCall): Option[NewIdentifier] = {
    val maybeScope = call.getScope.toScala

    if (maybeScope.isDefined || callNode.dispatchType == DispatchTypes.DYNAMIC_DISPATCH) {
      val name = maybeScope.map(_.toString).getOrElse("this")
      Some(
        NewIdentifier()
          .name(name)
          .code(name)
          .typeFullName(typeFullName)
          .order(0)
          .argumentIndex(0)
          .lineNumber(callNode.lineNumber)
          .columnNumber(callNode.columnNumber)
      )
    } else {
      None
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

  private def genericParamTypeMapForLambda(expectedType: Option[ExpectedType]): ResolvedTypeParametersMap = {
    expectedType
      .flatMap(_.resolvedType)
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
        val paramsWithSubstitutedTypes = resolvedParameters.map(_.getType).map {
          case resolvedType: ResolvedTypeVariable =>
            expectedTypeParamTypes.getValue(resolvedType.asTypeParameter)

          case resolvedType => resolvedType
        }

        paramsWithSubstitutedTypes.map(typeInfoCalc.fullName)

      case None =>
        // Unless types are explicitly specified in the lambda definition,
        // this will yield the erased types which is why the actual lambda
        // expression parameters are only used as a fallback.
        lambdaParameters
          .map(_.getType)
          .map(typeInfoCalc.fullName)
          .map(_.getOrElse(TypeConstants.UnresolvedType))
    }

    if (paramTypesList.size != lambdaParameters.size) {
      logger.error(s"Found different number lambda params and param types for $expr. Some parameters will be missing.")
    }

    val parameterNodes = lambdaParameters
      .zip(paramTypesList)
      .zipWithIndex
      .map { case ((param, typ), idx) =>
        val name = param.getNameAsString
        NewMethodParameterIn()
          .name(name)
          .typeFullName(typ)
          .order(idx + 1)
          .code(s"$typ $name")
          .evaluationStrategy(EvaluationStrategies.BY_SHARING)
          .lineNumber(line(expr))
      }

    parameterNodes.foreach { paramNode =>
      scopeStack.addToScope(paramNode, paramNode.name, paramNode.typeFullName)
    }

    parameterNodes.map(Ast(_))
  }

  private def getLambdaReturnType(
    maybeResolvedLambdaType: Option[ResolvedType],
    maybeBoundMethod: Option[ResolvedMethodDeclaration],
    expectedTypeParamTypes: ResolvedTypeParametersMap
  ): String = {
    val maybeBoundMethodReturnType = maybeBoundMethod.map { boundMethod =>
      boundMethod.getReturnType match {
        case returnType: ResolvedTypeVariable =>
          expectedTypeParamTypes.getValue(returnType.asTypeParameter)

        case returnType => returnType
      }
    }

    val returnType = maybeBoundMethodReturnType.orElse(maybeResolvedLambdaType)
    returnType.map(typeInfoCalc.fullName).getOrElse(TypeConstants.UnresolvedType)
  }

  def closureBinding(closureBindingId: String, originalName: String): NewClosureBinding = {
    NewClosureBinding()
      .closureBindingId(closureBindingId)
      .closureOriginalName(originalName)
      .evaluationStrategy(EvaluationStrategies.BY_SHARING)
  }

  private def closureBindingsForCapturedNodes(
    captured: List[NamedVariableNodeType],
    lambdaMethodName: String
  ): List[ClosureBindingEntry] = {
    captured.map { capturedNode =>
      val closureBindingId   = s"$lambdaMethodName:${capturedNode.name}"
      val closureBindingNode = closureBinding(closureBindingId, capturedNode.name)
      ClosureBindingEntry(capturedNode, closureBindingNode)
    }
  }

  private def localsForCapturedNodes(closureBindingEntries: List[ClosureBindingEntry]): List[NewLocal] = {
    val localsForCaptured =
      closureBindingEntries.map { case ClosureBindingEntry(node, binding) =>
        val local = NewLocal()
          .name(node.name)
          .code(node.name)
          .typeFullName(node.typeFullName)
          .closureBindingId(binding.closureBindingId)
        local
      }
    localsForCaptured.foreach { local => scopeStack.addToScope(local, local.name, local.typeFullName) }
    localsForCaptured
  }

  private def astForLambdaBody(
    body: Statement,
    localsForCapturedVars: Seq[NewLocal],
    returnType: String,
    order: Int
  ): Ast = {
    body match {
      case block: BlockStmt => astForBlockStatement(block, order, prefixAsts = localsForCapturedVars.map(Ast(_)))

      case stmt =>
        val returnOrder = localsForCapturedVars.size + 1
        val blockAst    = Ast(NewBlock().order(order).argumentIndex(order).lineNumber(line(body)))
        val bodyAst = if (returnType == TypeConstants.Void) {
          astsForStatement(stmt, returnOrder)
        } else {
          val returnAst = Ast(
            NewReturn()
              .order(returnOrder)
              .argumentIndex(returnOrder)
              .code(s"return ${body.toString}")
              .lineNumber(line(body))
          )
          val argumentAst = astsForStatement(stmt, 1)
          Seq(returnAst.withChildren(argumentAst))
        }

        blockAst
          .withChildren(localsForCapturedVars.map(Ast(_)))
          .withChildren(bodyAst)
    }
  }

  private def createLambdaMethodNode(lambdaName: String, parameters: Seq[Ast], returnType: String): NewMethod = {
    val enclosingTypeName =
      scopeStack.getEnclosingTypeDecl.map(_.fullName).getOrElse(TypeConstants.UnresolvedType)
    val signature =
      s"$returnType(${parameters.map(rootType).map(_.getOrElse(TypeConstants.UnresolvedType)).mkString(",")})"
    val lambdaFullName = composeMethodFullName(enclosingTypeName, lambdaName, signature)

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
    bindingEntries.foreach { case ClosureBindingEntry(node, closureBinding) =>
      diffGraph.addNode(closureBinding)
      diffGraph.addEdge(closureBinding, node, EdgeTypes.REF)
      diffGraph.addEdge(methodRef, closureBinding, EdgeTypes.CAPTURE)
    }
  }

  private def createAndPushLambdaMethod(
    expr: LambdaExpr,
    lambdaMethodName: String,
    implementedInfo: LambdaImplementedInfo,
    localsForCaptured: Seq[NewLocal],
    expectedLambdaType: Option[ExpectedType]
  ): NewMethod = {
    val implementedMethod    = implementedInfo.implementedMethod
    val implementedInterface = implementedInfo.implementedInterface

    // We need to get this information from the expected type as the JavaParser
    // symbol solver returns the erased types when resolving the lambda itself.
    val expectedTypeParamTypes = genericParamTypeMapForLambda(expectedLambdaType)
    val parametersWithoutThis  = buildParamListForLambda(expr, implementedMethod, expectedTypeParamTypes)

    val returnType = getLambdaReturnType(implementedInterface, implementedMethod, expectedTypeParamTypes)

    val lambdaMethodBody = astForLambdaBody(expr.getBody, localsForCaptured, returnType, parametersWithoutThis.size + 1)

    val thisParam = lambdaMethodBody.nodes
      .collect { case identifier: NewIdentifier => identifier }
      .find { identifier => identifier.name == "this" || identifier.name == "super" }
      .map { _ =>
        val typeFullName = scopeStack.getEnclosingTypeDecl.map(_.fullName).getOrElse(TypeConstants.UnresolvedType)
        thisAstForMethod(typeFullName, line(expr))
      }
      .toList

    val parameters = thisParam ++ parametersWithoutThis

    val lambdaMethodNode = createLambdaMethodNode(lambdaMethodName, parametersWithoutThis, returnType)
    val returnNode       = methodReturnNode(returnType, None, line(expr), column(expr))
    val virtualModifier  = Some(NewModifier().modifierType(ModifierTypes.VIRTUAL))
    val staticModifier   = Option.when(thisParam.isEmpty)(NewModifier().modifierType(ModifierTypes.STATIC))
    val privateModifier  = Some(NewModifier().modifierType(ModifierTypes.PRIVATE))

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
        .withChildren(modifiers)
        .withChildren(parameters)
        .withChild(lambdaMethodBody)
        .withChild(Ast(returnNode))

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
        .map(typeInfoCalc.fullName)
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

  private def getLambdaImplementedInfo(expr: LambdaExpr, expectedType: Option[ExpectedType]): LambdaImplementedInfo = {
    val maybeImplementedType = {
      val maybeResolved = Try(expr.calculateResolvedType())
      maybeResolved.toOption
        .orElse(expectedType.flatMap(_.resolvedType))
        .collect { case refType: ResolvedReferenceType => refType }
    }

    val maybeImplementedInterface = maybeImplementedType.flatMap(_.getTypeDeclaration.toScala)

    if (maybeImplementedInterface.isEmpty) {
      logger.warn(s"Could not resolve the interface implemented by the lambda $expr. Type info may be missing.")
    }

    // By definition, a functional interface will declare exactly one abstract method, so `find` is fine.
    val maybeBoundMethod = maybeImplementedInterface.flatMap(_.getDeclaredMethods.asScala.find(_.isAbstract))

    LambdaImplementedInfo(maybeImplementedType, maybeBoundMethod)
  }

  private def astForLambdaExpr(expr: LambdaExpr, order: Int, expectedType: Option[ExpectedType]): Ast = {
    scopeStack.pushNewScope(MethodScope(expectedType.getOrElse(ExpectedType.default)))

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
        .order(order)
        .argumentIndex(order)

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

  private def astForLiteralExpr(expr: LiteralExpr, order: Int = 1): Ast = {
    Ast(
      NewLiteral()
        .order(order)
        .argumentIndex(order)
        .code(expr.toString)
        .typeFullName(expressionReturnTypeFullName(expr).getOrElse(TypeConstants.UnresolvedType))
        .lineNumber(line(expr))
        .columnNumber(column(expr))
    )
  }

  private def getExpectedParamType(
    maybeResolvedCall: Try[ResolvedMethodLikeDeclaration],
    idx: Int
  ): Option[ExpectedType] = {
    maybeResolvedCall.toOption.map { methodDecl =>
      val paramCount = methodDecl.getNumberOfParams

      val resolvedType = if (idx < paramCount) {
        Some(methodDecl.getParam(idx).getType)
      } else if (paramCount > 0 && methodDecl.getParam(paramCount - 1).isVariadic) {
        Some(methodDecl.getParam(paramCount - 1).getType)
      } else {
        None
      }

      val typeName = resolvedType.map(typeInfoCalc.fullName)
      ExpectedType(typeName.getOrElse(TypeConstants.UnresolvedType), resolvedType)
    }
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
    callExpr.getScope.toScala match {
      case Some(scope: ThisExpr) =>
        expressionReturnTypeFullName(scope)
          .orElse(scopeStack.getEnclosingTypeDecl.map(_.fullName))

      case Some(scope: SuperExpr) =>
        expressionReturnTypeFullName(scope)
          .orElse(scopeStack.getEnclosingTypeDecl.flatMap(_.inheritsFromTypeFullName.headOption))

      case Some(scope) => expressionReturnTypeFullName(scope)

      case None =>
        Try(callExpr.resolve()).toOption.flatMap { methodDeclOption =>
          if (methodDeclOption.isStatic) {
            Some(typeInfoCalc.fullName(methodDeclOption.declaringType()))
          } else {
            scopeStack.getEnclosingTypeDecl.map(_.fullName)
          }
        } orElse (scopeStack.getEnclosingTypeDecl.map(_.fullName))
    }
  }

  private def argumentTypesForCall(maybeMethod: Try[ResolvedMethodLikeDeclaration], argAsts: Seq[Ast]): List[String] = {
    maybeMethod match {
      case Success(resolved) =>
        (0 until resolved.getNumberOfParams).map { idx =>
          val param = resolved.getParam(idx)
          typeInfoCalc.fullName(param.getType)
        }.toList

      case Failure(_) =>
        // Fall back to actual argument types if the called method couldn't be resolved.
        // This may result in missing dataflows.
        argAsts.map(arg => rootType(arg).getOrElse(TypeConstants.UnresolvedType)).toList
    }
  }

  private def argAstsForCall(
    call: Node,
    tryResolvedDecl: Try[ResolvedMethodLikeDeclaration],
    args: NodeList[Expression]
  ): Seq[Ast] = {
    val hasVariadicParameter = tryResolvedDecl.map(_.hasVariadicParameter).getOrElse(false)
    val paramCount           = tryResolvedDecl.map(_.getNumberOfParams).getOrElse(-1)

    val argsAsts = withOrder(args) { (arg, o) =>
      val expectedType = getExpectedParamType(tryResolvedDecl, o - 1)
      // Calculate order, taking into account varargs which will be children to a new arrayInitializer node.
      val order = if (hasVariadicParameter && (o >= paramCount)) o - paramCount + 1 else o
      astsForExpression(arg, order, expectedType)
    }.flatten

    tryResolvedDecl match {
      case Success(_) if hasVariadicParameter =>
        val expectedVariadicTypeFullName =
          getExpectedParamType(tryResolvedDecl, paramCount - 1)
            .map(_.fullName)
            .getOrElse(TypeConstants.UnresolvedType)
        val (regularArgs, varargs) = argsAsts.splitAt(paramCount - 1)
        val arrayInitializer =
          NewCall()
            .name(Operators.arrayInitializer)
            .methodFullName(Operators.arrayInitializer)
            .code(Operators.arrayInitializer)
            .typeFullName(expectedVariadicTypeFullName)
            .order(paramCount)
            .argumentIndex(paramCount)
            .dispatchType(DispatchTypes.STATIC_DISPATCH)
            .lineNumber(line(call))
            .columnNumber(column(call))

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

  private def astForMethodCall(call: MethodCallExpr, order: Int = 1, expectedReturnType: Option[ExpectedType]): Ast = {
    val maybeResolvedCall = Try(call.resolve())
    val argumentAsts      = argAstsForCall(call, maybeResolvedCall, call.getArguments)

    val expressionTypeFullName = expressionReturnTypeFullName(call)
      .orElse(expectedReturnType.map(_.fullName))
      .getOrElse(TypeConstants.UnresolvedType)

    val signature =
      maybeResolvedCall match {
        case Success(method) =>
          methodSignature(method, ResolvedTypeParametersMap.empty())
        case _ =>
          // Fallback. Method could not be resolved. So we fall back to using
          // expressionTypeFullName and the argument types to approximate the method
          // signature.
          val argumentTypes = argumentAsts.map(arg => rootType(arg).getOrElse(TypeConstants.UnresolvedType))
          composeMethodLikeSignature(expressionTypeFullName, argumentTypes)
      }

    val receiverTypeOption = targetTypeForCall(call)
    val receiverType       = receiverTypeOption.getOrElse(TypeConstants.UnresolvedReceiver)

    val methodFullName = composeMethodFullName(receiverType, call.getNameAsString, signature)

    val dispatchType = dispatchTypeForCall(maybeResolvedCall, call.getScope.toScala)

    val argumentsCode = getArgumentCodeString(call.getArguments)

    val codePrefix = codePrefixForMethodCall(call)
    val callNode = NewCall()
      .typeFullName(expressionTypeFullName)
      .name(call.getNameAsString)
      .methodFullName(methodFullName)
      .signature(signature)
      .dispatchType(dispatchType)
      .code(s"$codePrefix${call.getNameAsString}($argumentsCode)")
      .order(order)
      .argumentIndex(order)
      .lineNumber(line(call))
      .columnNumber(column(call))

    val scopeAsts = call.getScope.toScala match {
      case Some(scope) =>
        astsForExpression(scope, 0, receiverTypeOption.map(ExpectedType(_)))

      case None =>
        val objectNode =
          createObjectNode(receiverTypeOption.getOrElse(TypeConstants.UnresolvedType), call, callNode)
        objectNode.map(Ast(_)).toList
    }

    val ast = callAst(callNode, scopeAsts ++ argumentAsts)

    scopeAsts.headOption.flatMap(_.root) match {
      case None => ast

      case Some(rootNode) =>
        ast.withReceiverEdge(callNode, rootNode)
    }
  }

  def astForSuperExpr(superExpr: SuperExpr, order: Int, expectedType: Option[ExpectedType]): Ast = {
    val typeFullName =
      expressionReturnTypeFullName(superExpr)
        .orElse(expectedType.map(_.fullName))
        .getOrElse(TypeConstants.UnresolvedType)

    val thisIdentifier = NewIdentifier()
      .name("this")
      .code("super")
      .typeFullName(typeFullName)
      .order(order)
      .argumentIndex(order)
      .lineNumber(line(superExpr))
      .columnNumber(column(superExpr))

    Ast(thisIdentifier)
  }

  private def astsForParameterList(parameters: NodeList[Parameter], order: Int = 0): Seq[Ast] = {
    withOrder(parameters) { (p, o) =>
      astForParameter(p, order + o)
    }
  }

  private def astForParameter(parameter: Parameter, childNum: Int): Ast = {
    val maybeArraySuffix = if (parameter.isVarArgs) "[]" else ""
    val typeFullName = {
      typeInfoCalc
        .fullName(parameter.getType)
        .orElse(scopeStack.lookupVariableType(parameter.getTypeAsString))
        .orElse(scopeStack.getWildcardType(parameter.getTypeAsString))
        .getOrElse(TypeConstants.UnresolvedType)
    }
    val parameterNode = NewMethodParameterIn()
      .name(parameter.getName.toString)
      .code(parameter.toString)
      .typeFullName(s"$typeFullName$maybeArraySuffix")
      .order(childNum)
      .lineNumber(line(parameter))
      .columnNumber(column(parameter))
      .evaluationStrategy(EvaluationStrategies.BY_SHARING)
    val annotationAsts = parameter.getAnnotations.asScala.map(astForAnnotationExpr)
    val ast            = Ast(parameterNode)

    scopeStack.addToScope(parameterNode, parameter.getNameAsString, parameterNode.typeFullName)
    ast.withChildren(annotationAsts)
  }

  private def constructorFullName(typeDecl: Option[NewTypeDecl], signature: String): String = {
    val typeName = typeDecl.map(_.fullName).getOrElse(TypeConstants.UnresolvedType)
    s"$typeName.<init>:$signature"
  }

}

object AstCreator {
  def line(node: Node): Option[Integer] = {
    node.getBegin.map(x => Integer.valueOf(x.line)).toScala
  }

  def column(node: Node): Option[Integer] = {
    node.getBegin.map(x => Integer.valueOf(x.column)).toScala
  }

  def withOrder[T <: Node, X](nodeList: java.util.List[T])(f: (T, Int) => X): Seq[X] = {
    nodeList.asScala.zipWithIndex.map { case (x, i) =>
      f(x, i + 1)
    }.toSeq
  }
}
