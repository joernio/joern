package io.joern.javasrc2cpg.passes

import com.github.javaparser.ast.`type`.TypeParameter
import com.github.javaparser.ast.{CompilationUnit, Node, NodeList, PackageDeclaration}
import com.github.javaparser.ast.body.{
  BodyDeclaration,
  CallableDeclaration,
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
  ResolvedConstructorDeclaration,
  ResolvedMethodDeclaration,
  ResolvedMethodLikeDeclaration,
  ResolvedReferenceTypeDeclaration
}
import com.github.javaparser.symbolsolver.javaparsermodel.declarations.JavaParserFieldDeclaration
import io.joern.javasrc2cpg.passes.AstWithCtx.astWithCtxToSeq
import io.joern.javasrc2cpg.passes.Context.mergedCtx
import io.joern.javasrc2cpg.util.Scope.WildcardImportName
import io.joern.javasrc2cpg.util.{NodeTypeInfo, Scope, TypeInfoProvider}
import io.joern.javasrc2cpg.util.TypeInfoProvider.{TypeConstants, UnresolvedTypeDefault}
import io.shiftleft.codepropertygraph.generated.{
  ControlStructureTypes,
  DispatchTypes,
  EdgeTypes,
  EvaluationStrategies,
  ModifierTypes,
  Operators,
  PropertyNames
}
import io.shiftleft.codepropertygraph.generated.nodes.{
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
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate.DiffGraphBuilder

import java.util.UUID.randomUUID
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters.RichOptional
import scala.language.{existentials, implicitConversions}
import scala.util.{Failure, Success, Try}

case class BindingInfo(node: NewBinding, edgeMeta: Seq[(NewNode, NewNode, String)])
case class ClosureBindingInfo(identifier: NewIdentifier, closure: NewClosureBinding, bindingId: String)
case class ClosureBindingMeta(node: NewClosureBinding, edgeMeta: Seq[(NewNode, NewNode, String)])

case class PartialConstructor(initNode: NewCall, initArgs: Seq[AstWithCtx], blockAst: AstWithCtx)

case class Context(
  locals: Seq[NewLocal] = List(),
  identifiers: Map[String, NewIdentifier] = Map.empty,
  methodParameters: Seq[NewMethodParameterIn] = List(),
  bindingsInfo: Seq[BindingInfo] = List(),
  lambdaAsts: Seq[Ast] = List(),
  closureBindingInfo: Seq[ClosureBindingMeta] = List(),
  partialConstructors: Seq[PartialConstructor] = List(),
  staticInitializers: Seq[Ast] = List()
) {
  def ++(other: Context): Context = {
    val newLocals           = locals ++ other.locals
    val newIdentifiers      = identifiers ++ other.identifiers
    val newParameters       = methodParameters ++ other.methodParameters
    val newBindings         = bindingsInfo ++ other.bindingsInfo
    val newLambdas          = lambdaAsts ++ other.lambdaAsts
    val newClosureBindings  = closureBindingInfo ++ other.closureBindingInfo
    val newConstructorInits = partialConstructors ++ other.partialConstructors
    val newStaticInits      = staticInitializers ++ other.staticInitializers

    Context(
      newLocals,
      newIdentifiers,
      newParameters,
      newBindings,
      newLambdas,
      newClosureBindings,
      newConstructorInits,
      newStaticInits
    )
  }

  def addBindings(bindings: Seq[BindingInfo]): Context = {
    this.copy(bindingsInfo = this.bindingsInfo ++ bindings)
  }

  def mergeWith(others: Iterable[Context]): Context = {
    Context.mergedCtx(Seq(this) ++ others)
  }

  def clearConstructors(): Context = {
    this.copy(partialConstructors = Seq.empty)
  }
}

object Context {
  def mergedCtx(ctxs: Seq[Context]): Context = {
    ctxs.foldLeft(Context())((acc, ctx) => { acc ++ ctx })
  }
}

case class RefEdgePair(from: NewIdentifier, to: NewMethodParameterIn)

case class AstWithCtx(ast: Ast, ctx: Context)

case class AstWithStaticInit(astWithCtx: Seq[AstWithCtx], staticInits: Seq[AstWithCtx])

object AstWithStaticInit {
  val empty: AstWithStaticInit = AstWithStaticInit(Seq.empty, Seq.empty)

  def apply(astWithCtx: AstWithCtx): AstWithStaticInit = {
    AstWithStaticInit(Seq(astWithCtx), staticInits = Seq.empty)
  }
}

object AstWithCtx {
  val empty: AstWithCtx = AstWithCtx(Ast(), Context())

  implicit def astWithCtxToSeq(astWithCtx: AstWithCtx): Seq[AstWithCtx] = {
    Seq(astWithCtx)
  }
}

/** Translate a Java Parser AST into a CPG AST
  */
class AstCreator(filename: String, javaParserAst: CompilationUnit, global: Global) extends AstCreatorBase(filename) {

  private val logger = LoggerFactory.getLogger(this.getClass)
  import AstCreator._

  private val scopeStack                         = Scope()
  private val typeInfoProvider: TypeInfoProvider = TypeInfoProvider(global, scopeStack)

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
  def storeInDiffGraph(astWithCtx: AstWithCtx): Unit = {
    val ast = astWithCtx.ast
    Ast.storeInDiffGraph(ast, diffGraph)

    astWithCtx.ctx.bindingsInfo.foreach { bindingInfo =>
      diffGraph.addNode(bindingInfo.node)

      bindingInfo.edgeMeta.foreach { case (src, dst, label) =>
        diffGraph.addEdge(src, dst, label)
      }
    }

    astWithCtx.ctx.closureBindingInfo.foreach { closureBindingInfo =>
      diffGraph.addNode(closureBindingInfo.node)

      closureBindingInfo.edgeMeta.foreach { case (src, dest, label) =>
        diffGraph.addEdge(src, dest, label)
      }
    }
  }

  private def addImportsToScope(compilationUnit: CompilationUnit): Unit = {
    val (asteriskImports, specificImports) = compilationUnit.getImports.asScala.toList.partition(_.isAsterisk)
    specificImports.foreach { importStmt =>
      val name = importStmt.getName.getIdentifier
      val importNode =
        NewIdentifier()
          .name(name)
          .typeFullName(importStmt.getNameAsString) // fully qualified name
      scopeStack.addToScope(name, NodeTypeInfo(importNode))
    }

    asteriskImports match {
      case imp :: Nil =>
        val importNode = NewIdentifier().name(WildcardImportName).typeFullName(imp.getNameAsString)
        scopeStack.addToScope(WildcardImportName, importNode)

      case _ => // Only try to guess a wildcard import if exactly one is defined
    }
  }

  /** Translate compilation unit into AST
    */
  private def astForTranslationUnit(compilationUnit: CompilationUnit): AstWithCtx = {

    try {
      val AstWithCtx(ast, ctx) = astForPackageDeclaration(compilationUnit.getPackageDeclaration.toScala)

      val namespaceBlockFullName = {
        ast.root.collect { case x: NewNamespaceBlock => x.fullName }.getOrElse("none")
      }
      val namespaceScopeNode = {
        ast.root.collect { case x: NewNamespaceBlock => x }.getOrElse(NewUnknown())
      }

      scopeStack.pushNewScope(namespaceScopeNode)
      addImportsToScope(compilationUnit)

      val typeDeclAstsWithCtx = withOrder(compilationUnit.getTypes) { (typ, order) =>
        astForTypeDecl(typ, order, astParentType = "NAMESPACE_BLOCK", astParentFullName = namespaceBlockFullName)
      }

      val typeDeclAsts = typeDeclAstsWithCtx.map(_.ast)
      val mergedCtx    = ctx.mergeWith(typeDeclAstsWithCtx.map(_.ctx))

      val lambdaTypeDeclAsts = mergedCtx.lambdaAsts.map { lambdaAst =>
        val root = lambdaAst.root.get.asInstanceOf[NewMethod]
        // TODO: Inherit from implemented interface and bind to implemented method
        val lambdaTypeDecl = NewTypeDecl()
          .name(root.name)
          .fullName(root.fullName)
        Ast(lambdaTypeDecl).withChild(lambdaAst)
      }

      scopeStack.popScope()
      AstWithCtx(ast.withChildren(typeDeclAsts).withChildren(lambdaTypeDeclAsts), mergedCtx)
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
  private def astForPackageDeclaration(packageDecl: Option[PackageDeclaration]): AstWithCtx = {

    val namespaceBlock = packageDecl match {
      case Some(decl) =>
        val packageName = decl.getName.toString
        val name        = packageName.split("\\.").lastOption.getOrElse("")
        NewNamespaceBlock()
          .name(name)
          .fullName(packageName)
      case None =>
        globalNamespaceBlock()
    }
    AstWithCtx(Ast(namespaceBlock.filename(absolutePath(filename)).order(1)), Context())
  }

  private def bindingForMethod(maybeMethodNode: Option[NewMethod]): List[BindingInfo] = {
    maybeMethodNode match {
      case Some(methodNode) =>
        scopeStack.getEnclosingTypeDecl match {
          case Some(typeDecl) =>
            val node = NewBinding()
              .name(methodNode.name)
              .methodFullName(methodNode.fullName)
              .signature(methodNode.signature)

            BindingInfo(node, List((typeDecl, node, EdgeTypes.BINDS), (node, methodNode, EdgeTypes.REF))) :: Nil

          case None => Nil
        }

      case None => Nil
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
        val AstWithCtx(ast, ctx) = astForConstructor(constructor, order)
        val rootNode             = Try(ast.root.get.asInstanceOf[NewMethod]).toOption
        val bindingInfo          = bindingForMethod(rootNode)
        AstWithStaticInit(AstWithCtx(ast, ctx.addBindings(bindingInfo)))

      case method: MethodDeclaration =>
        val AstWithCtx(ast, ctx) = astForMethod(method, order)
        val rootNode             = Try(ast.root.get.asInstanceOf[NewMethod]).toOption
        val bindingInfo          = bindingForMethod(rootNode)
        AstWithStaticInit(AstWithCtx(ast, ctx.addBindings(bindingInfo)))

      case typeDeclaration: TypeDeclaration[_] =>
        AstWithStaticInit(astForTypeDecl(typeDeclaration, order, "TYPE_DECL", astParentFullName))

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
        val stmts = initDeclaration.getBody.getStatements.asScala
        val asts = withOrderAndCtx(stmts, clinitOrder) { case (stmt, order) =>
          astsForStatement(stmt, order)
        }
        AstWithStaticInit(astWithCtx = Seq.empty, staticInits = asts)

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
          typeInfoProvider.getTypeFullName(bound)
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
              typeInfoProvider
                .getResolvedTypeFullName(upperBound)
                .getOrElse(TypeConstants.Object)
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

  private def clinitAstsFromStaticInits(staticInits: Seq[AstWithCtx], order: Int): Option[Ast] = {
    if (staticInits.isEmpty) {
      None
    } else {
      // TODO: Get rid of magic strings
      val signature = "void()"
      val fullName = scopeStack.getEnclosingTypeDecl
        .map { typeDecl =>
          s"${typeDecl.fullName}.<clinit>:$signature"
        }
        .getOrElse("")

      val methodNode = NewMethod()
        .name("<clinit>")
        .fullName(fullName)
        .signature("void()")
        .order(order)

      val staticModifier = NewModifier()
        .modifierType(ModifierTypes.STATIC)
        .code(ModifierTypes.STATIC)
        .order(-1)

      val body = Ast(NewBlock().order(1)).withChildren(staticInits.map(_.ast))

      val methodReturn = methodReturnNode(None, None, "void")

      val methodAst =
        Ast(methodNode)
          .withChild(Ast(staticModifier))
          .withChild(body)
          .withChild(Ast(methodReturn))

      Some(methodAst)
    }
  }

  private def astForTypeDecl(
    typ: TypeDeclaration[_],
    order: Int,
    astParentType: String,
    astParentFullName: String
  ): AstWithCtx = {
    val baseTypeFullNames = if (typ.isClassOrInterfaceDeclaration) {
      val decl             = typ.asClassOrInterfaceDeclaration()
      val extendedTypes    = decl.getExtendedTypes.asScala
      val implementedTypes = decl.getImplementedTypes.asScala
      // For some reason, `typ.resolve().isInterface` returns `false` for interfaces,
      // so they now extend `Object` as well since checking for interfaces becomes
      // rather difficult.
      val maybeJavaObjectType = if (extendedTypes.isEmpty) {
        typeInfoProvider.registerType("java.lang.Object")
        Seq("java.lang.Object")
      } else {
        Seq()
      }
      maybeJavaObjectType ++ (extendedTypes ++ implementedTypes)
        .map(typ => typeInfoProvider.getTypeFullName(typ).getOrElse(UnresolvedTypeDefault))
        .toList
    } else {
      List.empty[String]
    }

    val typeFullName = typeInfoProvider.getTypeDeclType(typ)
    val name         = typeInfoProvider.getTypeDeclType(typ, fullName = false)

    val typeDecl = NewTypeDecl()
      .name(name)
      .fullName(typeFullName)
      .lineNumber(line(typ))
      .columnNumber(column(typ))
      .inheritsFromTypeFullName(baseTypeFullNames)
      .order(order)
      .filename(filename)
      .code(typ.getNameAsString)
      .astParentType(astParentType)
      .astParentFullName(astParentFullName)

    scopeStack.pushNewScope(typeDecl)

    val typeParameterMap = getTypeParameterMap(Try(typ.resolve()))
    typeParameterMap.foreach { case (identifier, node) =>
      scopeStack.addToScope(identifier, node)
    }

    val enumEntryAsts = if (typ.isEnumDeclaration) {
      withOrder(typ.asEnumDeclaration().getEntries) { case (entry, order) =>
        astForEnumEntry(entry, order)
      }
    } else {
      List.empty
    }

    var clinitOrder                             = 1
    val staticInits: mutable.Buffer[AstWithCtx] = mutable.Buffer()
    val memberAsts = withOrderAndCtx(typ.getMembers.asScala, initialOrder = enumEntryAsts.size) { (member, idx) =>
      val astWithInits =
        astForTypeDeclMember(member, order + idx, clinitOrder, astParentFullName = typeFullName)
      clinitOrder += astWithInits.staticInits.size
      staticInits.appendAll(astWithInits.staticInits)
      astWithInits.astWithCtx
    }

    val defaultConstructorAst = if (typ.getConstructors.isEmpty) {
      val order = memberAsts.size + 1
      Some(astForDefaultConstructor(order))
    } else {
      None
    }

    val annotationAsts = typ.getAnnotations.asScala.map(astForAnnotationExpr)

    val clinitAst =
      clinitAstsFromStaticInits(staticInits.toSeq, memberAsts.size + defaultConstructorAst.size + 1)

    val typeDeclAst = Ast(typeDecl)
      .withChildren(enumEntryAsts)
      .withChildren(memberAsts.map(_.ast))
      .withChildren(defaultConstructorAst.map(_.ast).toList)
      .withChildren(annotationAsts)
      .withChildren(clinitAst.toSeq)

    val typeDeclContext = Context.mergedCtx((memberAsts ++ defaultConstructorAst.toList).map(_.ctx))

    scopeStack.popScope()

    AstWithCtx(typeDeclAst, typeDeclContext)
  }

  private def astForDefaultConstructor(order: Int): AstWithCtx = {
    val typeFullName = scopeStack.getEnclosingTypeDecl.map(_.fullName).getOrElse("<empty>")
    val constructorNode = NewMethod()
      .name("<init>")
      .fullName(s"$typeFullName.<init>:void()")
      .signature("void()")
      .order(order)
      .filename(filename)
      .isExternal(false)

    val thisAst = thisAstForMethod(typeFullName, lineNumber = None)
    val bodyAst = Ast(NewBlock().order(1).argumentIndex(1))

    val returnNode = methodReturnNode(None, None, "void")
    val returnAst  = Ast(returnNode)

    val modifiers = List(
      Ast(NewModifier().modifierType(ModifierTypes.CONSTRUCTOR)),
      Ast(NewModifier().modifierType(ModifierTypes.PUBLIC))
    )

    val bindingsInfo = bindingForMethod(Some(constructorNode))

    val ast = Ast(constructorNode)
      .withChildren(modifiers)
      .withChild(thisAst.ast)
      .withChild(bodyAst)
      .withChild(returnAst)

    val ctx = Context(bindingsInfo = bindingsInfo)

    AstWithCtx(ast, ctx)
  }

  private def astForEnumEntry(entry: EnumConstantDeclaration, order: Int): Ast = {
    val typeFullName = typeInfoProvider.getTypeFullName(entry).getOrElse(UnresolvedTypeDefault)
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
    }.flatten

    Ast(entryNode)
      .withChildren(args.map(_.ast))
  }

  private def astForFieldVariable(v: VariableDeclarator, fieldDeclaration: FieldDeclaration, order: Int): AstWithCtx = {
    // TODO: Should be able to find expected type here
    val annotations = fieldDeclaration.getAnnotations
    val typeFullName =
      typeInfoProvider
        .getTypeFullName(v)
        .orElse(scopeStack.getWildcardType(v.getTypeAsString))
        .getOrElse(UnresolvedTypeDefault)
    val name = v.getName.toString
    val memberNode =
      NewMember()
        .name(name)
        .typeFullName(typeFullName)
        .order(order)
        .code(s"$typeFullName $name")
    val memberAst      = Ast(memberNode)
    val annotationAsts = annotations.asScala.map(astForAnnotationExpr)

    val staticModifier = if (fieldDeclaration.isStatic) {
      Some(NewModifier().modifierType(ModifierTypes.STATIC).code(ModifierTypes.STATIC).order(-1))
    } else {
      None
    }

    val accessModifier = if (fieldDeclaration.isPublic) {
      Some(NewModifier().modifierType(ModifierTypes.PUBLIC).code(ModifierTypes.PUBLIC).order(-1))
    } else if (fieldDeclaration.isPrivate) {
      Some(NewModifier().modifierType(ModifierTypes.PRIVATE).code(ModifierTypes.PRIVATE).order(-1))
    } else if (fieldDeclaration.isProtected) {
      Some(NewModifier().modifierType(ModifierTypes.PROTECTED).code(ModifierTypes.PROTECTED).order(-1))
    } else {
      None
    }

    val memberAstWithModifiers =
      memberAst
        .withChildren(annotationAsts)
        .withChildren(staticModifier.map(Ast(_)).toSeq)
        .withChildren(accessModifier.map(Ast(_)).toSeq)

    val nodeTypeInfo = NodeTypeInfo(memberNode, isField = true, isStatic = fieldDeclaration.isStatic)
    scopeStack.addToScope(name, nodeTypeInfo)
    AstWithCtx(memberAstWithModifiers, Context())
  }

  private def astForConstructor(constructorDeclaration: ConstructorDeclaration, childNum: Int): AstWithCtx = {
    scopeStack.pushNewScope(NewMethod())

    val parameterAstsWithCtx = astsForParameterList(constructorDeclaration.getParameters)
    val parameterTypes       = parameterAstsWithCtx.map(rootType(_).getOrElse(UnresolvedTypeDefault))
    val signature            = s"void(${parameterTypes.mkString(",")})"
    val fullName             = constructorFullName(scopeStack.getEnclosingTypeDecl, signature)

    val constructorNode = createPartialMethod(constructorDeclaration)
      .fullName(fullName)
      .signature(signature)

    parameterAstsWithCtx.foreach { case AstWithCtx(ast, ctx) =>
      ast.root match {
        case Some(p: NewMethodParameterIn) => scopeStack.addToScope(p.name, p)
        case _                             => // This should never happen
      }
    }

    val typeFullName = scopeStack.getEnclosingTypeDecl.map(_.fullName).getOrElse(UnresolvedTypeDefault)
    val thisAst      = thisAstForMethod(typeFullName, line(constructorDeclaration))

    val lastOrder = 2 + parameterAstsWithCtx.size

    val bodyAstWithCtx =
      astForMethodBody(Some(constructorDeclaration.getBody), lastOrder)
    val returnAstWithCtx = astForConstructorReturn(constructorDeclaration)

    val annotationAsts = constructorDeclaration.getAnnotations.asScala.map(astForAnnotationExpr)

    val constructorAst = Ast(constructorNode)
      .withChild(thisAst.ast)
      .withChildren(parameterAstsWithCtx.map(_.ast))
      .withChild(bodyAstWithCtx.ast)
      .withChild(returnAstWithCtx)
      .withChildren(annotationAsts)

    val ctx = bodyAstWithCtx.ctx.mergeWith(Seq(thisAst.ctx) ++ parameterAstsWithCtx.map(_.ctx))

    scopeStack.popScope()
    AstWithCtx(constructorAst, ctx)
  }

  private def thisAstForMethod(typeFullName: String, lineNumber: Option[Integer]): AstWithCtx = {
    val node = NewMethodParameterIn()
      .name("this")
      .lineNumber(lineNumber)
      .code("this")
      .order(0)
      .typeFullName(typeFullName)
      .dynamicTypeHintFullName(Seq(typeFullName))
      .evaluationStrategy(EvaluationStrategies.BY_SHARING)

    AstWithCtx(Ast(node), Context(methodParameters = Seq(node)))
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

  private def createAnnotationNode(annotationExpr: AnnotationExpr, order: Int): NewAnnotation = {
    NewAnnotation()
      .code(annotationExpr.toString)
      .name(annotationExpr.getName.getIdentifier)
      .fullName(typeInfoProvider.getTypeForExpression(annotationExpr).getOrElse(UnresolvedTypeDefault))
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

  private def getMethodFullName(
    typeDecl: Option[NewTypeDecl],
    methodDeclaration: MethodDeclaration,
    signature: Option[String]
  ) = {
    val typeName   = typeDecl.map(_.fullName)
    val methodName = methodDeclaration.getNameAsString

    (typeName, signature) match {
      case (Some(typ), Some(sig)) =>
        s"$typ.$methodName:$sig"

      case _ => ""
    }
  }

  private def astForMethod(methodDeclaration: MethodDeclaration, childNum: Int): AstWithCtx = {

    scopeStack.pushNewScope(NewMethod())

    val typeParamMap = getTypeParameterMap(methodDeclaration.getTypeParameters.asScala)
    typeParamMap.foreach { case (identifier, typeParam) =>
      scopeStack.addToScope(identifier, typeParam)
    }

    val parameterAstsWithCtx = astsForParameterList(methodDeclaration.getParameters)

    val returnType =
      typeInfoProvider
        .getReturnType(methodDeclaration)
        .orElse(scopeStack.lookupVariableType(methodDeclaration.getTypeAsString))
        .orElse(scopeStack.getWildcardType(methodDeclaration.getTypeAsString))

    val parameterTypes = parameterAstsWithCtx.map(rootType(_).getOrElse(UnresolvedTypeDefault))
    val signature = returnType map { typ =>
      s"$typ(${parameterTypes.mkString(",")})"
    }
    val methodFullName = getMethodFullName(scopeStack.getEnclosingTypeDecl, methodDeclaration, signature)

    val methodNode = createPartialMethod(methodDeclaration)
      .fullName(methodFullName)
      .signature(signature.getOrElse(""))

    val thisAst = if (methodDeclaration.isStatic) {
      Seq()
    } else {
      val typeFullName = scopeStack.getEnclosingTypeDecl.map(_.fullName).getOrElse(UnresolvedTypeDefault)
      Seq(thisAstForMethod(typeFullName, line(methodDeclaration)))
    }
    val lastOrder = 1 + parameterAstsWithCtx.size

    val bodyAstWithCtx =
      astForMethodBody(methodDeclaration.getBody.toScala, lastOrder)
    val returnAstWithCtx = astForMethodReturn(methodDeclaration)

    val annotationAsts = methodDeclaration.getAnnotations.asScala.map(astForAnnotationExpr)

    val modifiers =
      if (!methodDeclaration.isStatic) {
        Seq(Ast(NewModifier()
          .modifierType(ModifierTypes.VIRTUAL)
          .code(ModifierTypes.VIRTUAL)))
      } else {
        Seq()
      }

    val ast = Ast(methodNode)
      .withChildren(thisAst.map(_.ast))
      .withChildren(parameterAstsWithCtx.map(_.ast))
      .withChild(bodyAstWithCtx.ast)
      .withChildren(annotationAsts)
      .withChildren(modifiers)
      .withChild(returnAstWithCtx)

    val ctx = bodyAstWithCtx.ctx.mergeWith(parameterAstsWithCtx.map(_.ctx))

    scopeStack.popScope()
    AstWithCtx(ast, ctx)
  }

  private def astForMethodReturn(methodDeclaration: MethodDeclaration): Ast = {
    val typeFullName = typeInfoProvider.getReturnType(methodDeclaration).getOrElse(UnresolvedTypeDefault)
    Ast(methodReturnNode(line(methodDeclaration.getType), column(methodDeclaration.getType), typeFullName))
  }

  private def astForConstructorReturn(constructorDeclaration: ConstructorDeclaration): Ast = {
    val line   = constructorDeclaration.getEnd.map(x => Integer.valueOf(x.line)).toScala
    val column = constructorDeclaration.getEnd.map(x => Integer.valueOf(x.column)).toScala
    val node   = methodReturnNode(line, column, "void")
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

  private def astForMethodBody(body: Option[BlockStmt], order: Int): AstWithCtx = {
    body match {
      case Some(b) => astForBlockStatement(b, order)
      case None =>
        val blockNode = NewBlock()
        AstWithCtx(Ast(blockNode), Context())
    }
  }

  def astsForLabeledStatement(stmt: LabeledStmt, order: Int): Seq[AstWithCtx] = {
    val jumpTargetAst  = Ast(NewJumpTarget().name(stmt.getLabel.toString).order(order))
    val stmtAstWithCtx = astsForStatement(stmt.getStatement, order = order + 1)

    Seq(AstWithCtx(jumpTargetAst, Context())) ++ stmtAstWithCtx
  }

  def astForThrow(stmt: ThrowStmt, order: Int): AstWithCtx = {
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

  def astForCatchClause(catchClause: CatchClause, order: Int): AstWithCtx = {
    astForBlockStatement(catchClause.getBody, order)
  }

  def astForTry(stmt: TryStmt, order: Int): AstWithCtx = {
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
      .withChildren(catchAsts.map(_.ast))
    // Finally order must be 3 for CFG generation
    val finallyAst =
      stmt.getFinallyBlock.toScala.map(astForBlockStatement(_, 3, "finally")).toList

    val ast = Ast(tryNode)
      .withChild(tryAst.ast)
      .withChild(catchBlock)
      .withChildren(finallyAst.map(_.ast))

    val ctx = tryAst.ctx.mergeWith(catchAsts.map(_.ctx)).mergeWith(finallyAst.map(_.ctx))

    AstWithCtx(ast, ctx)
  }

  private def astsForStatement(statement: Statement, order: Int): Seq[AstWithCtx] = {
    // TODO: Implement missing handlers
    // case _: LocalClassDeclarationStmt  => Seq()
    // case _: LocalRecordDeclarationStmt => Seq()
    // case _: UnparsableStmt             => Seq() // TODO: log a warning
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
      case x: ExpressionStmt   => astsForExpression(x.getExpression, order, Some("void"))
      case x: ForEachStmt      => Seq(astForForEach(x, order))
      case x: ForStmt          => Seq(astForFor(x, order))
      case x: IfStmt           => Seq(astForIf(x, order))
      case x: LabeledStmt      => astsForLabeledStatement(x, order)
      case x: ReturnStmt       => Seq(astForReturnNode(x, order))
      case x: SwitchStmt       => Seq(astForSwitchStatement(x, order))
      case x: SynchronizedStmt => Seq(astForSynchronizedStatement(x, order))
      case x: ThrowStmt        => Seq(astForThrow(x, order))
      case x: TryStmt          => Seq(astForTry(x, order))
      case x: WhileStmt        => Seq(astForWhile(x, order))
      case x                   => Seq(unknownAst(x, order))
    }
  }

  private def astForElse(maybeStmt: Option[Statement]): Option[AstWithCtx] = {
    maybeStmt.map { stmt =>
      val elseAstsWithCtx = astsForStatement(stmt, 1)

      val elseNode =
        NewControlStructure()
          .controlStructureType(ControlStructureTypes.ELSE)
          .order(3)
          .argumentIndex(3)
          .lineNumber(line(stmt))
          .columnNumber(column(stmt))
          .code("else")

      AstWithCtx(Ast(elseNode).withChildren(elseAstsWithCtx.map(_.ast)), mergedCtx(elseAstsWithCtx.map(_.ctx)))
    }
  }

  def astForIf(stmt: IfStmt, order: Int): AstWithCtx = {
    val ifNode =
      NewControlStructure()
        .controlStructureType(ControlStructureTypes.IF)
        .order(order)
        .argumentIndex(order)
        .lineNumber(line(stmt))
        .columnNumber(column(stmt))
        .code(s"if (${stmt.getCondition.toString})")

    val conditionAstWithCtx =
      astsForExpression(stmt.getCondition, order = 1, Some("boolean")).headOption
        .getOrElse(AstWithCtx.empty)

    val thenAstsWithCtx = astsForStatement(stmt.getThenStmt, order = 2)
    val elseAstWithCtx  = astForElse(stmt.getElseStmt.toScala)

    val ast = Ast(ifNode)
      .withChild(conditionAstWithCtx.ast)
      .withChildren(thenAstsWithCtx.map(_.ast))
      .withChildren(elseAstWithCtx.map(_.ast).toList)

    val ifAst = conditionAstWithCtx.ast.root match {
      case Some(r) =>
        ast.withConditionEdge(ifNode, r)
      case None =>
        ast
    }
    val ctx =
      conditionAstWithCtx.ctx
        .mergeWith(thenAstsWithCtx.map(_.ctx))
        .mergeWith(elseAstWithCtx.map(_.ctx))

    AstWithCtx(ifAst, ctx)
  }

  def astForWhile(stmt: WhileStmt, order: Int): AstWithCtx = {
    val whileNode =
      NewControlStructure()
        .controlStructureType(ControlStructureTypes.WHILE)
        .order(order)
        .argumentIndex(order)
        .lineNumber(line(stmt))
        .columnNumber(column(stmt))
        .code(s"while (${stmt.getCondition.toString})")

    val conditionAstWithCtx =
      astsForExpression(stmt.getCondition, order = 1, Some("boolean")).headOption
        .getOrElse(AstWithCtx.empty)
    val stmtAstsWithCtx = astsForStatement(stmt.getBody, order = 2)

    val ast = Ast(whileNode)
      .withChild(conditionAstWithCtx.ast)
      .withChildren(stmtAstsWithCtx.map(_.ast))

    val whileAst = conditionAstWithCtx.ast.root match {
      case Some(r) =>
        ast.withConditionEdge(whileNode, r)
      case None =>
        ast
    }
    val ctx = conditionAstWithCtx.ctx.mergeWith(stmtAstsWithCtx.map(_.ctx))

    AstWithCtx(whileAst, ctx)
  }

  def astForDo(stmt: DoStmt, order: Int): AstWithCtx = {
    val doNode =
      NewControlStructure().controlStructureType(ControlStructureTypes.DO).order(order)
    val conditionAstWithCtx =
      astsForExpression(stmt.getCondition, order = 0, Some("boolean")).headOption
        .getOrElse(AstWithCtx.empty)
    val stmtAstsWithCtx = astsForStatement(stmt.getBody, order = 1)
    val ast = Ast(doNode)
      .withChild(conditionAstWithCtx.ast)
      .withChildren(stmtAstsWithCtx.map(_.ast))

    val doAst = conditionAstWithCtx.ast.root match {
      case Some(r) =>
        ast.withConditionEdge(doNode, r)
      case None =>
        ast
    }
    val ctx = conditionAstWithCtx.ctx.mergeWith(stmtAstsWithCtx.map(_.ctx))

    AstWithCtx(doAst, ctx)
  }

  def astForBreakStatement(stmt: BreakStmt, order: Int): AstWithCtx = {
    val node = NewControlStructure()
      .controlStructureType(ControlStructureTypes.BREAK)
      .lineNumber(line(stmt))
      .columnNumber(column(stmt))
      .code(stmt.toString)
      .order(order)
    AstWithCtx(Ast(node), Context())
  }

  def astForContinueStatement(stmt: ContinueStmt, order: Int): AstWithCtx = {
    val node = NewControlStructure()
      .controlStructureType(ControlStructureTypes.CONTINUE)
      .lineNumber(line(stmt))
      .columnNumber(column(stmt))
      .code(stmt.toString)
      .order(order)
    AstWithCtx(Ast(node), Context())
  }

  private def getForCode(stmt: ForStmt): String = {
    val init    = stmt.getInitialization.asScala.map(_.toString).mkString(", ")
    val compare = stmt.getCompare.toScala.map(_.toString)
    val update  = stmt.getUpdate.asScala.map(_.toString).mkString(", ")
    s"for ($init; $compare; $update)"
  }
  def astForFor(stmt: ForStmt, order: Int): AstWithCtx = {
    val forNode =
      NewControlStructure()
        .controlStructureType(ControlStructureTypes.FOR)
        .order(order)
        .argumentIndex(order)
        .code(getForCode(stmt))
        .lineNumber(line(stmt))
        .columnNumber(column(stmt))

    val initAstsWithCtx =
      withOrderAndCtx(stmt.getInitialization.asScala) { (s, o) =>
        astsForExpression(s, o, None)
      }

    val compareAstsWithCtx =
      withOrderAndCtx(stmt.getCompare.toScala, initAstsWithCtx.size + 1) { (x, o) =>
        astsForExpression(x, o, Some("boolean"))
      }

    val newOrder = initAstsWithCtx.size + compareAstsWithCtx.size
    val updateAstsWithCtx =
      withOrderAndCtx(stmt.getUpdate.asScala, newOrder + 1) { (x, o) =>
        astsForExpression(x, o, None)
      }

    val stmtAstsWithCtx =
      astsForStatement(stmt.getBody, newOrder + compareAstsWithCtx.size + 1)

    val ast = Ast(forNode)
      .withChildren(initAstsWithCtx.map(_.ast))
      .withChildren(compareAstsWithCtx.map(_.ast))
      .withChildren(updateAstsWithCtx.map(_.ast))
      .withChildren(stmtAstsWithCtx.map(_.ast))

    val forAst = compareAstsWithCtx.flatMap(_.ast.root) match {
      case c :: Nil =>
        ast.withConditionEdge(forNode, c)
      case _ => ast
    }
    val ctx = mergedCtx((initAstsWithCtx ++ compareAstsWithCtx ++ updateAstsWithCtx ++ stmtAstsWithCtx).map(_.ctx))

    AstWithCtx(forAst, ctx)
  }

  def astForForEach(stmt: ForEachStmt, order: Int): AstWithCtx = {
    val forNode = NewControlStructure()
      .controlStructureType(ControlStructureTypes.FOR)
      .order(order)

    val iterableAstsWithCtx = astsForExpression(stmt.getIterable, 1, None)
    val variableAstsWithCtx =
      astsForVariableDecl(stmt.getVariable, iterableAstsWithCtx.size + 1)
    val initContext = mergedCtx((iterableAstsWithCtx ++ variableAstsWithCtx).map(_.ctx))

    val bodyOrder       = iterableAstsWithCtx.size + variableAstsWithCtx.size + 1
    val bodyAstsWithCtx = astsForStatement(stmt.getBody, bodyOrder)

    val forEachAst = Ast(forNode)
      .withChildren(iterableAstsWithCtx.map(_.ast))
      .withChildren(variableAstsWithCtx.map(_.ast))
      .withChildren(bodyAstsWithCtx.map(_.ast))
    val ctx = mergedCtx((iterableAstsWithCtx ++ variableAstsWithCtx ++ bodyAstsWithCtx).map(_.ctx))

    AstWithCtx(forEachAst, ctx)
  }

  def astForSwitchStatement(stmt: SwitchStmt, order: Int): AstWithCtx = {
    val switchNode =
      NewControlStructure()
        .controlStructureType(ControlStructureTypes.SWITCH)
        .order(order)
        .argumentIndex(order)
        .code(s"switch(${stmt.getSelector.toString})")

    val selectorAstsWithCtx = astsForExpression(stmt.getSelector, 1, None)
    val selectorNode        = selectorAstsWithCtx.head.ast.root.get

    val entryAstsWithCtx = withOrderAndCtx(stmt.getEntries.asScala) { (e, o) =>
      astForSwitchEntry(e, o)
    }

    val switchBodyAst =
      Ast(NewBlock().order(2).argumentIndex(2)).withChildren(entryAstsWithCtx.map(_.ast))

    val switchAst =
      Ast(switchNode)
        .withChildren(selectorAstsWithCtx.map(_.ast))
        .withChild(switchBodyAst)
        .withConditionEdge(switchNode, selectorNode)
    val ctx = mergedCtx(entryAstsWithCtx.map(_.ctx))

    AstWithCtx(switchAst, ctx)
  }

  private def astForSynchronizedStatement(stmt: SynchronizedStmt, order: Int): AstWithCtx = {
    val parentNode =
      NewBlock()
        .lineNumber(line(stmt))
        .columnNumber(column(stmt))
        .order(order)
        .argumentIndex(order)

    val modifier = Ast(NewModifier().modifierType("SYNCHRONIZED"))

    val exprAsts = astsForExpression(stmt.getExpression, 1, None)
    val bodyAst  = astForBlockStatement(stmt.getBody, 1 + exprAsts.size)

    val ctx = bodyAst.ctx.mergeWith(exprAsts.map(_.ctx))
    val ast = Ast(parentNode)
      .withChild(modifier)
      .withChildren(exprAsts.map(_.ast))
      .withChild(bodyAst.ast)

    AstWithCtx(ast, ctx)
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
          val labelAsts = astsForExpression(label, labelOrder, None)

          Seq(Ast(jumpTarget)) ++ labelAsts.map(_.ast)
        }
    }
  }

  def astForSwitchEntry(entry: SwitchEntry, order: Int): Seq[AstWithCtx] = {
    val labelAsts = astsForSwitchCases(entry, order)

    val statementOrder = order + entry.getLabels.size
    val statementAstsWithCtx =
      withOrderAndCtx(entry.getStatements.asScala, statementOrder) { (s, o) =>
        astsForStatement(s, o)
      }

    val labelAstsWithCtx = labelAsts.map(AstWithCtx(_, Context()))
    labelAstsWithCtx ++ statementAstsWithCtx
  }

  private def astForAssertStatement(stmt: AssertStmt, order: Int): AstWithCtx = {
    val callNode = NewCall()
      .name("assert")
      .methodFullName("assert")
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .code(stmt.toString)
      .argumentIndex(order)
      .order(order)
      .lineNumber(line(stmt))
      .columnNumber(column(stmt))

    val args = astsForExpression(stmt.getCheck, 1, Some("boolean"))
    callAst(callNode, args)
  }

  private def astForBlockStatement(stmt: BlockStmt, order: Int, codeStr: String = "<empty>"): AstWithCtx = {
    val block = NewBlock()
      .order(order)
      .code(codeStr)
      .argumentIndex(order)
      .lineNumber(line(stmt))
      .columnNumber(column(stmt))

    scopeStack.pushNewScope(block)

    val stmtAstsWithCtx = withOrderAndCtx(stmt.getStatements.asScala) { (x, o) =>
      astsForStatement(x, o)
    }

    val blockAst = Ast(block).withChildren(stmtAstsWithCtx.map(_.ast))
    val ctx      = mergedCtx(stmtAstsWithCtx.map(_.ctx))

    scopeStack.popScope()

    AstWithCtx(blockAst, ctx)
  }

  private def astForReturnNode(ret: ReturnStmt, order: Int): AstWithCtx = {
    val returnNode = NewReturn()
      .lineNumber(line(ret))
      .columnNumber(column(ret))
      .argumentIndex(order)
      .order(order)
      .code(ret.toString)
    if (ret.getExpression.isPresent) {
      val exprAstsWithCtx = astsForExpression(ret.getExpression.get(), order + 1, None)
      val returnAst = Ast(returnNode)
        .withChildren(exprAstsWithCtx.map(_.ast))
        .withArgEdges(returnNode, exprAstsWithCtx.flatMap(_.ast.root))
      val ctx = mergedCtx(exprAstsWithCtx.map(_.ctx))
      AstWithCtx(returnAst, ctx)
    } else {
      AstWithCtx(Ast(returnNode), new Context())
    }
  }

  def astForUnaryExpr(expr: UnaryExpr, order: Int, expectedType: Option[String]): AstWithCtx = {
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

    val argsWithCtx = astsForExpression(expr.getExpression, 1, expectedType)

    val typeFullName =
      typeInfoProvider
        .getTypeForExpression(expr)
        .orElse(argsWithCtx.headOption.flatMap(rootType))
        .orElse(expectedType)
        .getOrElse(UnresolvedTypeDefault)

    val callNode = NewCall()
      .name(operatorName)
      .methodFullName(operatorName)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .code(expr.toString)
      .argumentIndex(order)
      .order(order)
      .typeFullName(typeFullName)

    callAst(callNode, argsWithCtx)
  }

  def astForArrayAccessExpr(expr: ArrayAccessExpr, order: Int, expectedType: Option[String]): AstWithCtx = {
    val typeFullName = typeInfoProvider.getTypeForExpression(expr).orElse(expectedType).getOrElse(UnresolvedTypeDefault)
    val callNode = NewCall()
      .name(Operators.indexAccess)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .code(expr.toString)
      .order(order)
      .argumentIndex(order)
      .methodFullName(Operators.indexAccess)
      .lineNumber(line(expr))
      .columnNumber(column(expr))
      .typeFullName(typeFullName)

    val argsWithCtx =
      astsForExpression(expr.getName, 1, expectedType.map(_ ++ "[]")) ++ astsForExpression(
        expr.getIndex,
        2,
        Some("int")
      )
    callAst(callNode, argsWithCtx)
  }

  def astForArrayCreationExpr(expr: ArrayCreationExpr, order: Int, expectedType: Option[String]): AstWithCtx = {
    val name = Operators.alloc
    val callNode = NewCall()
      .name(name)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .code(expr.toString)
      .order(order)
      .argumentIndex(order)
      .methodFullName(name)
      .lineNumber(line(expr))
      .columnNumber(column(expr))
      .typeFullName(
        typeInfoProvider.getTypeForExpression(expr).getOrElse(expectedType.getOrElse(UnresolvedTypeDefault))
      )

    val levelAsts = expr.getLevels.asScala.zipWithIndex.flatMap { case (lvl, idx) =>
      lvl.getDimension.toScala match {
        case Some(dimension) => astsForExpression(dimension, idx + 1, Some("int"))

        case None => Seq.empty
      }
    }

    val initializerAstWithCtx =
      expr.getInitializer.toScala
        .map(astForArrayInitializerExpr(_, expr.getLevels.size() + 1, expectedType))
        .getOrElse(AstWithCtx.empty)

    val argsWithCtx = (levelAsts ++ List(initializerAstWithCtx)).toSeq

    callAst(callNode, argsWithCtx)
  }

  def astForArrayInitializerExpr(expr: ArrayInitializerExpr, order: Int, expectedType: Option[String]): AstWithCtx = {
    val typeFullName = typeInfoProvider.getTypeForExpression(expr).orElse(expectedType).getOrElse(UnresolvedTypeDefault)
    val callNode = NewCall()
      .name("<operator>.arrayInitializer")
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .code(expr.toString)
      .order(order)
      .argumentIndex(order)
      .methodFullName("<operator>.arrayInitializer")
      .lineNumber(line(expr))
      .columnNumber(column(expr))
      .typeFullName(typeFullName)

    val MAX_INITIALIZERS = 1000

    val expectedValueType = expr.getValues.asScala.headOption.flatMap(typeInfoProvider.getTypeForExpression)
    val argsWithCtx = expr.getValues.asScala
      .slice(0, MAX_INITIALIZERS)
      .zipWithIndex
      .flatMap { case (c, o) =>
        astsForExpression(c, o, expectedValueType)
      }
      .toSeq

    val AstWithCtx(ast, ctx) = callAst(callNode, argsWithCtx)

    val initAst = if (expr.getValues.size() > MAX_INITIALIZERS) {
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

    AstWithCtx(initAst, ctx)
  }

  def astForBinaryExpr(expr: BinaryExpr, order: Int, expectedType: Option[String]): AstWithCtx = {
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

    val argsWithCtx =
      astsForExpression(expr.getLeft, 1, expectedType) ++ astsForExpression(expr.getRight, 2, expectedType)

    val typeFullName =
      typeInfoProvider
        .getTypeForExpression(expr)
        .orElse(argsWithCtx.headOption.flatMap(rootType))
        .orElse(argsWithCtx.lastOption.flatMap(rootType))
        .orElse(expectedType)
        .getOrElse(UnresolvedTypeDefault)

    val callNode = NewCall()
      .name(operatorName)
      .methodFullName(operatorName)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .code(expr.toString)
      .argumentIndex(order)
      .order(order)
      .lineNumber(line(expr))
      .columnNumber(column(expr))
      .typeFullName(typeFullName)

    callAst(callNode, argsWithCtx)
  }

  def astForCastExpr(expr: CastExpr, order: Int, expectedType: Option[String]): AstWithCtx = {
    val typeFullName =
      typeInfoProvider
        .getTypeFullName(expr)
        .orElse(expectedType)
        .getOrElse(UnresolvedTypeDefault)

    val callNode = NewCall()
      .name(Operators.cast)
      .methodFullName(Operators.cast)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .code(expr.toString)
      .argumentIndex(order)
      .order(order)
      .lineNumber(line(expr))
      .columnNumber(column(expr))
      .typeFullName(typeFullName)

    val typeNode = NewTypeRef()
      .code(expr.getType.toString)
      .order(1)
      .argumentIndex(1)
      .typeFullName(typeFullName)
      .lineNumber(line(expr))
      .columnNumber(column(expr))
    val typeAst = AstWithCtx(Ast(typeNode), Context())

    val exprAst = astsForExpression(expr.getExpression, 2, None)

    callAst(callNode, Seq(typeAst) ++ exprAst)
  }

  private def rootType(astWithCtx: AstWithCtx): Option[String] = {
    astWithCtx.ast.root.flatMap(_.properties.get(PropertyNames.TYPE_FULL_NAME).map(_.toString))
  }

  def astsForAssignExpr(expr: AssignExpr, order: Int, expectedType: Option[String]): Seq[AstWithCtx] = {
    val methodName = expr.getOperator match {
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

    val targetAst  = astsForExpression(expr.getTarget, 1, None)
    val targetType = targetAst.headOption.flatMap(rootType)
    val argsAsts   = astsForExpression(expr.getValue, 2, targetType)
    val valueType  = argsAsts.headOption.flatMap(rootType)
    val argsCtx    = mergedCtx(targetAst.map(_.ctx) ++ argsAsts.map(_.ctx))

    val callNode =
      NewCall()
        .name(methodName)
        .methodFullName(methodName)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
        .code(expr.toString)
        .argumentIndex(order)
        .order(order)
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .typeFullName(targetType.orElse(valueType).orElse(expectedType).getOrElse(UnresolvedTypeDefault))

    if (argsCtx.partialConstructors.isEmpty) {
      val assignAst = callAst(callNode, targetAst ++ argsAsts)
      Seq(assignAst)
    } else {
      if (argsCtx.partialConstructors.size > 1) {
        logger.warn("BUG: Received multiple partial constructors from assignment. Dropping all but the first.")
      }
      val partialConstructor = argsCtx.partialConstructors.head

      targetAst.flatMap(_.ast.root).toList match {
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
      val typeFullName = typeInfoProvider
        .getTypeFullName(variable)
        .orElse(scopeStack.lookupVariable(variable.getTypeAsString).map(_.node.typeFullName)) // TODO: TYPE_CLEANUP
        .getOrElse(UnresolvedTypeDefault)
      val code = s"${variable.getType} $name"

      val local = NewLocal().name(name).code(code).typeFullName(typeFullName).order(order + idx)
      scopeStack.addToScope(name, local)
      local
    }.toList
  }

  private def assignmentsForVarDecl(
    variables: Iterable[VariableDeclarator],
    lineNumber: Option[Integer],
    columnNumber: Option[Integer],
    order: Int
  ): Seq[AstWithCtx] = {
    var constructorCount = 0
    val variablesWithInitializers =
      variables.filter(_.getInitializer.toScala.isDefined)
    val assignments = variablesWithInitializers.zipWithIndex flatMap { case (variable, idx) =>
      val name                    = variable.getName.toString
      val initializer             = variable.getInitializer.toScala.get // Won't crash because of filter
      val initializerTypeFullName = variable.getInitializer.toScala.flatMap(typeInfoProvider.getTypeForExpression)
      val variableTypeFullName =
        typeInfoProvider
          .getTypeFullName(variable)
          .orElse(scopeStack.lookupVariableType(name))
          .orElse(scopeStack.getWildcardType(name))

      val typeFullName = variableTypeFullName match {
        case Some(typ) if TypeInfoProvider.isAutocastType(typ) => typ

        case _ =>
          variableTypeFullName.orElse(initializerTypeFullName).getOrElse(UnresolvedTypeDefault)
      }

      val callNode = NewCall()
        .name(Operators.assignment)
        .methodFullName(Operators.assignment)
        .code(s"$name = ${initializer.toString()}")
        .order(order + idx)
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
      val targetAst = AstWithCtx(Ast(identifier), Context(identifiers = Map(identifier.name -> identifier)))

      // TODO Add expected type here if possible
      val initializerAstsWithCtx = astsForExpression(initializer, 2, Some(typeFullName))
      // Since all partial constructors will be dealt with here, don't pass them up.
      val initAstsWithoutConstructorCtx = initializerAstsWithCtx.map { case AstWithCtx(ast, ctx) =>
        AstWithCtx(ast, ctx.clearConstructors())
      }

      val declAst = callAst(callNode, Seq(targetAst) ++ initAstsWithoutConstructorCtx)

      val constructorAsts = initializerAstsWithCtx
        .flatMap(_.ctx.partialConstructors)
        .map { partialConstructor =>
          constructorCount += 1
          completeInitForConstructor(partialConstructor, identifier, order + idx + constructorCount)
        }

      Seq(declAst) ++ constructorAsts
    }

    assignments.toList
  }

  private def completeInitForConstructor(
    partialConstructor: PartialConstructor,
    identifier: NewIdentifier,
    order: Int
  ): AstWithCtx = {
    val initNode = partialConstructor.initNode
      .order(order)
      .argumentIndex(order)

    val objectNode = identifier.copy
      .order(0)
      .argumentIndex(0)

    val args = partialConstructor.initArgs

    val ast = Ast(initNode)
      .withChild(Ast(objectNode))
      .withReceiverEdge(initNode, objectNode)
      .withChildren(args.map(_.ast))
      .withArgEdge(initNode, objectNode)
      .withArgEdges(initNode, args.flatMap(_.ast.root))

    AstWithCtx(ast, Context())
  }

  def astsForVariableDecl(varDecl: VariableDeclarationExpr, order: Int): Seq[AstWithCtx] = {

    val locals = localsForVarDecl(varDecl, order)
    val localAsts = locals.map { local =>
      AstWithCtx(Ast(local), Context(locals = Seq(local)))
    }

    val assignOrder = order + locals.size
    val assignmentsWithCtx =
      assignmentsForVarDecl(varDecl.getVariables.asScala, line(varDecl), column(varDecl), assignOrder)

    localAsts ++ assignmentsWithCtx
  }

  def callAst(rootNode: NewCall, args: Seq[AstWithCtx]): AstWithCtx = {
    val asts = args.map(_.ast)
    val ctx  = mergedCtx(args.map(_.ctx))
    val ast = Ast(rootNode)
      .withChildren(asts)
      .withArgEdges(rootNode, asts.flatMap(_.root))
    AstWithCtx(ast, ctx)
  }

  def astForClassExpr(expr: ClassExpr, order: Int): AstWithCtx = {
    val callNode = NewCall()
      .name(Operators.fieldAccess)
      .methodFullName(Operators.fieldAccess)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .code(expr.toString)
      .argumentIndex(order)
      .order(order)

    val identifier = NewIdentifier()
      .typeFullName(typeInfoProvider.getTypeFullName(expr).getOrElse(UnresolvedTypeDefault))
      .code(expr.getTypeAsString)
      .lineNumber(line(expr))
      .columnNumber(column(expr))
      .argumentIndex(1)
      .order(1)
    val idAstWithCtx = AstWithCtx(Ast(identifier), Context())

    val fieldIdentifier = NewFieldIdentifier()
      .canonicalName("class")
      .code("class")
      .lineNumber(line(expr))
      .columnNumber(column(expr))
      .argumentIndex(2)
      .order(2)
    val fieldIdAstWithCtx = AstWithCtx(Ast(fieldIdentifier), Context())

    callAst(callNode, Seq(idAstWithCtx, fieldIdAstWithCtx))
  }

  def astForConditionalExpr(expr: ConditionalExpr, order: Int, expectedType: Option[String]): AstWithCtx = {
    val condAst = astsForExpression(expr.getCondition, 1, Some(TypeConstants.Boolean))
    val thenAst = astsForExpression(expr.getThenExpr, 2, expectedType)
    val elseAst = astsForExpression(expr.getElseExpr, 3, expectedType)

    val typeFullName =
      typeInfoProvider
        .getTypeForExpression(expr)
        .orElse(thenAst.headOption.flatMap(rootType))
        .orElse(elseAst.headOption.flatMap(rootType))
        .orElse(expectedType)
        .getOrElse(UnresolvedTypeDefault)

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

  def astForEnclosedExpression(expr: EnclosedExpr, order: Int, expectedType: Option[String]): Seq[AstWithCtx] = {
    astsForExpression(expr.getInner, order, expectedType)
  }

  def astForFieldAccessExpr(expr: FieldAccessExpr, order: Int, expectedType: Option[String]): AstWithCtx = {
    val typeFullName =
      typeInfoProvider
        .getTypeForExpression(expr)
        .orElse(expectedType)
        .getOrElse(UnresolvedTypeDefault)

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
    val fieldIdAstsWithCtx = AstWithCtx(Ast(fieldIdentifierNode), Context())

    callAst(callNode, identifierAsts ++ Seq(fieldIdAstsWithCtx))
  }

  def astForInstanceOfExpr(expr: InstanceOfExpr, order: Int): AstWithCtx = {
    val callNode = NewCall()
      .name(Operators.instanceOf)
      .methodFullName(Operators.instanceOf)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .code(expr.toString)
      .argumentIndex(order)
      .order(order)
      .lineNumber(line(expr))
      .columnNumber(column(expr))
      .typeFullName("boolean")

    val exprAst      = astsForExpression(expr.getExpression, order = 1, None)
    val typeFullName = typeInfoProvider.getTypeFullName(expr.getType).getOrElse(UnresolvedTypeDefault)
    val typeNode =
      NewTypeRef()
        .code(expr.getType.toString)
        .order(exprAst.size + 1)
        .argumentIndex(exprAst.size + 1)
        .lineNumber(line(expr))
        .columnNumber(column(expr.getType))
        .typeFullName(typeFullName)
    val typeAst = AstWithCtx(Ast(typeNode), Context())

    callAst(callNode, exprAst ++ Seq(typeAst))
  }

  def astForNameExpr(x: NameExpr, order: Int, expectedType: Option[String]): AstWithCtx = {
    val name = x.getName.toString
    val typeFullName = typeInfoProvider
      .getTypeForExpression(x)
      .orElse(expectedType)
      .getOrElse(UnresolvedTypeDefault)

    Try(x.resolve()) match {
      case Success(value) if value.isField =>
        val identifierName = if (value.asField.isStatic) {
          // A static field represented by a NameExpr must belong to the class in which it's used. Static fields
          // from other classes are represented by a FieldAccessExpr instead.
          scopeStack.getEnclosingTypeDecl.map(_.name).getOrElse(UnresolvedTypeDefault)
        } else {
          "this"
        }

        val identifierTypeFullName =
          value match {
            case fieldDecl: JavaParserFieldDeclaration =>
              // TODO It is not quite correct to use the declaring classes type.
              // Instead we should take the using classes type which is either the same or a
              // sub class of the declaring class.
              typeInfoProvider.getResolvedTypeDeclFullName(fieldDecl.declaringType())
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

        val identifierAst = AstWithCtx(Ast(identifier), Context())
        val fieldIdentAst = AstWithCtx(Ast(fieldIdentifier), Context())

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
        val ast = variableOption.foldLeft(Ast(identifier))((ast, variableInfo) =>
          ast.withRefEdge(identifier, variableInfo.node)
        )

        AstWithCtx(ast, Context())
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
  def astForObjectCreationExpr(expr: ObjectCreationExpr, order: Int, expectedType: Option[String]): AstWithCtx = {
    val maybeResolvedExpr = Try(expr.resolve())
    val args = withOrder(expr.getArguments) { (x, o) =>
      val expectedArgType = getExpectedConsParamType(maybeResolvedExpr, o - 1)
      astsForExpression(x, o, expectedArgType)
    }.flatten

    val name = Operators.alloc
    val typeFullName = typeInfoProvider
      .getTypeFullName(expr)
      .orElse(expectedType)
      .getOrElse(UnresolvedTypeDefault)
    val argTypes = args.map { arg =>
      arg.ast.root.flatMap(_.properties.get(PropertyNames.TYPE_FULL_NAME)).getOrElse(UnresolvedTypeDefault)
    }
    val signature = s"void(${argTypes.mkString(",")})"

    val allocNode = NewCall()
      .name(name)
      .methodFullName(name)
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
      .methodFullName(s"$typeFullName.<init>:$signature")
      .lineNumber(line(expr))
      .typeFullName("void")
      .code(expr.toString)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .signature(signature)

    // Assume that a block ast is required, since there isn't enough information to decide otherwise.
    // This simplifies logic elsewhere, and unnecessary blocks will be garbage collected soon.
    val blockAst = blockAstForConstructorInvocation(line(expr), column(expr), allocNode, initNode, args, order)

    expr.getParentNode.toScala match {
      case Some(parent) if parent.isInstanceOf[VariableDeclarator] || parent.isInstanceOf[AssignExpr] =>
        val partialConstructor = List(PartialConstructor(initNode, args, blockAst))
        AstWithCtx(Ast(allocNode), Context(partialConstructors = partialConstructor))

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
    args: Seq[AstWithCtx],
    order: Int
  ): AstWithCtx = {
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
      .withChildren(args.map(_.ast))
      .withArgEdge(initNode, identifierForInit)
      .withArgEdges(initNode, args.flatMap(_.ast.root))

    val returnAst = Ast(identifier.copy.order(3).argumentIndex(3))

    val blockAst = Ast(blockNode)
      .withChild(assignmentAst)
      .withChild(initAst)
      .withChild(returnAst)

    AstWithCtx(blockAst, Context())
  }

  def astForThisExpr(expr: ThisExpr, order: Int, expectedType: Option[String]): AstWithCtx = {
    val typeFullName =
      typeInfoProvider
        .getTypeForExpression(expr)
        .orElse(expectedType)
        .getOrElse(UnresolvedTypeDefault)

    val identifier =
      NewIdentifier()
        .name("this")
        .typeFullName(typeFullName)
        .code(expr.toString)
        .order(order)
        .argumentIndex(order)
        .lineNumber(line(expr))
        .columnNumber(column(expr))

    AstWithCtx(Ast(identifier), Context())
  }

  private def astForExplicitConstructorInvocation(stmt: ExplicitConstructorInvocationStmt, order: Int): AstWithCtx = {
    val args = withOrder(stmt.getArguments) { (s, o) =>
      astsForExpression(s, o, None)
    }.flatten

    val typeFullName = typeInfoProvider.getTypeFullName(stmt).getOrElse(UnresolvedTypeDefault)
    val argTypes     = argumentTypesForCall(Try(stmt.resolve()), args)

    val signature = s"void(${argTypes.mkString(",")})"
    val callNode = NewCall()
      .name("<init>")
      .methodFullName(s"$typeFullName.<init>:$signature")
      .argumentIndex(order)
      .order(order)
      .code(stmt.toString)
      .lineNumber(line(stmt))
      .columnNumber(column(stmt))
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .signature(signature)
      .typeFullName("void")

    val thisNode = NewIdentifier()
      .name("this")
      .code("this")
      .order(0)
      .argumentIndex(0)
      .typeFullName(typeFullName)
    val thisAst = AstWithCtx(Ast(thisNode), Context())

    val AstWithCtx(ast, ctx) = callAst(callNode, Seq(thisAst) ++ args)

    // ast.root should just be `callNode`, but do a sanity check in any case.
    ast.root match {
      case None =>
        logger.warn("Attempting to create constructor invocation without root")
        AstWithCtx(ast, ctx)

      case Some(root) =>
        AstWithCtx(ast.withReceiverEdge(root, thisNode), ctx)
    }
  }

  private def astsForExpression(expression: Expression, order: Int, expectedType: Option[String]): Seq[AstWithCtx] = {
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
      case x: LambdaExpr              => Seq(astForLambdaExpr(x, order))
      case x: LiteralExpr             => Seq(astForLiteralExpr(x, order))
      case x: MethodCallExpr          => Seq(astForMethodCall(x, order, expectedType))
      case x: NameExpr                => Seq(astForNameExpr(x, order, expectedType))
      case x: ObjectCreationExpr      => Seq(astForObjectCreationExpr(x, order, expectedType))
      case x: SuperExpr               => astForSuperExpr(x, order, expectedType)
      case x: ThisExpr                => Seq(astForThisExpr(x, order, expectedType))
      case x: UnaryExpr               => Seq(astForUnaryExpr(x, order, expectedType))
      case x: VariableDeclarationExpr => astsForVariableDecl(x, order)
      case x                          => Seq(unknownAst(x, order))
    }
  }

  private def unknownAst(node: Node, order: Int): AstWithCtx = {
    val unknownNode =
      NewUnknown()
        .code(node.toString)
        .lineNumber(line(node))
        .columnNumber(column(node))
        .order(order)
        .argumentIndex(order)

    AstWithCtx(Ast(unknownNode), Context())
  }

  private def codePrefixForMethodCall(call: MethodCallExpr): String = {
    Try(call.resolve()) match {
      case Success(resolvedCall) =>
        call.getScope.toScala match {
          case Some(scope) => s"${scope.toString}."

          case None =>
            if (resolvedCall.isStatic) "" else "this."
        }

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

  var lambdaCounter = 0
  private def nextLambdaName(): String = {
    lambdaCounter += 1
    s"<lambda>$lambdaCounter"
  }

  private def lambdaSignature(params: List[Parameter]): String = {
    params match {
      case Nil => ""

      case List(_) => "ANY"

      case values => s"(${values.map(_ => "ANY").mkString(",")})"
    }
  }

  private def astForLambdaExpr(expr: LambdaExpr, order: Int): AstWithCtx = {
    // TODO: Fix types in scope for lambdas
    // TODO: Fix class name (currently `com.github.javaparser.ast.expr.LambdaExpr`)
    val className = scopeStack.getEnclosingTypeDecl.map(_.fullName).getOrElse("<empty>")
    val fullName  = s"$className:${nextLambdaName()}"

    val parameterAstsWithCtx = astsForParameterList(expr.getParameters)
    val namesToMethodParams  = mapNamesToParams(parameterAstsWithCtx)

    val bodyOrder = parameterAstsWithCtx.size + 2
    val bodyAstWithCtx = if (expr.getBody.isBlockStmt) {
      astsForStatement(expr.getBody, bodyOrder).headOption
        .getOrElse(emptyBlock(bodyOrder))
    } else {
      val blockNode =
        NewBlock()
          .lineNumber(line(expr.getBody))
          .columnNumber(column(expr.getBody))
          .order(bodyOrder)
          .argumentIndex(bodyOrder)

      val asts = astsForStatement(expr.getBody, 1)

      AstWithCtx(Ast(blockNode).withChildren(asts.map(_.ast)), Context.mergedCtx(asts.map(_.ctx)))
    }

    val (identifiersMatchingParams, identifiersNotMatchingParams) = {
      bodyAstWithCtx.ctx.identifiers.values.toSeq.partition(identifier => namesToMethodParams.contains(identifier.name))
    }

    val closureBindings = closureBindingsForLambdas(identifiersNotMatchingParams)
    val refEdgePairs    = buildRefEdgePairs(identifiersMatchingParams, namesToMethodParams)

    val methodAst =
      lambdaMethodAst(
        expr,
        fullName,
        parameterAstsWithCtx.map(_.ast),
        bodyAstWithCtx.ast,
        closureBindings,
        refEdgePairs
      )

    val methodRef = lambdaMethodRef(expr, fullName, order)

    val closuresWithMeta = buildClosuresWithMeta(closureBindings, methodRef)

    AstWithCtx(
      Ast(methodRef),
      Context(
        lambdaAsts = Seq(methodAst),
        identifiers = bodyAstWithCtx.ctx.identifiers,
        closureBindingInfo = closuresWithMeta
      )
    )
  }

  private def buildClosuresWithMeta(
    closureBindings: Seq[ClosureBindingInfo],
    methodRef: NewMethodRef
  ): Seq[ClosureBindingMeta] = {
    // This is just to get javasrc to compile. Lambdas are completeley broken, so this will probably be rewritten
    // completely
    val nameToNode: Map[String, NewNode] = Map.empty

    closureBindings.map { closureBindingInfo =>
      val name  = closureBindingInfo.identifier.name
      val edges = List((methodRef, closureBindingInfo.closure, EdgeTypes.CAPTURE))

      val refEdges =
        if (nameToNode.contains(name)) {
          val node = nameToNode(name)
          List((closureBindingInfo.closure, node, EdgeTypes.REF))
        } else {
          List()
        }
      ClosureBindingMeta(closureBindingInfo.closure, edges ++ refEdges)
    }
  }

  private def buildRefEdgePairs(
    identifiers: Seq[NewIdentifier],
    namesToMethodParams: Map[String, Ast]
  ): Seq[RefEdgePair] = {
    identifiers.map { identifier =>
      val methodParamInNode =
        namesToMethodParams(identifier.name).root.get.asInstanceOf[NewMethodParameterIn]
      RefEdgePair(identifier, methodParamInNode)
    }
  }

  private def closureBindingsForLambdas(identifiers: Seq[NewIdentifier]): Seq[ClosureBindingInfo] = {
    identifiers.map { identifier =>
      val closureBindingId = randomUUID().toString
      val closure =
        NewClosureBinding()
          .closureBindingId(Some(closureBindingId))
          .evaluationStrategy(EvaluationStrategies.BY_REFERENCE)
          .closureOriginalName(Some(identifier.name))

      ClosureBindingInfo(identifier, closure, closureBindingId)
    }.toList
  }

  private def mapNamesToParams(parameterAstsWithCtx: Seq[AstWithCtx]): Map[String, Ast] = {
    parameterAstsWithCtx
      .map(_.ast)
      .filter(_.root.get.isInstanceOf[NewMethodParameterIn])
      .map { paramAst =>
        val node = paramAst.root.get.asInstanceOf[NewMethodParameterIn]
        node.name -> paramAst
      }
      .toMap
  }

  private def lambdaMethodRef(expr: LambdaExpr, fullName: String, order: Int): NewMethodRef = {
    NewMethodRef()
      .code("")
      .methodFullName(fullName)
      .typeFullName("ANY")
      .lineNumber(line(expr))
      .columnNumber(column(expr))
      .order(order)
  }

  private def lambdaMethodAst(
    expr: LambdaExpr,
    fullName: String,
    parameterAsts: Seq[Ast],
    bodyAst: Ast,
    closureBindings: Seq[ClosureBindingInfo],
    refEdgePairs: Seq[RefEdgePair]
  ): Ast = {
    val signature    = lambdaSignature(expr.getParameters.asScala.toList)
    val lineNumber   = line(expr)
    val columnNumber = column(expr)

    val lambdaMethodNode =
      NewMethod()
        .name("<lambda>")
        .code("")
        .isExternal(false)
        .fullName(fullName)
        .lineNumber(lineNumber)
        .columnNumber(columnNumber)
        .signature(signature)
        .filename(filename)

    val localsForCapturedIdentifiers =
      closureBindings.zipWithIndex.map { case (bindingWithInfo, idx) =>
        val identifier = bindingWithInfo.identifier
        Ast(
          NewLocal()
            .name(identifier.name)
            .code(identifier.code)
            .typeFullName(identifier.typeFullName)
            .lineNumber(identifier.lineNumber)
            .columnNumber(identifier.columnNumber)
            .closureBindingId(bindingWithInfo.bindingId)
        )
      }

    val retNode = methodReturnNode(lineNumber, columnNumber, "ANY")

    val lambdaMethodAst = Ast(lambdaMethodNode)
      .withChildren(parameterAsts)
      .withChild(bodyAst.withChildren(localsForCapturedIdentifiers))
      .withChild(Ast(retNode))

    val lambdaMethodAstWithRefEdges = refEdgePairs.foldLeft(lambdaMethodAst)((acc, edgePair) => {
      acc.withRefEdge(edgePair.from, edgePair.to)
    })

    lambdaMethodAstWithRefEdges
  }

  private def astForLiteralExpr(expr: LiteralExpr, order: Int = 1): AstWithCtx = {
    AstWithCtx(
      Ast(
        NewLiteral()
          .order(order)
          .argumentIndex(order)
          .code(expr.toString)
          .typeFullName(typeInfoProvider.getTypeForExpression(expr).getOrElse(UnresolvedTypeDefault))
          .lineNumber(line(expr))
          .columnNumber(column(expr))
      ),
      Context()
    )
  }

  private def getExpectedParamType(maybeResolvedCall: Try[ResolvedMethodDeclaration], idx: Int): Option[String] = {
    maybeResolvedCall.toOption.flatMap { call =>
      Try(typeInfoProvider.getResolvedTypeFullName(call.getParam(idx).getType)).toOption.flatten
    }
  }

  private def getExpectedConsParamType(
    maybeResolvedCall: Try[ResolvedConstructorDeclaration],
    idx: Int
  ): Option[String] = {
    maybeResolvedCall.toOption.flatMap { call =>
      Try(typeInfoProvider.getResolvedTypeFullName(call.getParam(idx).getType)).toOption.flatten
    }
  }

  private def dispatchTypeForCall(maybeDecl: Try[ResolvedMethodDeclaration], maybeScope: Option[Expression]): String = {
    maybeScope match {
      case Some(_: SuperExpr) =>
        DispatchTypes.STATIC_DISPATCH
      case _ =>
        maybeDecl match {
          case Success(decl) =>
            if (decl.isStatic) {
              DispatchTypes.STATIC_DISPATCH
            } else {
              DispatchTypes.DYNAMIC_DISPATCH
            }
          case _ =>
            DispatchTypes.DYNAMIC_DISPATCH
        }
    }
  }

  private def targetTypeForCall(callExpr: MethodCallExpr): Option[String] = {
    Try(callExpr.resolve()) match {
      case Success(resolveMethodDecl) =>
        Some(typeInfoProvider.getResolvedTypeDeclFullName(resolveMethodDecl.declaringType()))
      case Failure(_) =>
        callExpr.getScope.toScala match {
          case Some(scope: ThisExpr) =>
            typeInfoProvider
              .getTypeForExpression(scope)
              .orElse(scopeStack.getEnclosingTypeDecl.map(_.fullName))

          case Some(scope: SuperExpr) =>
            typeInfoProvider
              .getTypeForExpression(scope)
              .orElse(scopeStack.getEnclosingTypeDecl.flatMap(_.inheritsFromTypeFullName.headOption))

          case Some(scope) => typeInfoProvider.getTypeForExpression(scope)

          case None =>
            scopeStack.getEnclosingTypeDecl.map(_.fullName)
        }
    }
  }

  private def rootName(ast: AstWithCtx): Option[String] = {
    ast.ast.root.flatMap(_.properties.get(PropertyNames.NAME).map(_.toString))
  }

  private def argumentTypesForCall(
    maybeMethod: Try[ResolvedMethodLikeDeclaration],
    argAsts: Seq[AstWithCtx]
  ): List[String] = {
    maybeMethod match {
      case Success(resolved) =>
        val matchingArgs = argAsts.headOption match {
          case Some(ast) if rootName(ast).contains("this") =>
            argAsts.tail

          case _ => argAsts
        }

        (0 until resolved.getNumberOfParams)
          .zip(matchingArgs)
          .map { case (idx, argAst) =>
            val param = resolved.getParam(idx)
            typeInfoProvider
              .getTypeFullName(param)
              .orElse(rootType(argAst))
              .getOrElse(UnresolvedTypeDefault)
          }
          .toList

      case Failure(_) =>
        // Fall back to actual argument types if the called method couldn't be resolved.
        // This may result in missing dataflows.
        argAsts.map(arg => rootType(arg).getOrElse(UnresolvedTypeDefault)).toList
    }
  }

  private def astForMethodCall(call: MethodCallExpr, order: Int = 1, expectedReturnType: Option[String]): AstWithCtx = {
    val maybeResolvedCall = Try(call.resolve())
    val argumentAsts = withOrder(call.getArguments) { (arg, o) =>
      // TODO: Verify index
      val expectedType = getExpectedParamType(maybeResolvedCall, o - 1)
      astsForExpression(arg, o, expectedType)
    }.flatten

    val maybeReturnType = typeInfoProvider
      .getReturnType(call)
      .orElse(expectedReturnType)

    val dispatchType = dispatchTypeForCall(maybeResolvedCall, call.getScope.toScala)

    val maybeTargetType = targetTypeForCall(call)

    val argumentTypes = argumentTypesForCall(maybeResolvedCall, argumentAsts)

    val (signature, methodFullName) = (maybeTargetType, maybeReturnType) match {
      case (Some(targetType), Some(returnType)) =>
        val signature      = s"$returnType(${argumentTypes.mkString(",")})"
        val methodFullName = s"$targetType.${call.getNameAsString}:$signature"
        (signature, methodFullName)

      case _ =>
        ("", "")
    }

    val codePrefix = codePrefixForMethodCall(call)
    val callNode = NewCall()
      .typeFullName(maybeReturnType.getOrElse(UnresolvedTypeDefault))
      .name(call.getNameAsString)
      .methodFullName(methodFullName)
      .signature(signature)
      .dispatchType(dispatchType)
      .code(s"$codePrefix${call.getNameAsString}(${call.getArguments.asScala.mkString(", ")})")
      .order(order)
      .argumentIndex(order)
      .lineNumber(line(call))
      .columnNumber(column(call))

    val scopeAsts = call.getScope.toScala match {
      case Some(scope) =>
        astsForExpression(scope, 0, maybeTargetType)

      case None =>
        val objectNode = createObjectNode(maybeTargetType.getOrElse(UnresolvedTypeDefault), call, callNode)
        objectNode.map { objIdentifier =>
          AstWithCtx(Ast(objIdentifier), Context(identifiers = Map(objIdentifier.name -> objIdentifier)))
        }.toSeq
    }
//    val maybeScopeAst = call.getScope.toScala.toList.flatMap(astsForExpression(_, scopeContext, 0, maybeTargetType))
//    val objectNode = createObjectNode(maybeTargetType.getOrElse(UnresolvedTypeDefault), call, callNode)
//    val objectAst = objectNode
//      .map(objIdentifier =>
//        AstWithCtx(Ast(objIdentifier), Context(identifiers = Map(objIdentifier.name -> objIdentifier)))
//      )
//      .getOrElse(AstWithCtx.empty)

    val ast = callAst(callNode, scopeAsts ++ argumentAsts)

    scopeAsts.headOption.flatMap(_.ast.root) match {
      case None => ast

      case Some(rootNode) =>
        AstWithCtx(ast.ast.withReceiverEdge(callNode, rootNode), ast.ctx)
    }
  }

  def astForSuperExpr(superExpr: SuperExpr, order: Int, expectedType: Option[String]): AstWithCtx = {
    val typeFullName =
      typeInfoProvider
        .getTypeForExpression(superExpr)
        .orElse(expectedType)
        .getOrElse(UnresolvedTypeDefault)

    val thisIdentifier = NewIdentifier()
      .name("this")
      .code("super")
      .typeFullName(typeFullName)
      .order(order)
      .argumentIndex(order)
      .lineNumber(line(superExpr))
      .columnNumber(column(superExpr))

    AstWithCtx(Ast(thisIdentifier), new Context())
  }

  private def astsForParameterList(parameters: NodeList[Parameter], order: Int = 0): Seq[AstWithCtx] = {
    withOrder(parameters) { (p, o) =>
      astForParameter(p, order + o)
    }
  }

  private def astForParameter(parameter: Parameter, childNum: Int): AstWithCtx = {
    val typeFullName =
      typeInfoProvider
        .getTypeFullName(parameter)
        .orElse(scopeStack.lookupVariableType(parameter.getTypeAsString))
        .orElse(scopeStack.getWildcardType(parameter.getTypeAsString))
        .getOrElse(UnresolvedTypeDefault)
    val parameterNode = NewMethodParameterIn()
      .name(parameter.getName.toString)
      .code(parameter.toString)
      .typeFullName(typeFullName)
      .order(childNum)
      .lineNumber(line(parameter))
      .columnNumber(column(parameter))
      .evaluationStrategy(EvaluationStrategies.BY_VALUE)
    val annotationAsts = parameter.getAnnotations.asScala.map(astForAnnotationExpr)
    val ast            = Ast(parameterNode)

    scopeStack.addToScope(parameter.getNameAsString, parameterNode)
    AstWithCtx(ast.withChildren(annotationAsts), Context(methodParameters = List(parameterNode)))
  }

  private def constructorFullName(typeDecl: Option[NewTypeDecl], signature: String): String = {
    val typeName = typeDecl.map(_.fullName).getOrElse(UnresolvedTypeDefault)
    s"$typeName.<init>:$signature"
  }

  private def emptyBlock(order: Int): AstWithCtx = {
    val node = NewBlock().order(order).argumentIndex(order)
    AstWithCtx(Ast(node), Context())
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

  // TODO: Remove this completely when removing order setting
  def withOrderAndCtx[T <: Node](nodeList: Iterable[T], initialOrder: Int = 1)(
    f: (T, Int) => Seq[AstWithCtx]
  ): Seq[AstWithCtx] = {
    var orderOffset = 0

    val asts = nodeList.flatMap { x =>
      val astsWithCtx = f(x, initialOrder + orderOffset)
      val ctx         = mergedCtx(astsWithCtx.map(_.ctx))
      orderOffset += astsWithCtx.size
      astsWithCtx
    }.toSeq

    asts
  }
}
