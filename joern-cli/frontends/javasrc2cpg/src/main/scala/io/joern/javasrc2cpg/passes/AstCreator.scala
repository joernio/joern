package io.joern.javasrc2cpg.passes

import com.github.javaparser.ast.{CompilationUnit, Node, NodeList, PackageDeclaration}
import com.github.javaparser.ast.body.{
  CallableDeclaration,
  ConstructorDeclaration,
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
  MethodCallExpr,
  MethodReferenceExpr,
  NameExpr,
  NullLiteralExpr,
  ObjectCreationExpr,
  PatternExpr,
  StringLiteralExpr,
  SuperExpr,
  SwitchExpr,
  TextBlockLiteralExpr,
  ThisExpr,
  TypeExpr,
  UnaryExpr,
  VariableDeclarationExpr
}
import com.github.javaparser.ast.nodeTypes.NodeWithType
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
  LocalClassDeclarationStmt,
  LocalRecordDeclarationStmt,
  ReturnStmt,
  Statement,
  SwitchEntry,
  SwitchStmt,
  SynchronizedStmt,
  ThrowStmt,
  TryStmt,
  UnparsableStmt,
  WhileStmt,
  YieldStmt
}
import com.github.javaparser.resolution.Resolvable
import com.github.javaparser.resolution.declarations.ResolvedMethodDeclaration
import com.github.javaparser.resolution.types.ResolvedType
import io.joern.javasrc2cpg.passes.AstWithCtx.astWithCtxToSeq
import io.joern.javasrc2cpg.passes.Context.mergedCtx
import io.shiftleft.codepropertygraph.generated.{
  ControlStructureTypes,
  DispatchTypes,
  EdgeTypes,
  EvaluationStrategies,
  Operators
}
import io.shiftleft.codepropertygraph.generated.nodes.{
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
  NewNamespaceBlock,
  NewNode,
  NewReturn,
  NewTypeDecl
}
import io.shiftleft.passes.DiffGraph
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal.globalNamespaceName
import io.shiftleft.x2cpg.Ast

import java.util.UUID.randomUUID
import scala.jdk.CollectionConverters._
import scala.collection.mutable
import scala.jdk.OptionConverters.RichOptional
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

case class BindingInfo(node: NewBinding, edgeMeta: Seq[(NewNode, NewNode, String)])
case class ClosureBindingInfo(
    identifier: NewIdentifier,
    closure: NewClosureBinding,
    bindingId: String
)
case class ClosureBindingMeta(node: NewClosureBinding, edgeMeta: Seq[(NewNode, NewNode, String)])

case class Context(
    locals: Seq[NewLocal] = List(),
    identifiers: Seq[NewIdentifier] = List(),
    methodParameters: Seq[NewMethodParameterIn] = List(),
    bindingsInfo: Seq[BindingInfo] = List(),
    lambdaAsts: Seq[Ast] = List(),
    closureBindingInfo: Seq[ClosureBindingMeta] = List()
) {
  def ++(other: Context): Context = {
    val newLocals = locals ++ other.locals
    val newIdentifiers = identifiers ++ other.identifiers
    val newParameters = methodParameters ++ other.methodParameters
    val newBindings = bindingsInfo ++ other.bindingsInfo
    val newLambdas = lambdaAsts ++ other.lambdaAsts
    val newClosureBindings = closureBindingInfo ++ other.closureBindingInfo

    Context(newLocals, newIdentifiers, newParameters, newBindings, newLambdas, newClosureBindings)
  }

  def mergeWith(others: Iterable[Context]): Context = {
    Context.mergedCtx(Seq(this) ++ others)
  }
}

object Context {
  def mergedCtx(ctxs: Seq[Context]): Context = {
    ctxs.foldLeft(Context())((acc, ctx) => { acc ++ ctx })
  }
}

case class ScopeContext(
    typeDecl: Option[NewTypeDecl] = None,
    methodParameters: Seq[NewMethodParameterIn] = List(),
    locals: Seq[NewLocal] = List()
) {
  def withNewParams(newParams: Seq[NewMethodParameterIn]): ScopeContext = {
    newParams match {
      case Seq()     => this
      case newParams => copy(methodParameters = methodParameters ++ newParams)
    }
  }

  def withNewLocals(newLocals: Seq[NewLocal]): ScopeContext = {
    newLocals match {
      case Seq()     => this
      case newLocals => copy(locals = locals ++ newLocals)
    }
  }
}

case class RefEdgePair(from: NewIdentifier, to: NewMethodParameterIn)

case class AstWithCtx(ast: Ast, ctx: Context)

object AstWithCtx {
  val empty: AstWithCtx = AstWithCtx(Ast(), Context())

  implicit def astWithCtxToSeq(astWithCtx: AstWithCtx): Seq[AstWithCtx] = {
    Seq(astWithCtx)
  }
}

class AstCreator(filename: String, global: Global) {

  import AstCreator._

  val stack: mutable.Stack[NewNode] = mutable.Stack()
  val diffGraph: DiffGraph.Builder = DiffGraph.newBuilder

  /** Add `typeName` to a global map and return it. The
    * map is later passed to a pass that creates TYPE
    * nodes for each key in the map.
    */
  private def registerType(typeName: String): String = {
    global.usedTypes.put(typeName, true)
    typeName
  }

  /** Entry point of AST creation. Translates a compilation
    * unit created by JavaParser into a DiffGraph containing
    * the corresponding CPG AST.
    */
  def createAst(parserResult: CompilationUnit): Iterator[DiffGraph] = {
    storeInDiffGraph(astForCompilationUnit(parserResult))
    Iterator(diffGraph.build())
  }

  /** Copy nodes/edges of given `AST` into the diff graph
    */
  private def storeInDiffGraph(astWithCtx: AstWithCtx): Unit = {
    val ast = astWithCtx.ast
    ast.nodes.foreach { node =>
      diffGraph.addNode(node)
    }
    ast.edges.foreach { edge =>
      diffGraph.addEdge(edge.src, edge.dst, EdgeTypes.AST)
    }
    ast.conditionEdges.foreach { edge =>
      diffGraph.addEdge(edge.src, edge.dst, EdgeTypes.CONDITION)
    }
    ast.argEdges.foreach { edge =>
      diffGraph.addEdge(edge.src, edge.dst, EdgeTypes.ARGUMENT)
    }
    ast.refEdges.foreach { edge =>
      diffGraph.addEdge(edge.src, edge.dst, EdgeTypes.REF)
    }
    ast.receiverEdges.foreach { edge =>
      diffGraph.addEdge(edge.src, edge.dst, EdgeTypes.RECEIVER)
    }

    astWithCtx.ctx.bindingsInfo.foreach { bindingInfo =>
      diffGraph.addNode(bindingInfo.node)

      bindingInfo.edgeMeta.foreach {
        case (src, dst, label) =>
          diffGraph.addEdge(src, dst, label)
      }
    }

    astWithCtx.ctx.closureBindingInfo.foreach { closureBindingInfo =>
      diffGraph.addNode(closureBindingInfo.node)

      closureBindingInfo.edgeMeta.foreach {
        case (src, dest, label) =>
          diffGraph.addEdge(src, dest, label)
      }
    }
  }

  /** Translate compilation unit into AST
    */
  private def astForCompilationUnit(compilationUnit: CompilationUnit): AstWithCtx = {
    val AstWithCtx(ast, ctx) = astForPackageDeclaration(
      compilationUnit.getPackageDeclaration.toScala
    )
    val namespaceBlockFullName = {
      ast.root.collect { case x: NewNamespaceBlock => x.fullName }.getOrElse("none")
    }
    val typeDeclAstsWithCtx = withOrder(compilationUnit.getTypes) { (typ, order) =>
      astForTypeDecl(typ, order, namespaceBlockFullName)
    }

    val typeDeclAsts = typeDeclAstsWithCtx.map(_.ast)
    val mergedCtx = ctx.mergeWith(typeDeclAstsWithCtx.map(_.ctx))

    val lambdaTypeDeclAsts = mergedCtx.lambdaAsts.map { lambdaAst =>
      val root = lambdaAst.root.get.asInstanceOf[NewMethod]
      // TODO: Inherit from implemented interface and bind to implemented method
      val lambdaTypeDecl = NewTypeDecl()
        .name(root.name)
        .fullName(root.fullName)
      Ast(lambdaTypeDecl).withChild(lambdaAst)
    }

    AstWithCtx(ast.withChildren(typeDeclAsts).withChildren(lambdaTypeDeclAsts), mergedCtx)
  }

  /** Translate package declaration into AST consisting of
    * a corresponding namespace block.
    */
  private def astForPackageDeclaration(packageDecl: Option[PackageDeclaration]): AstWithCtx = {
    val absolutePath = new java.io.File(filename).toPath.toAbsolutePath.normalize().toString
    val namespaceBlock = packageDecl match {
      case Some(decl) =>
        val packageName = decl.getName.toString
        val name = packageName.split("\\.").lastOption.getOrElse("")
        NewNamespaceBlock()
          .name(name)
          .fullName(packageName)
      case None =>
        createGlobalNamespaceBlock
    }
    AstWithCtx(
      Ast(namespaceBlock.filename(absolutePath).order(1)),
      Context()
    )
  }

  private def createGlobalNamespaceBlock: NewNamespaceBlock =
    NewNamespaceBlock()
      .name(globalNamespaceName)
      .fullName(globalNamespaceName)

  private def astForTypeDecl(
      typ: TypeDeclaration[_],
      order: Int,
      namespaceBlockFullName: String
  ): AstWithCtx = {
    val baseTypeFullNames = typ
      .asClassOrInterfaceDeclaration()
      .getExtendedTypes
      .asScala
      .map(x => registerType(x.resolve().getQualifiedName))
      .toList

    val typeDecl = NewTypeDecl()
      .name(typ.getNameAsString)
      .fullName(typ.getFullyQualifiedName.toScala.getOrElse(""))
      .inheritsFromTypeFullName(baseTypeFullNames)
      .order(order)
      .filename(filename)
      .code(typ.getNameAsString)
      .astParentType("NAMESPACE_BLOCK")
      .astParentFullName(namespaceBlockFullName)

    val initScopeContext = ScopeContext(typeDecl = Some(typeDecl))

    val (constructorAsts, scopeContextWithCons) =
      withOrderAndCtx(typ.getConstructors.asScala, initScopeContext) { (c, scopeCtx, order) =>
        astForConstructor(c, scopeCtx, order)
      }

    val (methodAsts, _) = withOrderAndCtx(typ.getMethods.asScala, scopeContextWithCons) { (m, scopeCtx, order) =>
      astForMethod(m, scopeCtx, order + typ.getConstructors.size)
    }

    val bindingsInfo =
      (constructorAsts ++ methodAsts).map(_.ast).map { ast =>
        val methodNode = ast.root.get.asInstanceOf[NewMethod]
        val signature = {
          if (methodNode.signature.endsWith("()")) {
            "ANY()"
          } else {
            val numParams = methodNode.signature.count(_ == ',')
            "ANY(ANY" + ",ANY" * (numParams - 1) + ")"
          }
        }
        val node =
          NewBinding()
            .name(methodNode.name)
            .signature(signature)
        BindingInfo(
          node,
          List((typeDecl, node, EdgeTypes.BINDS), (node, ast.root.get, EdgeTypes.REF))
        )
      }

    val memberAsts = typ.getMembers.asScala
      .filter(_.isFieldDeclaration)
      .flatMap { m =>
        val fieldDeclaration = m.asFieldDeclaration()
        fieldDeclaration.getVariables.asScala
      }
      .zipWithIndex
      .map {
        case (v, i) =>
          astForVariableDeclarator(v, i + methodAsts.size + 1)
      }
      .toList

    val typeDeclAst = Ast(typeDecl)
      .withChildren(memberAsts.map(_.ast))
      .withChildren(constructorAsts.map(_.ast))
      .withChildren(methodAsts.map(_.ast))

    val bindingsContext = Context(bindingsInfo = bindingsInfo)
    val typeDeclContext =
      bindingsContext.mergeWith((constructorAsts ++ methodAsts ++ memberAsts).map(_.ctx))

    AstWithCtx(typeDeclAst, typeDeclContext)
  }

  private def astForVariableDeclarator(v: VariableDeclarator, order: Int): AstWithCtx = {
    val typeFullName = registerType(tryResolveType(v))
    val name = v.getName.toString
    val ast = Ast(
      NewMember()
        .name(name)
        .typeFullName(typeFullName)
        .order(order)
        .code(s"$typeFullName $name")
    )
    AstWithCtx(ast, Context())
  }

  private def astForConstructor(
      constructorDeclaration: ConstructorDeclaration,
      scopeContext: ScopeContext,
      childNum: Int
  ): AstWithCtx = {
    val constructorNode =
      createConstructorNode(constructorDeclaration, scopeContext.typeDecl, childNum)

    val parameterAstsWithCtx = astsForParameterList(constructorDeclaration.getParameters)
    val lastOrder = 2 + parameterAstsWithCtx.size
    val scopeWithParams =
      scopeContext.copy(methodParameters = parameterAstsWithCtx.flatMap(_.ctx.methodParameters))

    val bodyAstWithCtx =
      astForMethodBody(Some(constructorDeclaration.getBody), scopeWithParams, lastOrder)
    val returnAstWithCtx = astForConstructorReturn(constructorDeclaration)

    val constructorAst = Ast(constructorNode)
      .withChildren(parameterAstsWithCtx.map(_.ast))
      .withChild(bodyAstWithCtx.ast)
      .withChild(returnAstWithCtx)

    val ctx = bodyAstWithCtx.ctx.mergeWith(parameterAstsWithCtx.map(_.ctx))

    AstWithCtx(constructorAst, ctx)
  }

  private def astForMethod(
      methodDeclaration: MethodDeclaration,
      scopeContext: ScopeContext,
      childNum: Int
  ): AstWithCtx = {
    val methodNode = createMethodNode(methodDeclaration, scopeContext.typeDecl, childNum)
    val parameterAstsWithCtx = astsForParameterList(methodDeclaration.getParameters)
    val lastOrder = 1 + parameterAstsWithCtx.size

    val scopeCtxWithParams =
      scopeContext.copy(methodParameters = parameterAstsWithCtx.flatMap(_.ctx.methodParameters))
    val bodyAstWithCtx =
      astForMethodBody(methodDeclaration.getBody.toScala, scopeCtxWithParams, lastOrder)
    val returnAstWithCtx = astForMethodReturn(methodDeclaration)

    val ast = Ast(methodNode)
      .withChildren(parameterAstsWithCtx.map(_.ast))
      .withChild(bodyAstWithCtx.ast)
      .withChild(returnAstWithCtx)

    val ctx = bodyAstWithCtx.ctx.mergeWith(parameterAstsWithCtx.map(_.ctx))

    AstWithCtx(ast, ctx)
  }

  private def astForMethodReturn(methodDeclaration: MethodDeclaration): Ast = {
    val typeFullName = registerType(tryResolveType(methodDeclaration))
    val methodReturnNode =
      NewMethodReturn()
        .order(methodDeclaration.getParameters.size + 2)
        .typeFullName(typeFullName)
        .code(methodDeclaration.getTypeAsString)
        .lineNumber(line(methodDeclaration.getType))
    Ast(methodReturnNode)
  }

  private def astForConstructorReturn(constructorDeclaration: ConstructorDeclaration): Ast = {
    val typeFullName = Try(constructorDeclaration.resolve().declaringType().getName)
      .getOrElse(s"<unresolved>.${constructorDeclaration.getNameAsString}")
    val constructorReturnNode =
      NewMethodReturn()
        .order(constructorDeclaration.getParameters.size + 2)
        .typeFullName(typeFullName)
        .code(constructorDeclaration.getNameAsString)
        .lineNumber(constructorDeclaration.getEnd.map(x => Integer.valueOf(x.line)).toScala)
    Ast(constructorReturnNode)
  }

  /** Constructor and Method declarations share a lot of fields, so this method adds the fields they have in common.
    * `fullName` and `signature` are omitted
    */
  private def createPartialMethod(
      declaration: CallableDeclaration[_],
      childNum: Int
  ): NewMethod = {
    val code = declaration.getDeclarationAsString.trim
    val columnNumber = declaration.getBegin.map(x => Integer.valueOf(x.column)).toScala
    val endLine = declaration.getEnd.map(x => Integer.valueOf(x.line)).toScala
    val endColumn = declaration.getEnd.map(x => Integer.valueOf(x.column)).toScala

    val methodNode = NewMethod()
      .name(declaration.getNameAsString)
      .code(code)
      .isExternal(false)
      .order(childNum)
      .filename(filename)
      .lineNumber(line(declaration))
      .columnNumber(columnNumber)
      .lineNumberEnd(endLine)
      .columnNumberEnd(endColumn)

    methodNode
  }

  private def createConstructorNode(
      constructorDeclaration: ConstructorDeclaration,
      typeDecl: Option[NewTypeDecl],
      childNum: Int
  ): NewMethod = {
    val fullName = constructorFullName(typeDecl, constructorDeclaration)
    val signature =
      constructorDeclaration.getNameAsString + paramListSignature(constructorDeclaration)
    createPartialMethod(constructorDeclaration, childNum)
      .fullName(fullName)
      .signature(signature)
  }

  private def createMethodNode(
      methodDeclaration: MethodDeclaration,
      typeDecl: Option[NewTypeDecl],
      childNum: Int
  ) = {
    val fullName = methodFullName(typeDecl, methodDeclaration)
    val signature = methodDeclaration.getTypeAsString + paramListSignature(methodDeclaration)
    createPartialMethod(methodDeclaration, childNum)
      .fullName(fullName)
      .signature(signature)
  }

  private def astForMethodBody(
      body: Option[BlockStmt],
      scopeContext: ScopeContext,
      order: Int
  ): AstWithCtx = {
    body match {
      case Some(b) => astForBlockStatement(b, scopeContext, order)
      case None =>
        val blockNode = NewBlock()
        AstWithCtx(Ast(blockNode), Context())
    }
  }

  def astsForLabeledStatement(
      stmt: LabeledStmt,
      scopeContext: ScopeContext,
      order: Int
  ): Seq[AstWithCtx] = {
    val jumpTargetAst = Ast(NewJumpTarget().name(stmt.getLabel.toString).order(order))
    val stmtAstWithCtx = astsForStatement(stmt.getStatement, scopeContext, order = order + 1)

    Seq(AstWithCtx(jumpTargetAst, Context())) ++ stmtAstWithCtx
  }

  def astForThrow(stmt: ThrowStmt, scopeContext: ScopeContext, order: Int): AstWithCtx = {
    val throwNode = NewCall()
      .name("operator.<throw>")
      .methodFullName("operator.<throw>")
      .lineNumber(line(stmt))
      .columnNumber(column(stmt))
      .code(stmt.toString())
      .order(order)
      .argumentIndex(order)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)

    val args = astsForExpression(stmt.getExpression, scopeContext, order = 1)

    callAst(throwNode, args)
  }

  def astForCatchClause(
      catchClause: CatchClause,
      scopeContext: ScopeContext,
      order: Int
  ): AstWithCtx = {
    astForBlockStatement(catchClause.getBody, scopeContext, order)
  }

  def astForTry(stmt: TryStmt, scopeContext: ScopeContext, order: Int): AstWithCtx = {
    // TODO: Handle try body
    val tryNode = NewControlStructure()
      .controlStructureType(ControlStructureTypes.TRY)
      .code("try")
      .order(order)
      .argumentIndex(order)
      .lineNumber(line(stmt))
      .columnNumber(column(stmt))

    val tryAst = astForBlockStatement(stmt.getTryBlock, scopeContext, 1, "try");
    // Catch order must be 2 for CFG generation
    val catchAsts = withOrder(stmt.getCatchClauses) { (s, o) =>
      astForCatchClause(s, scopeContext, o)
    }
    val catchBlock = Ast(NewBlock().order(2).argumentIndex(2).code("catch"))
      .withChildren(catchAsts.map(_.ast))
    // Finally order must be 3 for CFG generation
    val finallyAst =
      stmt.getFinallyBlock.toScala.map(astForBlockStatement(_, scopeContext, 3, "finally")).toList

    val ast = Ast(tryNode)
      .withChild(tryAst.ast)
      .withChild(catchBlock)
      .withChildren(finallyAst.map(_.ast))

    val ctx = tryAst.ctx.mergeWith(catchAsts.map(_.ctx)).mergeWith(finallyAst.map(_.ctx))

    AstWithCtx(ast, ctx)
  }

  private def astsForStatement(
      statement: Statement,
      scopeContext: ScopeContext,
      order: Int
  ): Seq[AstWithCtx] = {
    statement match {
      case x: ExplicitConstructorInvocationStmt =>
        Seq(astForExplicitConstructorInvocation(x, scopeContext, order))
      case x: AssertStmt                 => Seq(astForAssertStatement(x, scopeContext, order))
      case x: BlockStmt                  => Seq(astForBlockStatement(x, scopeContext, order))
      case x: BreakStmt                  => Seq(astForBreakStatement(x, order))
      case x: ContinueStmt               => Seq(astForContinueStatement(x, order))
      case x: DoStmt                     => Seq(astForDo(x, scopeContext, order))
      case _: EmptyStmt                  => Seq() // Intentionally skipping this
      case x: ExpressionStmt             => astsForExpression(x.getExpression, scopeContext, order)
      case x: ForEachStmt                => Seq(astForForEach(x, scopeContext, order))
      case x: ForStmt                    => Seq(astForFor(x, scopeContext, order))
      case x: IfStmt                     => Seq(astForIf(x, scopeContext, order))
      case x: LabeledStmt                => astsForLabeledStatement(x, scopeContext, order)
      case _: LocalClassDeclarationStmt  => Seq()
      case _: LocalRecordDeclarationStmt => Seq()
      case x: ReturnStmt                 => astsForReturnNode(x, scopeContext, order)
      case x: SwitchStmt                 => Seq(astForSwitchStatement(x, scopeContext, order))
      case _: SynchronizedStmt           => Seq()
      case x: ThrowStmt                  => Seq(astForThrow(x, scopeContext, order))
      case x: TryStmt                    => Seq(astForTry(x, scopeContext, order))
      case _: UnparsableStmt             => Seq() // TODO: log a warning
      case x: WhileStmt                  => Seq(astForWhile(x, scopeContext, order))
      case _: YieldStmt                  => Seq()
      case _                             => Seq()
    }
  }

  private def astForElse(
      maybeStmt: Option[Statement],
      scopeContext: ScopeContext,
      order: Int
  ): Option[AstWithCtx] = {
    maybeStmt.map { stmt =>
      val elseAstsWithCtx = astsForStatement(stmt, scopeContext, 1)

      val elseNode =
        NewControlStructure()
          .controlStructureType(ControlStructureTypes.ELSE)
          .order(order)
          .argumentIndex(order)
          .lineNumber(line(stmt))
          .columnNumber(column(stmt))
          .code("else")

      AstWithCtx(
        Ast(elseNode).withChildren(elseAstsWithCtx.map(_.ast)),
        mergedCtx(elseAstsWithCtx.map(_.ctx))
      )
    }
  }

  def astForIf(stmt: IfStmt, scopeContext: ScopeContext, order: Int): AstWithCtx = {
    val ifNode =
      NewControlStructure()
        .controlStructureType(ControlStructureTypes.IF)
        .order(order)
        .argumentIndex(order)
        .lineNumber(line(stmt))
        .columnNumber(column(stmt))
        .code(s"if (${stmt.getCondition.toString})")

    val conditionAstWithCtx =
      astsForExpression(stmt.getCondition, scopeContext, order = 1).headOption
        .getOrElse(AstWithCtx.empty)

    val thenAstsWithCtx = astsForStatement(stmt.getThenStmt, scopeContext, order = 2)
    val elseAstWithCtx = astForElse(stmt.getElseStmt.toScala, scopeContext, order = 3)

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

  def astForWhile(stmt: WhileStmt, scopeContext: ScopeContext, order: Int): AstWithCtx = {
    val whileNode =
      NewControlStructure()
        .controlStructureType(ControlStructureTypes.WHILE)
        .order(order)
        .argumentIndex(order)
        .lineNumber(line(stmt))
        .columnNumber(column(stmt))
        .code(s"while (${stmt.getCondition.toString})")

    val conditionAstWithCtx =
      astsForExpression(stmt.getCondition, scopeContext, order = 1).headOption
        .getOrElse(AstWithCtx.empty)
    val stmtAstsWithCtx = astsForStatement(stmt.getBody, scopeContext, order = 2)

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

  def astForDo(stmt: DoStmt, scopeContext: ScopeContext, order: Int): AstWithCtx = {
    val doNode =
      NewControlStructure().controlStructureType(ControlStructureTypes.DO).order(order)
    val conditionAstWithCtx =
      astsForExpression(stmt.getCondition, scopeContext, order = 0).headOption
        .getOrElse(AstWithCtx.empty)
    val stmtAstsWithCtx = astsForStatement(stmt.getBody, scopeContext, order = 1)
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
    val init = stmt.getInitialization.asScala.map(_.toString).mkString(", ")
    val compare = stmt.getCompare.toScala.map(_.toString)
    val update = stmt.getUpdate.asScala.map(_.toString).mkString(", ")
    s"for ($init; $compare; $update)"
  }
  def astForFor(stmt: ForStmt, scopeContext: ScopeContext, order: Int): AstWithCtx = {
    val forNode =
      NewControlStructure()
        .controlStructureType(ControlStructureTypes.FOR)
        .order(order)
        .argumentIndex(order)
        .code(getForCode(stmt))
        .lineNumber(line(stmt))
        .columnNumber(column(stmt))

    val (initAstsWithCtx, scopeCtxWithInit) =
      withOrderAndCtx(stmt.getInitialization.asScala, scopeContext) { (s, scopeCtx, o) =>
        astsForExpression(s, scopeCtx, o)
      }

    val (compareAstsWithCtx, scopeCtxWithComp) =
      withOrderAndCtx(stmt.getCompare.toScala, scopeCtxWithInit, initAstsWithCtx.size + 1) { (x, scopeCtx, o) =>
        astsForExpression(x, scopeCtx, o)
      }

    val newOrder = initAstsWithCtx.size + compareAstsWithCtx.size
    val (updateAstsWithCtx, scopeCtxWithUpdt) =
      withOrderAndCtx(stmt.getUpdate.asScala, scopeCtxWithComp, newOrder + 1) { (x, scopeCtx, o) =>
        astsForExpression(x, scopeCtx, o)
      }

    val stmtAstsWithCtx =
      astsForStatement(stmt.getBody, scopeCtxWithUpdt, newOrder + compareAstsWithCtx.size + 1)

    val ast = Ast(forNode)
      .withChildren(initAstsWithCtx.map(_.ast))
      .withChildren(compareAstsWithCtx.map(_.ast))
      .withChildren(updateAstsWithCtx.map(_.ast))
      .withChildren(stmtAstsWithCtx.map(_.ast))

    val forAst = compareAstsWithCtx.flatMap(_.ast.root) match {
      case List(c) =>
        ast.withConditionEdge(forNode, c)
      case _ => ast
    }
    val ctx = mergedCtx(
      (initAstsWithCtx ++ compareAstsWithCtx ++ updateAstsWithCtx ++ stmtAstsWithCtx).map(_.ctx)
    )

    AstWithCtx(forAst, ctx)
  }

  def astForForEach(stmt: ForEachStmt, scopeContext: ScopeContext, order: Int): AstWithCtx = {
    val forNode = NewControlStructure()
      .controlStructureType(ControlStructureTypes.FOR)
      .order(order)

    val iterableAstsWithCtx = astsForExpression(stmt.getIterable, scopeContext, 1)
    val variableAstsWithCtx =
      astsForVariableDecl(stmt.getVariable, scopeContext, iterableAstsWithCtx.size + 1)
    val initContext = mergedCtx((iterableAstsWithCtx ++ variableAstsWithCtx).map(_.ctx))
    val scopeCtxWithVars = scopeContext.withNewLocals(initContext.locals)

    val bodyOrder = iterableAstsWithCtx.size + variableAstsWithCtx.size + 1
    val bodyAstsWithCtx = astsForStatement(stmt.getBody, scopeCtxWithVars, bodyOrder)

    val forEachAst = Ast(forNode)
      .withChildren(iterableAstsWithCtx.map(_.ast))
      .withChildren(variableAstsWithCtx.map(_.ast))
      .withChildren(bodyAstsWithCtx.map(_.ast))
    val ctx = mergedCtx((iterableAstsWithCtx ++ variableAstsWithCtx ++ bodyAstsWithCtx).map(_.ctx))

    AstWithCtx(forEachAst, ctx)
  }

  def astForSwitchStatement(
      stmt: SwitchStmt,
      scopeContext: ScopeContext,
      order: Int
  ): AstWithCtx = {
    val switchNode =
      NewControlStructure()
        .controlStructureType(ControlStructureTypes.SWITCH)
        .order(order)
        .argumentIndex(order)
        .code(s"switch(${stmt.getSelector.toString})")

    val selectorAstsWithCtx = astsForExpression(stmt.getSelector, scopeContext, 1)
    val selectorNode = selectorAstsWithCtx.head.ast.root.get

    val (entryAstsWithCtx, _) = withOrderAndCtx(stmt.getEntries.asScala, scopeContext) { (e, scopeCtx, o) =>
      astForSwitchEntry(e, scopeCtx, o)
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

  private def astsForSwitchCases(
      entry: SwitchEntry,
      scopeContext: ScopeContext,
      order: Int
  ): Seq[Ast] = {
    entry.getLabels.asScala.toList match {
      case Nil =>
        val target = NewJumpTarget()
          .name("default")
          .code("default")
          .order(order)
          .argumentIndex(order)
        Seq(Ast(target))

      case labels =>
        labels.zipWithIndex.flatMap {
          case (label, idx) =>
            val labelOrder = order + idx
            val jumpTarget = NewJumpTarget()
              .name("case")
              .code(label.toString)
              .order(labelOrder)
              .argumentIndex(labelOrder)
            val labelAsts = astsForExpression(label, scopeContext, labelOrder)

            Seq(Ast(jumpTarget)) ++ labelAsts.map(_.ast)
        }
    }
  }

  def astForSwitchEntry(
      entry: SwitchEntry,
      scopeContext: ScopeContext,
      order: Int
  ): Seq[AstWithCtx] = {
    val labelAsts = astsForSwitchCases(entry, scopeContext, order)

    val statementOrder = order + entry.getLabels.size
    val (statementAstsWithCtx, _) =
      withOrderAndCtx(entry.getStatements.asScala, scopeContext, statementOrder) { (s, scopeCtx, o) =>
        astsForStatement(s, scopeCtx, o)
      }

    val labelAstsWithCtx = labelAsts.map(AstWithCtx(_, Context()))
    labelAstsWithCtx ++ statementAstsWithCtx
  }

  private def astForAssertStatement(
      stmt: AssertStmt,
      scopeContext: ScopeContext,
      order: Int
  ): AstWithCtx = {
    val callNode = NewCall()
      .name("assert")
      .methodFullName("assert")
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .code(stmt.toString)
      .argumentIndex(order)
      .order(order)
      .lineNumber(line(stmt))
      .columnNumber(column(stmt))

    val args = astsForExpression(stmt.getCheck, scopeContext, 1)
    callAst(callNode, args)
  }

  private def astForBlockStatement(
      stmt: BlockStmt,
      scopeContext: ScopeContext,
      order: Int,
      codeStr: String = "<empty>"
  ): AstWithCtx = {
    val block = NewBlock()
      .order(order)
      .code(codeStr)
      .argumentIndex(order)
      .lineNumber(line(stmt))
      .columnNumber(column(stmt))

    val (stmtAstsWithCtx, _) = withOrderAndCtx(stmt.getStatements.asScala, scopeContext) { (x, scopeCtx, o) =>
      astsForStatement(x, scopeCtx, o)
    }

    val blockAst = Ast(block).withChildren(stmtAstsWithCtx.map(_.ast))
    val ctx = mergedCtx(stmtAstsWithCtx.map(_.ctx))

    AstWithCtx(blockAst, ctx)
  }

  private def astsForReturnNode(
      ret: ReturnStmt,
      scopeContext: ScopeContext,
      order: Int
  ): Seq[AstWithCtx] = {
    // TODO: Make return node with expression as children
    if (ret.getExpression.isPresent) {
      val exprAstsWithCtx = astsForExpression(ret.getExpression.get(), scopeContext, order + 1)
      val returnNode = NewReturn().order(order)
      val returnAst = Ast(returnNode).withChildren(exprAstsWithCtx.map(_.ast))
      val ctx = mergedCtx(exprAstsWithCtx.map(_.ctx))
      Seq(AstWithCtx(returnAst, ctx))
    } else {
      Seq()
    }
  }

  def astForUnaryExpr(stmt: UnaryExpr, scopeContext: ScopeContext, order: Int): AstWithCtx = {
    val operatorName = stmt.getOperator match {
      case UnaryExpr.Operator.LOGICAL_COMPLEMENT => Operators.logicalNot
      case UnaryExpr.Operator.POSTFIX_DECREMENT  => Operators.postDecrement
      case UnaryExpr.Operator.POSTFIX_INCREMENT  => Operators.postIncrement
      case UnaryExpr.Operator.PREFIX_DECREMENT   => Operators.preDecrement
      case UnaryExpr.Operator.PREFIX_INCREMENT   => Operators.preIncrement
      case UnaryExpr.Operator.BITWISE_COMPLEMENT => Operators.not
      case UnaryExpr.Operator.PLUS               => Operators.plus
      case UnaryExpr.Operator.MINUS              => Operators.minus
    }

    val callNode = NewCall()
      .name(operatorName)
      .methodFullName(operatorName)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .code(stmt.toString)
      .argumentIndex(order)
      .order(order)

    val argsWithCtx = astsForExpression(stmt.getExpression, scopeContext, 1)
    callAst(callNode, argsWithCtx)
  }

  def astForArrayAccessExpr(
      expr: ArrayAccessExpr,
      scopeContext: ScopeContext,
      order: Int
  ): AstWithCtx = {
    val callNode = NewCall()
      .name(Operators.indexAccess)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .code(expr.toString)
      .order(order)
      .argumentIndex(order)
      .methodFullName(Operators.indexAccess)
      .lineNumber(line(expr))
      .columnNumber(column(expr))

    val argsWithCtx = astsForExpression(expr.getName, scopeContext, 1) ++ astsForExpression(
      expr.getIndex,
      scopeContext,
      2
    )
    callAst(callNode, argsWithCtx)
  }

  def astForArrayCreationExpr(
      expr: ArrayCreationExpr,
      scopeContext: ScopeContext,
      order: Int
  ): AstWithCtx = {
    val name = "<operator>.arrayCreator"
    val callNode = NewCall()
      .name(name)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .code(expr.toString)
      .order(order)
      .argumentIndex(order)
      .methodFullName(name)
      .lineNumber(line(expr))
      .columnNumber(column(expr))

    val levelAsts = expr.getLevels.asScala.zipWithIndex.flatMap {
      case (lvl, idx) =>
        lvl.getDimension.toScala match {
          case Some(dimension) => astsForExpression(dimension, scopeContext, idx + 1)

          case None => Seq.empty
        }
    }

    val initializerAstWithCtx =
      expr.getInitializer.toScala
        .map(astForArrayInitializerExpr(_, scopeContext, expr.getLevels.size() + 1))
        .getOrElse(AstWithCtx.empty)

    val argsWithCtx = (levelAsts ++ List(initializerAstWithCtx)).toSeq

    callAst(callNode, argsWithCtx)
  }

  def astForArrayInitializerExpr(
      expr: ArrayInitializerExpr,
      scopeContext: ScopeContext,
      order: Int
  ): AstWithCtx = {
    val callNode = NewCall()
      .name("<operator>.arrayInitializer")
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .code(expr.toString)
      .order(order)
      .argumentIndex(order)
      .methodFullName("<operator>.arrayInitializer")
      .lineNumber(line(expr))
      .columnNumber(column(expr))

    val MAX_INITIALIZERS = 1000

    val argsWithCtx = expr.getValues.asScala
      .slice(0, MAX_INITIALIZERS)
      .zipWithIndex
      .flatMap {
        case (c, o) =>
          astsForExpression(c, scopeContext, o)
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

  def astForBinaryExpr(stmt: BinaryExpr, scopeContext: ScopeContext, order: Int): AstWithCtx = {
    val operatorName = stmt.getOperator match {
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
      case _                                        => ""
    }

    val callNode = NewCall()
      .name(operatorName)
      .methodFullName(operatorName)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .code(stmt.toString)
      .argumentIndex(order)
      .order(order)

    val argsWithCtx = astsForExpression(stmt.getLeft, scopeContext, 1) ++ astsForExpression(
      stmt.getRight,
      scopeContext,
      2
    )

    callAst(callNode, argsWithCtx)
  }

  def astForAssignExpr(expr: AssignExpr, scopeContext: ScopeContext, order: Int): AstWithCtx = {
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

    def callNode =
      NewCall()
        .name(methodName)
        .methodFullName(methodName)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
        .code(expr.toString)
        .argumentIndex(order)
        .order(order)
        .dispatchType(DispatchTypes.STATIC_DISPATCH)

    val args = astsForExpression(expr.getTarget, scopeContext, 1) ++ astsForExpression(
      expr.getValue,
      scopeContext,
      2
    )
    callAst(callNode, args)
  }

  private def localsForVarDecl(varDecl: VariableDeclarationExpr, order: Int): List[NewLocal] = {
    varDecl.getVariables.asScala.zipWithIndex.map {
      case (variable, idx) =>
        val name = variable.getName.toString
        val typeFullName = registerType(tryResolveType(variable))
        val code = s"${variable.getType} $name"

        NewLocal().name(name).code(code).typeFullName(typeFullName).order(order + idx)
    }.toList
  }

  private def getInitializerType(variable: VariableDeclarator): String = {
    variable.getInitializer.toScala match {
      case None => "<empty>"

      case Some(initializer) =>
        try {
          registerType(initializer.calculateResolvedType().describe())
        } catch {
          case _: Throwable =>
            try {
              registerType(tryResolveType(variable))
            } catch {
              case _: Throwable => registerType("<empty>")
            }
        }
    }
  }

  private def assignmentsForVarDecl(
      varDecl: VariableDeclarationExpr,
      scopeContext: ScopeContext,
      order: Int
  ): Seq[AstWithCtx] = {
    val variablesWithInitializers =
      varDecl.getVariables.asScala.filter(_.getInitializer.toScala.isDefined)
    val assignments = variablesWithInitializers.zipWithIndex map {
      case (variable, idx) =>
        val name = variable.getName.toString
        val initializer = variable.getInitializer.toScala.get // Won't crash because of filter
        val initializerTypeFullName = getInitializerType(variable)

        val callNode = NewCall()
          .name(Operators.assignment)
          .code(s"$name = ${initializer.toString()}")
          .order(order + idx)
          .argumentIndex(order + idx)
          .lineNumber(line(varDecl))
          .columnNumber(column(varDecl))
          .typeFullName(tryResolveType(variable))
          .dispatchType(DispatchTypes.STATIC_DISPATCH)

        val identifier = NewIdentifier()
          .name(name)
          .order(1)
          .argumentIndex(1)
          .code(name)
          .typeFullName(initializerTypeFullName)
          .lineNumber(line(variable))
          .columnNumber(column(variable))
        val identifierAst = AstWithCtx(Ast(identifier), Context(identifiers = Seq(identifier)))

        val initializerAstsWithCtx = astsForExpression(initializer, scopeContext, 2)

        callAst(callNode, Seq(identifierAst) ++ initializerAstsWithCtx)
    }

    assignments.toList
  }

  def astsForVariableDecl(
      varDecl: VariableDeclarationExpr,
      scopeContext: ScopeContext,
      order: Int
  ): Seq[AstWithCtx] = {

    val locals = localsForVarDecl(varDecl, order)
    val localAsts = locals.map { local =>
      AstWithCtx(
        Ast(local),
        Context(locals = Seq(local))
      )
    }

    val assignOrder = order + locals.size
    val assignScopeCtx = scopeContext.withNewLocals(locals)
    val assignmentsWithCtx = assignmentsForVarDecl(varDecl, assignScopeCtx, assignOrder)

    localAsts ++ assignmentsWithCtx
  }

  def callAst(rootNode: NewNode, args: Seq[AstWithCtx]): AstWithCtx = {
    val asts = args.map(_.ast)
    val ctx = mergedCtx(args.map(_.ctx))
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
      .typeFullName("ANY")
      .code(expr.getTypeAsString)
      .lineNumber(line(expr))
      .columnNumber(column(expr))
      .argumentIndex(1)
      .order(1)
    val idAstWithCtx = AstWithCtx(Ast(identifier), Context(identifiers = List(identifier)))

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

  def astForConditionalExpr(
      expr: ConditionalExpr,
      scopeContext: ScopeContext,
      order: Int
  ): AstWithCtx = {
    val callNode = NewCall()
      .name(Operators.conditional)
      .methodFullName(Operators.conditional)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .code(expr.toString)
      .argumentIndex(order)
      .order(order)
      .lineNumber(line(expr))
      .columnNumber(column(expr))

    val condAst = astsForExpression(expr.getCondition, scopeContext, 1)
    val thenAst = astsForExpression(expr.getThenExpr, scopeContext, 2)
    val elseAst = astsForExpression(expr.getElseExpr, scopeContext, 3)

    callAst(callNode, condAst ++ thenAst ++ elseAst)
  }

  def astForEnclosedExpression(
      expr: EnclosedExpr,
      scopeContext: ScopeContext,
      order: Int
  ): Seq[AstWithCtx] = {
    astsForExpression(expr.getInner, scopeContext, order)
  }

  def astForFieldAccessExpr(
      expr: FieldAccessExpr,
      scopeContext: ScopeContext,
      order: Int
  ): AstWithCtx = {
    val callNode = NewCall()
      .name(Operators.fieldAccess)
      .methodFullName(Operators.fieldAccess)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .code(expr.toString)
      .argumentIndex(order)
      .order(order)
      .lineNumber(line(expr))
      .columnNumber(column(expr))

    val fieldIdentifier = expr.getName
    val identifierAsts = astsForExpression(expr.getScope, scopeContext, 1)
    val fieldIdentifierNode = NewFieldIdentifier()
      .canonicalName(fieldIdentifier.toString)
      .argumentIndex(2)
      .order(2)
      .lineNumber(line(fieldIdentifier))
      .columnNumber(column(fieldIdentifier))
      .code(fieldIdentifier.toString)
    val fieldIdAstsWithCtx = AstWithCtx(Ast(fieldIdentifierNode), Context())

    callAst(callNode, Seq(fieldIdAstsWithCtx) ++ identifierAsts)
  }

  def astForNameExpr(x: NameExpr, order: Int): AstWithCtx = {
    val name = x.getName.toString
    val typeFullName =
      try {
        registerType(x.resolve().getType.describe())
      } catch {
        case _: Throwable =>
          // TODO: This is a hack to deal with static field accesses. Need to figure out how to deal with this properly.
          registerType(s"class ${x.getName.toString}")
      }
    val identifier = NewIdentifier()
      .name(name)
      .order(order)
      .argumentIndex(order)
      .code(name)
      .typeFullName(typeFullName)
      .lineNumber(line(x.getName))
      .columnNumber(column(x.getName))

    AstWithCtx(Ast(identifier), Context(identifiers = List(identifier)))
  }

  def astForObjectCreationExpr(
      expr: ObjectCreationExpr,
      scopeContext: ScopeContext,
      order: Int
  ): AstWithCtx = {
    // TODO: Decide on a final name for this.
    val name = s"<constructor>.${expr.getTypeAsString}"
    val callNode = NewCall()
      .name(name)
      .methodFullName(name)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .order(order)
      .argumentIndex(order)

    val args = withOrder(expr.getArguments) { (x, o) =>
      astsForExpression(x, scopeContext, o)
    }.flatten

    callAst(callNode, args)
  }

  private def astForExplicitConstructorInvocation(
      stmt: ExplicitConstructorInvocationStmt,
      scopeContext: ScopeContext,
      order: Int
  ): AstWithCtx = {
    val name = Try(
      stmt.resolve().getQualifiedName
    ).getOrElse(
      // TODO: add an unresolved guess here
      s"<empty>"
    )
    val callNode = NewCall()
      .name(name)
      .methodFullName(name)
      .argumentIndex(order)
      .order(order)
      .code(stmt.toString)
      .lineNumber(line(stmt))
      .columnNumber(column(stmt))
      .dispatchType(DispatchTypes.STATIC_DISPATCH)

    val args = withOrder(stmt.getArguments) { (s, o) =>
      astsForExpression(s, scopeContext, o)
    }.flatten

    callAst(callNode, args)
  }

  private def astsForExpression(
      expression: Expression,
      scopeContext: ScopeContext,
      order: Int
  ): Seq[AstWithCtx] = {
    expression match {
      case _: AnnotationExpr          => Seq()
      case x: ArrayAccessExpr         => Seq(astForArrayAccessExpr(x, scopeContext, order))
      case x: ArrayCreationExpr       => Seq(astForArrayCreationExpr(x, scopeContext, order))
      case x: ArrayInitializerExpr    => Seq(astForArrayInitializerExpr(x, scopeContext, order))
      case x: AssignExpr              => Seq(astForAssignExpr(x, scopeContext, order))
      case x: BinaryExpr              => Seq(astForBinaryExpr(x, scopeContext, order))
      case _: CastExpr                => Seq()
      case x: ClassExpr               => Seq(astForClassExpr(x, order))
      case x: ConditionalExpr         => Seq(astForConditionalExpr(x, scopeContext, order))
      case x: EnclosedExpr            => astForEnclosedExpression(x, scopeContext, order)
      case x: FieldAccessExpr         => Seq(astForFieldAccessExpr(x, scopeContext, order))
      case _: InstanceOfExpr          => Seq()
      case x: LambdaExpr              => Seq(astForLambdaExpr(x, scopeContext, order))
      case x: LiteralExpr             => Seq(astForLiteralExpr(x, order))
      case x: MethodCallExpr          => Seq(astForMethodCall(x, scopeContext, order))
      case _: MethodReferenceExpr     => Seq()
      case x: NameExpr                => Seq(astForNameExpr(x, order))
      case x: ObjectCreationExpr      => Seq(astForObjectCreationExpr(x, scopeContext, order))
      case _: PatternExpr             => Seq()
      case _: SuperExpr               => Seq()
      case _: SwitchExpr              => Seq()
      case _: ThisExpr                => Seq()
      case _: TypeExpr                => Seq()
      case x: UnaryExpr               => Seq(astForUnaryExpr(x, scopeContext, order))
      case x: VariableDeclarationExpr => astsForVariableDecl(x, scopeContext, order)
    }
  }

  private def createCallSignature(decl: ResolvedMethodDeclaration): String = {
    val returnType = Try(decl.getReturnType.describe()).toOption.getOrElse("<empty>")

    val paramTypes =
      for (i <- 0 until decl.getNumberOfParams)
        yield Try(decl.getParam(i).getType.describe()).toOption.getOrElse("<empty>")

    s"$returnType(${paramTypes.mkString(",")})"
  }

  private def createCallNode(
      call: MethodCallExpr,
      resolvedDecl: Try[ResolvedMethodDeclaration],
      order: Int
  ) = {
    val callNode = NewCall()
      .name(call.getNameAsString)
      .code(s"${call.getNameAsString}(${call.getArguments.asScala.mkString(", ")})")
      .order(order)
      .argumentIndex(order)
    resolvedDecl match {
      case Success(resolved) =>
        val signature = createCallSignature(resolved)
        callNode.methodFullName(s"${resolved.getQualifiedName}:$signature")
        callNode.signature(signature)
        callNode.dispatchType(DispatchTypes.STATIC_DISPATCH)
      case Failure(_) => // TODO: Logging

    }
    if (call.getName.getBegin.isPresent) {
      callNode
        .lineNumber(line(call.getName))
        .columnNumber(column(call.getName))
    }
    callNode
  }

  private def createThisNode(
      resolvedDecl: Try[ResolvedMethodDeclaration]
  ): Option[NewIdentifier] = {
    resolvedDecl.toOption
      .filterNot(_.isStatic)
      .map { resolved =>
        NewIdentifier()
          .name("this")
          .code("this")
          .typeFullName(resolved.declaringType().getQualifiedName)
          .order(0)
          .argumentIndex(0)
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

  private def astForLambdaExpr(
      expr: LambdaExpr,
      scopeContext: ScopeContext,
      order: Int
  ): AstWithCtx = {
    // TODO: Fix class name (currently `com.github.javaparser.ast.expr.LambdaExpr`)
    val className = scopeContext.typeDecl.map(_.fullName).getOrElse("<empty>")
    val fullName = s"$className:${nextLambdaName()}"

    val parameterAstsWithCtx = astsForParameterList(expr.getParameters)
    val namesToMethodParams = mapNamesToParams(parameterAstsWithCtx)

    val lambdaScopeCtx =
      scopeContext.withNewParams(parameterAstsWithCtx.flatMap(_.ctx.methodParameters))
    val bodyAstWithCtx =
      astsForStatement(expr.getBody, lambdaScopeCtx, parameterAstsWithCtx.size + 2).head

    val (identifiersMatchingParams, identifiersNotMatchingParams) = {
      bodyAstWithCtx.ctx.identifiers.partition(identifier => namesToMethodParams.contains(identifier.name))
    }

    val closureBindings = closureBindingsForLambdas(identifiersNotMatchingParams)
    val refEdgePairs = buildRefEdgePairs(identifiersMatchingParams, namesToMethodParams)

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

    val closuresWithMeta = buildClosuresWithMeta(closureBindings, scopeContext, methodRef)

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
      scopeContext: ScopeContext,
      methodRef: NewMethodRef
  ): Seq[ClosureBindingMeta] = {
    val nameToNode: Map[String, NewNode] =
      (scopeContext.methodParameters ++ scopeContext.locals).map { p =>
        p.name -> p
      }.toMap

    closureBindings.map { closureBindingInfo =>
      val name = closureBindingInfo.identifier.name
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

  private def closureBindingsForLambdas(
      identifiers: Seq[NewIdentifier]
  ): Seq[ClosureBindingInfo] = {
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
    val signature = lambdaSignature(expr.getParameters.asScala.toList)
    val lineNumber = line(expr)
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
      closureBindings.zipWithIndex.map {
        case (bindingWithInfo, idx) =>
          val identifier = bindingWithInfo.identifier
          Ast(
            NewLocal()
              .name(identifier.name)
              .code(identifier.code)
              .typeFullName(identifier.typeFullName)
              .lineNumber(identifier.lineNumber)
              .columnNumber(identifier.columnNumber)
              .closureBindingId(bindingWithInfo.bindingId)
              .order(idx + 1)
          )
      }

    val methodReturnOrder = parameterAsts.size + 2
    val methodReturnNode =
      NewMethodReturn()
        .order(methodReturnOrder)
        .evaluationStrategy(EvaluationStrategies.BY_VALUE)
        .typeFullName("ANY")
        .code("RET")
        .lineNumber(lineNumber)
        .columnNumber(columnNumber)

    val lambdaMethodAst = Ast(lambdaMethodNode)
      .withChildren(parameterAsts)
      .withChild(bodyAst.withChildren(localsForCapturedIdentifiers))
      .withChild(Ast(methodReturnNode))

    val lambdaMethodAstWithRefEdges = refEdgePairs.foldLeft(lambdaMethodAst)((acc, edgePair) => {
      acc.withRefEdge(edgePair.from, edgePair.to)
    })

    lambdaMethodAstWithRefEdges
  }

  private def astForLiteralExpr(expr: LiteralExpr, order: Int = 1): AstWithCtx = {
    val literalType = expr match {
      case _: BooleanLiteralExpr   => "boolean"
      case _: CharLiteralExpr      => "char"
      case _: DoubleLiteralExpr    => "double"
      case _: IntegerLiteralExpr   => "int"
      case _: LongLiteralExpr      => "long"
      case _: NullLiteralExpr      => "null"
      case _: StringLiteralExpr    => "java.lang.String"
      case _: TextBlockLiteralExpr => "java.lang.String"
      case _                       => "<empty>"
    }

    registerType(literalType)
    AstWithCtx(
      Ast(
        NewLiteral().order(order).argumentIndex(order).code(expr.toString).typeFullName(literalType)
      ),
      Context()
    )
  }

  private def astForMethodCall(
      call: MethodCallExpr,
      scopeContext: ScopeContext,
      order: Int = 1
  ): AstWithCtx = {

    val resolvedDecl = Try(call.resolve())
    val callNode = createCallNode(call, resolvedDecl, order)
    val thisAsts = createThisNode(resolvedDecl)
      .map(_.lineNumber(line(call)))
      .map(_.columnNumber(column(call)))
      .map(x => AstWithCtx(Ast(x), Context(identifiers = List(x))))
      .toList

    val argAsts = withOrder(call.getArguments) { (arg, order) =>
      // FIXME: There's an implicit assumption here that each call to
      // astsForExpression only returns a single tree.
      astsForExpression(arg, scopeContext, order)
    }.flatten

    val ast = Ast(callNode)
      .withChildren(thisAsts.map(_.ast))
      .withChildren(argAsts.map(_.ast))
      .withArgEdges(callNode, thisAsts.flatMap(_.ast.root))
      .withArgEdges(callNode, argAsts.flatMap(_.ast.root))

    val ctx = mergedCtx((thisAsts ++ argAsts).map(_.ctx))

    AstWithCtx(ast, ctx)
  }

  private def tryResolveType(node: NodeWithType[_, _ <: Resolvable[ResolvedType]]): String = {
    try {
      node.getType.resolve().describe()
    } catch {
      case _: Throwable =>
        s"<unresolved>.${node.getType}"
    }
  }

  private def astsForParameterList(
      parameters: NodeList[Parameter],
      order: Int = 0
  ): Seq[AstWithCtx] = {
    withOrder(parameters) { (p, o) =>
      astForParameter(p, order + o)
    }
  }

  private def astForParameter(parameter: Parameter, childNum: Int): AstWithCtx = {
    val typeFullName = registerType(tryResolveType(parameter))
    val parameterNode = NewMethodParameterIn()
      .name(parameter.getName.toString)
      .code(parameter.toString)
      .typeFullName(typeFullName)
      .order(childNum)
      .lineNumber(line(parameter))
      .columnNumber(column(parameter))
    val ast = Ast(parameterNode)
    AstWithCtx(ast, Context(methodParameters = List(parameterNode)))
  }

  private def methodFullName(
      typeDecl: Option[NewTypeDecl],
      methodDeclaration: MethodDeclaration
  ): String = {
    val typeName = typeDecl.map(_.fullName).getOrElse("")
    val returnType = tryResolveType(methodDeclaration)
    val methodName = methodDeclaration.getNameAsString
    s"$typeName.$methodName:$returnType${paramListSignature(methodDeclaration)}"
  }

  private def constructorFullName(
      typeDecl: Option[NewTypeDecl],
      constructorDeclaration: ConstructorDeclaration
  ): String = {
    val typeName = typeDecl.map(_.fullName).getOrElse("")
    val methodName = constructorDeclaration.getNameAsString
    s"$typeName.$methodName:$typeName${paramListSignature(constructorDeclaration)}"
  }

  private def paramListSignature(methodDeclaration: CallableDeclaration[_]) = {
    val paramTypes = methodDeclaration.getParameters.asScala.map(tryResolveType)
    "(" + paramTypes.mkString(",") + ")"
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
    nodeList.asScala.zipWithIndex.map {
      case (x, i) =>
        f(x, i + 1)
    }.toSeq
  }

  def withOrderAndCtx[T <: Node](
      nodeList: Iterable[T],
      initialCtx: ScopeContext,
      initialOrder: Int = 1
  )(f: (T, ScopeContext, Int) => Seq[AstWithCtx]): (Seq[AstWithCtx], ScopeContext) = {
    var scopeContext = initialCtx
    var orderOffset = 0

    val asts = nodeList.flatMap { x =>
      val astsWithCtx = f(x, scopeContext, initialOrder + orderOffset)
      val ctx = mergedCtx(astsWithCtx.map(_.ctx))
      orderOffset += astsWithCtx.size
      scopeContext = scopeContext.withNewLocals(ctx.locals).withNewParams(ctx.methodParameters)
      astsWithCtx
    }.toSeq

    (asts, scopeContext)
  }
}
