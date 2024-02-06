package io.joern.rubysrc2cpg.deprecated.astcreation

import better.files.File
import io.joern.rubysrc2cpg.deprecated.parser.DeprecatedRubyParser.*
import io.joern.rubysrc2cpg.deprecated.passes.Defines
import io.joern.x2cpg.Defines.DynamicCallUnknownFullName
import io.joern.x2cpg.Imports.createImportNodeAndLink
import io.joern.x2cpg.X2Cpg.stripQuotes
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes, Operators}
import org.antlr.v4.runtime.ParserRuleContext
import org.slf4j.LoggerFactory

import scala.jdk.CollectionConverters.CollectionHasAsScala

trait AstForStatementsCreator(filename: String)(implicit withSchemaValidation: ValidationMode) {
  this: AstCreator =>

  private val logger = LoggerFactory.getLogger(this.getClass)
  private val prefixMethods = Set(
    "attr_reader",
    "attr_writer",
    "attr_accessor",
    "remove_method",
    "public_class_method",
    "private_class_method",
    "private",
    "protected",
    "module_function"
  )

  private def astForAliasStatement(ctx: AliasStatementContext): Ast = {
    val aliasName  = ctx.definedMethodNameOrSymbol(0).getText.substring(1)
    val methodName = ctx.definedMethodNameOrSymbol(1).getText.substring(1)
    methodAliases.addOne(aliasName, methodName)
    Ast()
  }

  private def astForUndefStatement(ctx: UndefStatementContext): Ast = {
    val undefNames = ctx.definedMethodNameOrSymbol().asScala.flatMap(astForDefinedMethodNameOrSymbolContext).toSeq
    val call       = callNode(ctx, code(ctx), RubyOperators.undef, RubyOperators.undef, DispatchTypes.STATIC_DISPATCH)
    callAst(call, undefNames)
  }

  private def astForBeginStatement(ctx: BeginStatementContext): Ast = {
    val stmts     = Option(ctx.compoundStatement).map(astForCompoundStatement(_)).getOrElse(Seq())
    val blockNode = NewBlock().typeFullName(Defines.Any)
    blockAst(blockNode, stmts.toList)
  }

  private def astForEndStatement(ctx: EndStatementContext): Ast = {
    val stmts     = Option(ctx.compoundStatement).map(astForCompoundStatement(_)).getOrElse(Seq())
    val blockNode = NewBlock().typeFullName(Defines.Any)
    blockAst(blockNode, stmts.toList)
  }

  private def astForModifierStatement(ctx: ModifierStatementContext): Ast = ctx.mod.getType match {
    case IF     => astForIfModifierStatement(ctx)
    case UNLESS => astForUnlessModifierStatement(ctx)
    case WHILE  => astForWhileModifierStatement(ctx)
    case UNTIL  => astForUntilModifierStatement(ctx)
    case RESCUE => astForRescueModifierStatement(ctx)
  }

  private def astForIfModifierStatement(ctx: ModifierStatementContext): Ast = {
    val lhs    = astForStatement(ctx.statement(0))
    val rhs    = astForStatement(ctx.statement(1)).headOption
    val ifNode = controlStructureNode(ctx, ControlStructureTypes.IF, code(ctx))
    lhs.headOption.flatMap(_.root) match
      // If the LHS is a `next` command with a return value, then this if statement is its condition and it becomes a
      // `return`
      case Some(x: NewControlStructure) if x.code == Defines.ModifierNext && lhs.head.nodes.size > 1 =>
        val retNode = NewReturn().code(Defines.ModifierNext).lineNumber(x.lineNumber).columnNumber(x.columnNumber)
        controlStructureAst(ifNode, rhs, Seq(lhs.head.subTreeCopy(x, replacementNode = Option(retNode))))
      case _ => controlStructureAst(ifNode, rhs, lhs)
  }

  private def astForUnlessModifierStatement(ctx: ModifierStatementContext): Ast = {
    val lhs    = astForStatement(ctx.statement(0))
    val rhs    = astForStatement(ctx.statement(1))
    val ifNode = controlStructureNode(ctx, ControlStructureTypes.IF, code(ctx))
    controlStructureAst(ifNode, lhs.headOption, rhs)
  }

  private def astForWhileModifierStatement(ctx: ModifierStatementContext): Ast = {
    val lhs = astForStatement(ctx.statement(0))
    val rhs = astForStatement(ctx.statement(1))
    whileAst(rhs.headOption, lhs, Some(text(ctx)))
  }

  private def astForUntilModifierStatement(ctx: ModifierStatementContext): Ast = {
    val lhs = astForStatement(ctx.statement(0))
    val rhs = astForStatement(ctx.statement(1))
    whileAst(rhs.headOption, lhs, Some(text(ctx)))
  }

  private def astForRescueModifierStatement(ctx: ModifierStatementContext): Ast = {
    val lhs       = astForStatement(ctx.statement(0))
    val rhs       = astForStatement(ctx.statement(1))
    val throwNode = controlStructureNode(ctx, ControlStructureTypes.THROW, code(ctx))
    controlStructureAst(throwNode, rhs.headOption, lhs)
  }

  /** If the last statement is a return, this is returned. If not, then a return node is created.
    */
  protected def lastStmtAsReturnAst(ctx: ParserRuleContext, lastStmtAst: Ast, maybeCode: Option[String] = None): Ast =
    lastStmtAst.root.collectFirst { case x: NewReturn => x } match
      case Some(_) => lastStmtAst
      case None =>
        val code    = maybeCode.getOrElse(text(ctx))
        val retNode = returnNode(ctx, code)
        lastStmtAst.root match
          case Some(method: NewMethod) => returnAst(retNode, Seq(Ast(methodToMethodRef(ctx, method))))
          case _                       => returnAst(retNode, Seq(lastStmtAst))

  protected def astForBodyStatementContext(ctx: BodyStatementContext, isMethodBody: Boolean = false): Seq[Ast] = {
    if (ctx.rescueClause.size > 0) Seq(astForRescueClause(ctx))
    else astForCompoundStatement(ctx.compoundStatement(), isMethodBody)
  }

  protected def astForCompoundStatement(
    ctx: CompoundStatementContext,
    isMethodBody: Boolean = false,
    canConsiderAsLeaf: Boolean = true
  ): Seq[Ast] = {
    val stmtAsts = Option(ctx)
      .map(_.statements())
      .map(astForStatements(_, isMethodBody, canConsiderAsLeaf))
      .getOrElse(Seq.empty)
    if (isMethodBody) {
      stmtAsts
    } else {
      Seq(blockAst(blockNode(ctx), stmtAsts.toList))
    }
  }

  /*
   * Each statement set can be considered a block. The blocks of a method can be considered to form a hierarchy.
   * We can consider the blocks structure as a n-way tree. Leaf blocks are blocks that have no more sub blocks i.e children in the
   * hierarchy. The last statement of the block of the method which is the top level/root block i.e. method body should be
   * converted into a implicit return. However, if the last statement is a if-else it has sub-blocks/child blocks and the last statement of each leaf block in it
   * will have to be converted to a implicit return, unless it is already a implicit return.
   * Some sub-blocks are exempt from their last statements being converted to returns. Examples are blocks that are arguments to functions like string interpolation.
   *
   * isMethodBody => The statement set is the top level block in the method. i.e. the root block
   * canConsiderAsLeaf => The statement set can be considered a leaf block. This is set to false by the caller when it is a statement
   * set as a part of an expression. Eg. argument in string interpolation. We do not want to construct return nodes out of
   * string interpolation arguments. These are exempt blocks for implicit returns.
   * blockChildHash => Hash of a block id to any child. Absence of a block in this after all its statements have been processed implies
   * that the block is a leaf
   * blockIdCounter => A simple counter used to assign an unique id to each block.
   */
  protected def astForStatements(
    ctx: StatementsContext,
    isMethodBody: Boolean = false,
    canConsiderAsLeaf: Boolean = true
  ): Seq[Ast] = {

    def astsForStmtCtx(stCtx: StatementContext, stmtCount: Int, stmtCounter: Int): Seq[Ast] = {
      if (isMethodBody) processingLastMethodStatement.lazySet(stmtCounter == stmtCount)
      val stAsts = astForStatement(stCtx)
      if (stAsts.nonEmpty && canConsiderAsLeaf && processingLastMethodStatement.get) {
        blockChildHash.get(currentBlockId.get) match {
          case Some(_) =>
            // this is a non-leaf block
            stAsts
          case None =>
            // this is a leaf block
            processingLastMethodStatement.lazySet(!(isMethodBody && stmtCounter == stmtCount))
            Seq(lastStmtAsReturnAst(stCtx, stAsts.head, Option(text(stCtx))))
        }
      } else {
        stAsts
      }
    }

    Option(ctx)
      .map { ctx =>
        val stmtCount     = ctx.statement.size
        val parentBlockId = currentBlockId.get
        if (canConsiderAsLeaf) blockChildHash.update(parentBlockId, currentBlockId.get)
        currentBlockId.lazySet(blockIdCounter.addAndGet(1))

        val stmtAsts = Option(ctx)
          .map(_.statement)
          .map(_.asScala)
          .getOrElse(Seq.empty)
          .zipWithIndex
          .flatMap { case (stmtCtx, idx) => astsForStmtCtx(stmtCtx, stmtCount, idx + 1) }
          .toSeq
        currentBlockId.lazySet(parentBlockId)
        stmtAsts
      }
      .getOrElse(Seq.empty)
  }

  // TODO: return Ast instead of Seq[Ast].
  private def astForStatement(ctx: StatementContext): Seq[Ast] = scanStmtForHereDoc(ctx match {
    case ctx: AliasStatementContext               => Seq(astForAliasStatement(ctx))
    case ctx: UndefStatementContext               => Seq(astForUndefStatement(ctx))
    case ctx: BeginStatementContext               => Seq(astForBeginStatement(ctx))
    case ctx: EndStatementContext                 => Seq(astForEndStatement(ctx))
    case ctx: ModifierStatementContext            => Seq(astForModifierStatement(ctx))
    case ctx: ExpressionOrCommandStatementContext => astForExpressionOrCommand(ctx.expressionOrCommand())
  })

  // TODO: return Ast instead of Seq[Ast]
  protected def astForExpressionOrCommand(ctx: ExpressionOrCommandContext): Seq[Ast] = ctx match {
    case ctx: InvocationExpressionOrCommandContext => astForInvocationExpressionOrCommandContext(ctx)
    case ctx: NotExpressionOrCommandContext        => Seq(astForNotKeywordExpressionOrCommand(ctx))
    case ctx: OrAndExpressionOrCommandContext      => Seq(astForOrAndExpressionOrCommand(ctx))
    case ctx: ExpressionExpressionOrCommandContext => astForExpressionContext(ctx.expression())
    case _ =>
      logger.error(s"astForExpressionOrCommand() $relativeFilename, ${text(ctx)} All contexts mismatched.")
      Seq(Ast())
  }

  private def astForNotKeywordExpressionOrCommand(ctx: NotExpressionOrCommandContext): Ast = {
    val exprOrCommandAst = astForExpressionOrCommand(ctx.expressionOrCommand())
    val call             = callNode(ctx, code(ctx), Operators.not, Operators.not, DispatchTypes.STATIC_DISPATCH)
    callAst(call, exprOrCommandAst)
  }

  private def astForOrAndExpressionOrCommand(ctx: OrAndExpressionOrCommandContext): Ast = ctx.op.getType match {
    case OR  => astForOrExpressionOrCommand(ctx)
    case AND => astForAndExpressionOrCommand(ctx)
  }

  private def astForOrExpressionOrCommand(ctx: OrAndExpressionOrCommandContext): Ast = {
    val argsAst = ctx.expressionOrCommand().asScala.flatMap(astForExpressionOrCommand)
    val call    = callNode(ctx, code(ctx), Operators.or, Operators.or, DispatchTypes.STATIC_DISPATCH)
    callAst(call, argsAst.toList)
  }

  private def astForAndExpressionOrCommand(ctx: OrAndExpressionOrCommandContext): Ast = {
    val argsAst = ctx.expressionOrCommand().asScala.flatMap(astForExpressionOrCommand)
    val call    = callNode(ctx, code(ctx), Operators.and, Operators.and, DispatchTypes.STATIC_DISPATCH)
    callAst(call, argsAst.toList)
  }

  private def astForSuperCommand(ctx: SuperCommandContext): Ast =
    astForSuperCall(ctx, astForArguments(ctx.argumentsWithoutParentheses().arguments()))

  private def astForYieldCommand(ctx: YieldCommandContext): Ast =
    astForYieldCall(ctx, Option(ctx.argumentsWithoutParentheses().arguments()))

  private def astForSimpleMethodCommand(ctx: SimpleMethodCommandContext): Seq[Ast] = {
    val methodIdentifierAsts = astForMethodIdentifierContext(ctx.methodIdentifier(), code(ctx))
    methodIdentifierAsts.headOption.foreach(methodNameAsIdentifierStack.push)
    val argsAsts = astForArguments(ctx.argumentsWithoutParentheses().arguments())

    /* get args without the method def in it */
    val argAstsWithoutMethods = argsAsts.filterNot(_.root.exists(_.isInstanceOf[NewMethod]))

    /* isolate methods from the original args and create identifier ASTs from it */
    val methodDefAsts = argsAsts.filter(_.root.exists(_.isInstanceOf[NewMethod]))
    val methodToIdentifierAsts = methodDefAsts.flatMap {
      _.nodes.collectFirst { case methodNode: NewMethod =>
        Ast(
          createIdentifierWithScope(
            methodNode.name,
            methodNode.name,
            Defines.Any,
            Seq.empty,
            methodNode.lineNumber,
            methodNode.columnNumber,
            definitelyIdentifier = true
          )
        )
      }
    }

    /* TODO: we add the isolated method defs later on to the parent instead */
    methodDefInArgument.addAll(methodDefAsts)

    val callNodes = methodIdentifierAsts.head.nodes.collect { case x: NewCall => x }
    if (callNodes.size == 1) {
      val callNode = callNodes.head
      if (callNode.name == "require" || callNode.name == "load") {
        resolveRequireOrLoadPath(argsAsts, callNode)
      } else if (callNode.name == "require_relative") {
        resolveRelativePath(filename, argsAsts, callNode)
      } else if (prefixMethods.contains(callNode.name)) {
        /* we remove the method definition AST from argument and add its corresponding identifier form */
        Seq(callAst(callNode, argAstsWithoutMethods ++ methodToIdentifierAsts))
      } else {
        Seq(callAst(callNode, argsAsts))
      }
    } else {
      argsAsts
    }
  }

  private def astForMemberAccessCommand(ctx: MemberAccessCommandContext): Seq[Ast] = {
    astForMethodNameContext(ctx.methodName).headOption
      .flatMap(_.root)
      .collectFirst { case x: NewCall => resolveAlias(x.name) }
      .map(methodName =>
        callNode(
          ctx,
          code(ctx),
          methodName,
          DynamicCallUnknownFullName,
          DispatchTypes.STATIC_DISPATCH,
          None,
          Option(Defines.Any)
        )
      ) match
      case Some(newCall) =>
        val primaryAst = astForPrimaryContext(ctx.primary())
        val argsAst    = astForArguments(ctx.argumentsWithoutParentheses().arguments)
        primaryAst.headOption
          .flatMap(_.root)
          .collectFirst { case x: NewMethod => x }
          .map { methodNode =>
            val methodRefNode = methodToMethodRef(ctx, methodNode)
            blockMethods.addOne(primaryAst.head)
            Seq(callAst(newCall, Seq(Ast(methodRefNode)) ++ argsAst))
          }
          .getOrElse(Seq(callAst(newCall, argsAst, primaryAst.headOption)))
      case None => Seq.empty
  }

  private def methodToMethodRef(ctx: ParserRuleContext, methodNode: NewMethod): NewMethodRef =
    methodRefNode(ctx, s"def ${methodNode.name}(...)", methodNode.fullName, Defines.Any)

  protected def astForCommand(ctx: CommandContext): Seq[Ast] = ctx match {
    case ctx: YieldCommandContext        => Seq(astForYieldCommand(ctx))
    case ctx: SuperCommandContext        => Seq(astForSuperCommand(ctx))
    case ctx: SimpleMethodCommandContext => astForSimpleMethodCommand(ctx)
    case ctx: MemberAccessCommandContext => astForMemberAccessCommand(ctx)
  }

  private def resolveRequireOrLoadPath(argsAst: Seq[Ast], callNode: NewCall): Seq[Ast] = {
    val importedNode = argsAst.headOption.map(_.nodes.collect { case x: NewLiteral => x }).getOrElse(Seq.empty)
    if (importedNode.size == 1) {
      val node      = importedNode.head
      val pathValue = stripQuotes(node.code)
      val result = pathValue match {
        case path if File(path).exists =>
          path
        case path if File(s"$path.rb").exists =>
          s"$path.rb"
        case _ =>
          pathValue
      }
      packageStack.append(result)
      val importNode = createImportNodeAndLink(result, pathValue, Some(callNode), diffGraph)
      Seq(callAst(callNode, argsAst), Ast(importNode))
    } else {
      Seq(callAst(callNode, argsAst))
    }
  }

  protected def resolveRelativePath(currentFile: String, argsAst: Seq[Ast], callNode: NewCall): Seq[Ast] = {
    val importedNode = argsAst.head.nodes.collect { case x: NewLiteral => x }
    if (importedNode.size == 1) {
      val node        = importedNode.head
      val pathValue   = stripQuotes(node.code)
      val updatedPath = if (pathValue.endsWith(".rb")) pathValue else s"$pathValue.rb"

      val currentDirectory = File(currentFile).parent
      val file             = File(currentDirectory, updatedPath)
      packageStack.append(file.pathAsString)
      val importNode = createImportNodeAndLink(updatedPath, pathValue, Some(callNode), diffGraph)
      Seq(callAst(callNode, argsAst), Ast(importNode))
    } else {
      Seq(callAst(callNode, argsAst))
    }
  }

  protected def astForBlock(ctx: BlockContext, blockMethodName: Option[String] = None): Ast = ctx match
    case ctx: DoBlockBlockContext    => astForDoBlock(ctx.doBlock(), blockMethodName)
    case ctx: BraceBlockBlockContext => astForBraceBlock(ctx.braceBlock(), blockMethodName)

  private def astForBlockHelper(
    ctx: ParserRuleContext,
    blockParamCtx: Option[BlockParameterContext],
    compoundStmtCtx: CompoundStatementContext,
    blockMethodName: Option[String] = None
  ) = {
    blockMethodName match {
      case Some(blockMethodName) =>
        astForBlockFunction(
          compoundStmtCtx.statements(),
          blockParamCtx,
          blockMethodName,
          line(compoundStmtCtx).head,
          lineEnd(compoundStmtCtx).head,
          column(compoundStmtCtx).head,
          columnEnd(compoundStmtCtx).head
        ).head
      case None =>
        val blockNode_    = blockNode(ctx, code(ctx), Defines.Any)
        val blockBodyAst  = astForCompoundStatement(compoundStmtCtx)
        val blockParamAst = blockParamCtx.flatMap(astForBlockParameterContext)
        blockAst(blockNode_, blockBodyAst.toList ++ blockParamAst)
    }
  }

  protected def astForDoBlock(ctx: DoBlockContext, blockMethodName: Option[String] = None): Ast = {
    astForBlockHelper(ctx, Option(ctx.blockParameter), ctx.bodyStatement().compoundStatement(), blockMethodName)
  }

  private def astForBraceBlock(ctx: BraceBlockContext, blockMethodName: Option[String] = None): Ast = {
    astForBlockHelper(ctx, Option(ctx.blockParameter), ctx.bodyStatement().compoundStatement(), blockMethodName)
  }

  // TODO: This class shouldn't be required and will eventually be phased out.
  protected implicit class BlockContextExt(val ctx: BlockContext) {
    def compoundStatement: CompoundStatementContext = {
      fold(_.bodyStatement.compoundStatement, _.bodyStatement.compoundStatement)
    }

    def blockParameter: Option[BlockParameterContext] = {
      fold(ctx => Option(ctx.blockParameter()), ctx => Option(ctx.blockParameter()))
    }

    private def fold[A](f: DoBlockContext => A, g: BraceBlockContext => A): A = ctx match {
      case ctx: DoBlockBlockContext    => f(ctx.doBlock())
      case ctx: BraceBlockBlockContext => g(ctx.braceBlock())
    }
  }

}
