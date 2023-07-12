package io.joern.rubysrc2cpg.astcreation

import better.files.File
import io.joern.rubysrc2cpg.parser.RubyParser.*
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.x2cpg.Ast
import io.joern.x2cpg.Defines.DynamicCallUnknownFullName
import io.joern.x2cpg.Imports.createImportNodeAndLink
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{NewBlock, NewCall, NewControlStructure, NewImport, NewLiteral}
import org.slf4j.LoggerFactory
import org.antlr.v4.runtime.ParserRuleContext

import scala.jdk.CollectionConverters.CollectionHasAsScala

trait AstForStatementsCreator {
  this: AstCreator =>

  private val logger = LoggerFactory.getLogger(this.getClass)
  protected def astForAliasStatement(ctx: AliasStatementContext): Ast = {
    val aliasName  = ctx.definedMethodNameOrSymbol(0).getText.substring(1)
    val methodName = ctx.definedMethodNameOrSymbol(1).getText.substring(1)
    methodAliases.addOne(aliasName, methodName)
    Ast()
  }

  protected def astForUndefStatement(ctx: UndefStatementContext): Ast = {
    val undefNames = ctx.definedMethodNameOrSymbol().asScala.flatMap(astForDefinedMethodNameOrSymbolContext).toSeq
    val call       = callNode(ctx, ctx.getText, RubyOperators.undef, RubyOperators.undef, DispatchTypes.STATIC_DISPATCH)
    callAst(call, undefNames)
  }

  protected def astForBeginStatement(ctx: BeginStatementContext): Ast = {
    val stmts     = astForStatements(ctx.statements())
    val blockNode = NewBlock().typeFullName(Defines.Any)
    blockAst(blockNode, stmts.toList)
  }

  protected def astForEndStatement(ctx: EndStatementContext): Ast = {
    val stmts     = astForStatements(ctx.statements())
    val blockNode = NewBlock().typeFullName(Defines.Any)
    blockAst(blockNode, stmts.toList)
  }

  protected def astForModifierStatement(ctx: ModifierStatementContext): Ast = ctx.mod.getType match {
    case IF     => astForIfModifierStatement(ctx)
    case UNLESS => astForUnlessModifierStatement(ctx)
    case WHILE  => astForWhileModifierStatement(ctx)
    case UNTIL  => astForUntilModifierStatement(ctx)
    case RESCUE => astForRescueModifierStatement(ctx)
  }

  protected def astForIfModifierStatement(ctx: ModifierStatementContext): Ast = {
    val lhs    = astForStatement(ctx.statement(0))
    val rhs    = astForStatement(ctx.statement(1)).headOption
    val ifNode = controlStructureNode(ctx, ControlStructureTypes.IF, ctx.getText)
    controlStructureAst(ifNode, rhs, lhs)
  }

  protected def astForUnlessModifierStatement(ctx: ModifierStatementContext): Ast = {
    val lhs    = astForStatement(ctx.statement(0))
    val rhs    = astForStatement(ctx.statement(1))
    val ifNode = controlStructureNode(ctx, ControlStructureTypes.IF, ctx.getText)
    controlStructureAst(ifNode, lhs.headOption, rhs)
  }

  protected def astForWhileModifierStatement(ctx: ModifierStatementContext): Ast = {
    val lhs = astForStatement(ctx.statement(0))
    val rhs = astForStatement(ctx.statement(1))
    whileAst(rhs.headOption, lhs, Some(ctx.getText))
  }

  protected def astForUntilModifierStatement(ctx: ModifierStatementContext): Ast = {
    val lhs = astForStatement(ctx.statement(0))
    val rhs = astForStatement(ctx.statement(1))
    whileAst(rhs.headOption, lhs, Some(ctx.getText))
  }

  protected def astForRescueModifierStatement(ctx: ModifierStatementContext): Ast = {
    val lhs       = astForStatement(ctx.statement(0))
    val rhs       = astForStatement(ctx.statement(1))
    val throwNode = controlStructureNode(ctx, ControlStructureTypes.THROW, ctx.getText)
    controlStructureAst(throwNode, rhs.headOption, lhs)
  }

  protected def astForCompoundStatement(ctx: CompoundStatementContext, packInBlock: Boolean = true): Seq[Ast] = {
    val stmtAsts = Option(ctx).map(_.statements()).map(astForStatements).getOrElse(Seq())
    if (packInBlock) {
      Seq(blockAst(blockNode(ctx), stmtAsts.toList))
    } else {
      stmtAsts
    }
  }

  protected def astForStatements(ctx: StatementsContext): Seq[Ast] = {
    Option(ctx) match {
      case Some(ctx) =>
        Option(ctx).map(_.statement()).map(_.asScala).getOrElse(Seq()).flatMap(astForStatement).toSeq
      case None =>
        Seq()
    }
  }

  // TODO: return Ast instead of Seq[Ast].
  protected def astForStatement(ctx: StatementContext): Seq[Ast] = ctx match {
    case ctx: AliasStatementContext               => Seq(astForAliasStatement(ctx))
    case ctx: UndefStatementContext               => Seq(astForUndefStatement(ctx))
    case ctx: BeginStatementContext               => Seq(astForBeginStatement(ctx))
    case ctx: EndStatementContext                 => Seq(astForEndStatement(ctx))
    case ctx: ModifierStatementContext            => Seq(astForModifierStatement(ctx))
    case ctx: ExpressionOrCommandStatementContext => astForExpressionOrCommand(ctx.expressionOrCommand())
  }

  // TODO: return Ast instead of Seq[Ast]
  protected def astForExpressionOrCommand(ctx: ExpressionOrCommandContext): Seq[Ast] = ctx match {
    case ctx: InvocationExpressionOrCommandContext => astForInvocationExpressionOrCommandContext(ctx)
    case ctx: NotExpressionOrCommandContext        => Seq(astForNotKeywordExpressionOrCommand(ctx))
    case ctx: OrAndExpressionOrCommandContext      => Seq(astForOrAndExpressionOrCommand(ctx))
    case ctx: ExpressionExpressionOrCommandContext => astForExpressionContext(ctx.expression())
    case _ =>
      logger.error(s"astForExpressionOrCommand() $filename, ${ctx.getText} All contexts mismatched.")
      Seq(Ast())
  }

  protected def astForNotKeywordExpressionOrCommand(ctx: NotExpressionOrCommandContext): Ast = {
    val exprOrCommandAst = astForExpressionOrCommand(ctx.expressionOrCommand())
    val call             = callNode(ctx, ctx.getText, Operators.not, Operators.not, DispatchTypes.STATIC_DISPATCH)
    callAst(call, exprOrCommandAst)
  }

  protected def astForOrAndExpressionOrCommand(ctx: OrAndExpressionOrCommandContext): Ast = ctx.op.getType match {
    case OR  => astForOrExpressionOrCommand(ctx)
    case AND => astForAndExpressionOrCommand(ctx)
  }

  protected def astForOrExpressionOrCommand(ctx: OrAndExpressionOrCommandContext): Ast = {
    val argsAst = ctx.expressionOrCommand().asScala.flatMap(astForExpressionOrCommand)
    val call    = callNode(ctx, ctx.getText, Operators.or, Operators.or, DispatchTypes.STATIC_DISPATCH)
    callAst(call, argsAst.toList)
  }

  protected def astForAndExpressionOrCommand(ctx: OrAndExpressionOrCommandContext): Ast = {
    val argsAst = ctx.expressionOrCommand().asScala.flatMap(astForExpressionOrCommand)
    val call    = callNode(ctx, ctx.getText, Operators.and, Operators.and, DispatchTypes.STATIC_DISPATCH)
    callAst(call, argsAst.toList)
  }

  protected def astForSuperCommand(ctx: SuperCommandContext): Ast =
    astForSuperCall(ctx, astForArguments(ctx.argumentsWithoutParentheses().arguments()))

  protected def astForYieldCommand(ctx: YieldCommandContext): Ast =
    astForYieldCall(ctx, Option(ctx.argumentsWithoutParentheses().arguments()))

  protected def astForSimpleMethodCommand(ctx: SimpleMethodCommandContext): Seq[Ast] = {
    val methodIdentifierAsts = astForMethodIdentifierContext(ctx.methodIdentifier(), ctx.getText)
    methodNameAsIdentifierStack.push(methodIdentifierAsts.head)
    val argsAsts = astForArguments(ctx.argumentsWithoutParentheses().arguments())

    val callNodes = methodIdentifierAsts.head.nodes.collect { case x: NewCall => x }
    if (callNodes.size == 1) {
      val callNode = callNodes.head
      if (callNode.name == "require" || callNode.name == "load") {
        resolveRequireOrLoadPath(argsAsts, callNode)
      } else if (callNode.name == "require_relative") {
        resolveRelativePath(filename, argsAsts, callNode)
      } else if (methodNames.contains(getActualMethodName(callNode.name))) {
        val thisNode = identifierNode(ctx, "this", "this", classStack.reverse.mkString(pathSep))
        Seq(callAst(callNode, argsAsts, Some(Ast(thisNode))))
      } else {
        Seq(callAst(callNode, argsAsts))
      }
    } else {
      argsAsts
    }
  }

  protected def astForMemberAccessCommand(ctx: MemberAccessCommandContext): Seq[Ast] = {
    val argsAst    = astForArguments(ctx.argumentsWithoutParentheses().arguments)
    val primaryAst = astForPrimaryContext(ctx.primary())
    val methodCallNode = astForMethodNameContext(ctx.methodName()).head.nodes.head
      .asInstanceOf[NewCall]
    val callNode = NewCall()
      .name(getActualMethodName(methodCallNode.name))
      .code(ctx.getText)
      .methodFullName(DynamicCallUnknownFullName)
      .signature("")
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .typeFullName(Defines.Any)
      .lineNumber(methodCallNode.lineNumber)
      .columnNumber(methodCallNode.columnNumber)
    Seq(callAst(callNode, primaryAst ++ argsAst))
  }

  protected def astForCommand(ctx: CommandContext): Seq[Ast] = ctx match {
    case ctx: YieldCommandContext        => Seq(astForYieldCommand(ctx))
    case ctx: SuperCommandContext        => Seq(astForSuperCommand(ctx))
    case ctx: SimpleMethodCommandContext => astForSimpleMethodCommand(ctx)
    case ctx: MemberAccessCommandContext => astForMemberAccessCommand(ctx)
  }

  protected def resolveRequireOrLoadPath(argsAst: Seq[Ast], callNode: NewCall): Seq[Ast] = {
    val importedNode = argsAst.head.nodes.collect { case x: NewLiteral => x }
    if (importedNode.size == 1) {
      val node      = importedNode.head
      val pathValue = node.code.replaceAll("'", "").replaceAll("\"", "")
      val result = pathValue match {
        case path if File(path).exists =>
          path
        case path if File(s"$path.rb").exists =>
          s"${path}.rb"
        case _ =>
          pathValue
      }
      packageStack.append(result)
      val importNode = createImportNodeAndLink(result, "", Some(callNode), diffGraph)
      Seq(callAst(callNode, argsAst), Ast(importNode))
    } else {
      Seq(callAst(callNode, argsAst))
    }
  }

  protected def resolveRelativePath(currentFile: String, argsAst: Seq[Ast], callNode: NewCall): Seq[Ast] = {
    val importedNode = argsAst.head.nodes.collect { case x: NewLiteral => x }
    if (importedNode.size == 1) {
      val node        = importedNode.head
      val pathValue   = node.code.replaceAll("'", "").replaceAll("\"", "")
      val updatedPath = if (pathValue.endsWith(".rb")) pathValue else s"$pathValue.rb"

      val currentDirectory = File(currentFile).parent
      val file             = File(currentDirectory, updatedPath)
      packageStack.append(file.pathAsString)
      val importNode = createImportNodeAndLink(updatedPath, "", Some(callNode), diffGraph)
      Seq(callAst(callNode, argsAst), Ast(importNode))
    } else {
      Seq(callAst(callNode, argsAst))
    }
  }

  protected def astForBlock(ctx: BlockContext): Ast = ctx match
    case ctx: DoBlockBlockContext    => astForDoBlock(ctx.doBlock())
    case ctx: BraceBlockBlockContext => astForBraceBlock(ctx.braceBlock())

  private def astForBlockHelper(
    ctx: ParserRuleContext,
    blockParamCtx: Option[BlockParameterContext],
    compoundStmtCtx: CompoundStatementContext
  ) = {
    val blockNode_    = blockNode(ctx, ctx.getText, Defines.Any)
    val blockBodyAst  = astForCompoundStatement(compoundStmtCtx)
    val blockParamAst = blockParamCtx.flatMap(astForBlockParameterContext)
    blockAst(blockNode_, blockBodyAst.toList ++ blockParamAst)
  }

  protected def astForDoBlock(ctx: DoBlockContext): Ast = {
    astForBlockHelper(ctx, Option(ctx.blockParameter), ctx.compoundStatement)
  }

  protected def astForBraceBlock(ctx: BraceBlockContext): Ast = {
    astForBlockHelper(ctx, Option(ctx.blockParameter), ctx.compoundStatement)
  }

  // TODO: This class shouldn't be required and will eventually be phased out.
  protected implicit class BlockContextExt(val ctx: BlockContext) {
    def compoundStatement: CompoundStatementContext = {
      fold(_.compoundStatement(), _.compoundStatement)
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
