package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.parser.RubyParser._
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.x2cpg.Ast
import io.joern.x2cpg.Defines.DynamicCallUnknownFullName
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{
  NewBlock,
  NewCall,
  NewControlStructure,
  NewIdentifier,
  NewLiteral
}

import scala.jdk.CollectionConverters.CollectionHasAsScala

trait AstForStatementsCreator { this: AstCreator =>

  protected def astForAliasStatement(ctx: AliasStatementContext): Ast = {
    val aliasName  = ctx.definedMethodNameOrSymbol(0).getText.substring(1)
    val methodName = ctx.definedMethodNameOrSymbol(1).getText.substring(1)
    methodAliases.addOne(aliasName, methodName)
    Ast()
  }

  protected def astForUndefStatement(ctx: UndefStatementContext): Ast = {
    val undefMethods =
      ctx
        .definedMethodNameOrSymbol()
        .asScala
        .flatMap(astForDefinedMethodNameOrSymbolContext)
        .toSeq

    val operatorName = RubyOperators.undef
    val callNode = NewCall()
      .name(operatorName)
      .code(ctx.getText)
      .methodFullName(operatorName)
      .signature("")
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .typeFullName(Defines.Any)
      .lineNumber(ctx.UNDEF().getSymbol().getLine())
      .columnNumber(ctx.UNDEF().getSymbol().getCharPositionInLine())
    callAst(callNode, undefMethods)
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
    val lhs = astForStatement(ctx.statement(0))
    val rhs = astForStatement(ctx.statement(1)).headOption
    val ifNode = NewControlStructure()
      .controlStructureType(ControlStructureTypes.IF)
      .code(ctx.getText)
    controlStructureAst(ifNode, rhs, lhs)
  }

  protected def astForUnlessModifierStatement(ctx: ModifierStatementContext): Ast = {
    val lhs = astForStatement(ctx.statement(0))
    val rhs = astForStatement(ctx.statement(1))
    val ifNode = NewControlStructure()
      .controlStructureType(ControlStructureTypes.IF)
      .code(ctx.getText)
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
    val lhs = astForStatement(ctx.statement(0))
    val rhs = astForStatement(ctx.statement(1))
    val throwNode = NewControlStructure()
      .controlStructureType(ControlStructureTypes.THROW)
      .code(ctx.getText)
    controlStructureAst(throwNode, rhs.headOption, lhs)
  }

  protected def astForCompoundStatement(ctx: CompoundStatementContext): Seq[Ast] =
    Option(ctx.statements()).map(astForStatements).getOrElse(Seq())

  protected def astForStatements(ctx: StatementsContext): Seq[Ast] =
    Option(ctx.statement()).map(_.asScala).getOrElse(Seq()).flatMap(astForStatement).toSeq

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

  protected def astForSuperCommand(ctx: SuperCommandContext): Seq[Ast] = {
    astForArgumentsWithoutParenthesesContext(ctx.argumentsWithoutParentheses())
  }

  protected def astForYieldCommand(ctx: YieldCommandContext): Seq[Ast] = {
    val argsAst = astForArgumentsWithoutParenthesesContext(ctx.argumentsWithoutParentheses())

    val callNode = NewCall()
      .name(UNRESOLVED_YIELD)
      .code(ctx.getText)
      .methodFullName(UNRESOLVED_YIELD)
      .signature("")
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .typeFullName(Defines.Any)
      .lineNumber(ctx.YIELD().getSymbol().getLine())
      .columnNumber(ctx.YIELD().getSymbol().getCharPositionInLine())
    Seq(callAst(callNode, argsAst))
  }

  protected def astForSimpleMethodCommand(ctx: SimpleMethodCommandContext): Seq[Ast] = {
    val methodIdentifierAsts = astForMethodIdentifierContext(ctx.methodIdentifier(), ctx.getText)
    methodNameAsIdentifierStack.push(methodIdentifierAsts.head)
    val argsAsts = astForArgumentsWithoutParenthesesContext(ctx.argumentsWithoutParentheses())

    val callNodes = methodIdentifierAsts.head.nodes.filter(node => node.isInstanceOf[NewCall])
    if (callNodes.size == 1) {
      val callNode = callNodes.head.asInstanceOf[NewCall]
      if (
        callNode.name == "require" ||
        callNode.name == "require_once" ||
        callNode.name == "load"
      ) {
        val literalImports = argsAsts.head.nodes
          .filter(node => node.isInstanceOf[NewLiteral])

        if (literalImports.size == 1) {
          val importedFile =
            literalImports.head
              .asInstanceOf[NewLiteral]
              .code
          println(s"AST to be created for imported file ${importedFile}")
        } else {
          println(
            s"Cannot process import since it is determined on the fly. Just creating a call node for later processing"
          )
          Seq(callAst(callNode, argsAsts))
        }
      }
      Seq(callAst(callNode, argsAsts))
    } else {
      argsAsts
    }
  }

  protected def astForMemberAccessCommand(ctx: MemberAccessCommandContext): Seq[Ast] = {
    val argsAst    = astForArgumentsWithoutParenthesesContext(ctx.argumentsWithoutParentheses())
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
    case ctx: YieldCommandContext        => astForYieldCommand(ctx)
    case ctx: SuperCommandContext        => astForSuperCommand(ctx)
    case ctx: SimpleMethodCommandContext => astForSimpleMethodCommand(ctx)
    case ctx: MemberAccessCommandContext => astForMemberAccessCommand(ctx)
  }
}
