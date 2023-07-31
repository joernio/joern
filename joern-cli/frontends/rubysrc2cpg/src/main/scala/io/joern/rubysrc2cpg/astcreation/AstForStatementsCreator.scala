package io.joern.rubysrc2cpg.astcreation

import better.files.File
import io.joern.rubysrc2cpg.parser.RubyParser.*
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.x2cpg.Ast
import io.joern.x2cpg.Defines.DynamicCallUnknownFullName
import io.joern.x2cpg.Imports.createImportNodeAndLink
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{
  NewBlock,
  NewCall,
  NewControlStructure,
  NewIdentifier,
  NewImport,
  NewLiteral,
  NewMethod,
  NewReturn
}
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

  protected def lastStmtAsReturn(code: String, lastStmtAst: Ast): Ast = {
    val lastStmtIsAlreadyReturn = lastStmtAst.root match {
      case Some(value) => value.isInstanceOf[NewReturn]
      case None        => false
    }

    if (lastStmtIsAlreadyReturn) {
      lastStmtAst
    } else {
      val retNode = NewReturn()
        .code(code)
      returnAst(retNode, Seq[Ast](lastStmtAst))
    }
  }

  protected def astForCompoundStatement(
    ctx: CompoundStatementContext,
    isMethodBody: Boolean = false,
    canConsiderAsLeaf: Boolean = true
  ): Seq[Ast] = {
    val stmtAsts = Option(ctx)
      .map(_.statements())
      .map(st => { astForStatements(st, isMethodBody, canConsiderAsLeaf) })
      .getOrElse(Seq())
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
    Option(ctx) match {
      case Some(ctx) =>
        val stmtCount   = ctx.statement().size()
        var stmtCounter = 0
        val myBlockId   = blockIdCounter
        blockIdCounter += 1
        val parentBlockId = currentBlockId
        if (canConsiderAsLeaf) {
          blockChildHash.update(parentBlockId, myBlockId)
        }
        currentBlockId = myBlockId

        val stmtAsts = Option(ctx)
          .map(_.statement())
          .map(_.asScala)
          .getOrElse(Seq())
          .flatMap(stCtx => {
            stmtCounter += 1
            if (isMethodBody) {
              if (stmtCounter == stmtCount) {
                processingLastMethodStatement = true
              } else {
                processingLastMethodStatement = false
              }
            }
            val stAsts = astForStatement(stCtx)
            if (stAsts.size > 0 && canConsiderAsLeaf && processingLastMethodStatement) {
              blockChildHash.get(myBlockId) match {
                case Some(value) =>
                  // this is a non-leaf block
                  stAsts
                case None =>
                  // this is a leaf block
                  if (isMethodBody && stmtCounter == stmtCount) {
                    processingLastMethodStatement = false
                  }
                  Seq(lastStmtAsReturn(stCtx.getText, stAsts.head))
              }
            } else {
              stAsts
            }
          })
          .toSeq
        currentBlockId = parentBlockId
        stmtAsts
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

    /* get args without the method def in it */
    val argAstsWithoutMethods = argsAsts.filterNot(_.nodes.head.isInstanceOf[NewMethod])

    /* isolate methods from the original args and create identifier ASTs from it */
    val methodDefAsts = argsAsts.filter(_.nodes.head.isInstanceOf[NewMethod])
    val methodToIdentifierAsts = methodDefAsts.map { ast =>
      val id = NewIdentifier()
        .name(ast.nodes.head.asInstanceOf[NewMethod].name)
        .code(ast.nodes.head.asInstanceOf[NewMethod].name)
        .typeFullName(Defines.Any)
        .lineNumber(ast.nodes.head.asInstanceOf[NewMethod].lineNumber)
      Ast(id)
    }

    /* TODO: we add the isolated method defs later on to the parent instead */
    methodDefAsts.foreach { ast =>
      methodDefInArgument.add(ast)
    }

    val callNodes = methodIdentifierAsts.head.nodes.collect { case x: NewCall => x }
    if (callNodes.size == 1) {
      val callNode = callNodes.head
      if (callNode.name == "require" || callNode.name == "load") {
        resolveRequireOrLoadPath(argsAsts, callNode)
      } else if (callNode.name == "require_relative") {
        resolveRelativePath(filename, argsAsts, callNode)
      } else if (callNode.name == "attr_accessor") {
        /* we remove the method definition AST from argument and add its corresponding identifier form */
        Seq(callAst(callNode, argAstsWithoutMethods ++ methodToIdentifierAsts))
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
    astForBlockHelper(ctx, Option(ctx.blockParameter), ctx.bodyStatement().compoundStatement())
  }

  protected def astForBraceBlock(ctx: BraceBlockContext): Ast = {
    astForBlockHelper(ctx, Option(ctx.blockParameter), ctx.bodyStatement().compoundStatement())
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
