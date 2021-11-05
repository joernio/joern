package io.shiftleft.fuzzyc2cpg.parser.functions.builder;

import io.shiftleft.fuzzyc2cpg.FunctionParser;
import io.shiftleft.fuzzyc2cpg.FunctionParser.Additive_expressionContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.And_expressionContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.ArrayIndexingContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.Assign_exprContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.Bit_and_expressionContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.Block_starterContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.BreakStatementContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.Cast_expressionContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.Cast_targetContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.Catch_statementContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.Closing_curlyContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.ConditionContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.Conditional_expressionContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.ContinueStatementContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.DeclByClassContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.Do_statementContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.Else_statementContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.Equality_expressionContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.Exclusive_or_expressionContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.ExprContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.Expr_statementContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.For_init_statementContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.For_statementContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.FuncCallContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.Function_argumentContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.Function_argument_listContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.GotoStatementContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.ConstantContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.IdentifierContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.If_statementContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.IncDecOpContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.Inc_decContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.Inclusive_or_expressionContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.InitDeclSimpleContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.InitDeclWithAssignContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.InitDeclWithCallContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.Initializer_listContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.LabelContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.MemberAccessContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.Multiplicative_expressionContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.Opening_curlyContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.Or_expressionContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.Primary_expressionContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.PtrMemberAccessContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.Relational_expressionContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.ReturnStatementContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.Shift_expressionContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.SizeofContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.Sizeof_expressionContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.Sizeof_operand2Context;
import io.shiftleft.fuzzyc2cpg.FunctionParser.Sizeof_operandContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.StatementContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.StatementsContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.Switch_statementContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.ThrowStatementContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.Try_statementContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.Type_nameContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.Unary_expressionContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.Unary_op_and_cast_exprContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.Unary_operatorContext;
import io.shiftleft.fuzzyc2cpg.FunctionParser.While_statementContext;
import io.shiftleft.fuzzyc2cpg.ast.AstNode;
import io.shiftleft.fuzzyc2cpg.ast.AstNodeBuilder;
import io.shiftleft.fuzzyc2cpg.ast.expressions.*;
import io.shiftleft.fuzzyc2cpg.ast.langc.expressions.CallExpression;
import io.shiftleft.fuzzyc2cpg.ast.langc.expressions.SizeofExpression;
import io.shiftleft.fuzzyc2cpg.ast.langc.statements.blockstarters.ElseStatement;
import io.shiftleft.fuzzyc2cpg.ast.langc.statements.blockstarters.IfStatement;
import io.shiftleft.fuzzyc2cpg.ast.declarations.ClassDefStatement;
import io.shiftleft.fuzzyc2cpg.ast.declarations.IdentifierDecl;
import io.shiftleft.fuzzyc2cpg.ast.declarations.IdentifierDeclType;
import io.shiftleft.fuzzyc2cpg.ast.logical.statements.BlockCloser;
import io.shiftleft.fuzzyc2cpg.ast.logical.statements.BlockStarter;
import io.shiftleft.fuzzyc2cpg.ast.logical.statements.CompoundStatement;
import io.shiftleft.fuzzyc2cpg.ast.logical.statements.Label;
import io.shiftleft.fuzzyc2cpg.ast.logical.statements.Statement;
import io.shiftleft.fuzzyc2cpg.ast.statements.ExpressionStatement;
import io.shiftleft.fuzzyc2cpg.ast.statements.IdentifierDeclStatement;
import io.shiftleft.fuzzyc2cpg.ast.statements.blockstarters.CatchStatement;
import io.shiftleft.fuzzyc2cpg.ast.statements.blockstarters.DoStatement;
import io.shiftleft.fuzzyc2cpg.ast.statements.blockstarters.ForStatement;
import io.shiftleft.fuzzyc2cpg.ast.statements.blockstarters.SwitchStatement;
import io.shiftleft.fuzzyc2cpg.ast.statements.blockstarters.TryStatement;
import io.shiftleft.fuzzyc2cpg.ast.statements.blockstarters.WhileStatement;
import io.shiftleft.fuzzyc2cpg.ast.statements.jump.BreakStatement;
import io.shiftleft.fuzzyc2cpg.ast.statements.jump.ContinueStatement;
import io.shiftleft.fuzzyc2cpg.ast.statements.jump.GotoStatement;
import io.shiftleft.fuzzyc2cpg.ast.statements.jump.ReturnStatement;
import io.shiftleft.fuzzyc2cpg.ast.statements.jump.ThrowStatement;
import io.shiftleft.fuzzyc2cpg.parser.AstNodeFactory;
import io.shiftleft.fuzzyc2cpg.parser.shared.InitDeclContextWrapper;
import io.shiftleft.fuzzyc2cpg.parser.shared.builders.ClassDefBuilder;
import io.shiftleft.fuzzyc2cpg.parser.shared.builders.IdentifierDeclBuilder;
import java.util.EmptyStackException;
import java.util.HashMap;
import org.antlr.v4.runtime.ParserRuleContext;

/**
 * The FunctionContentBuilder is invoked while walking the parse tree to create
 * ASTs for the contents of functions, i.e., the first-level compound statements
 * of functions.
 *
 * Since the fuzzy parser avoids using nested grammar rules as these rules often
 * require reading all tokens of a file only to realize that the default rule
 * must be taken, the most difficult task this code fulfills is to produce a
 * correctly nested AST.
 */

public class FunctionContentBuilder extends AstNodeBuilder<AstNode> {

  ContentBuilderStack stack = new ContentBuilderStack();
  NestingReconstructor nesting = new NestingReconstructor(stack);
  HashMap<AstNode, ParserRuleContext> nodeToRuleContext = new HashMap<AstNode, ParserRuleContext>();

  // exitStatements is called when the entire
  // function-content has been walked

  public void exitStatements(StatementsContext ctx) {
    if (stack.size() != 1) {
      throw new RuntimeException("Broken stack while parsing");
    }

  }

  // For all statements, begin by pushing a Statement Object
  // onto the stack.

  public void enterStatement(StatementContext ctx) {
    AstNode statementItem = AstNodeFactory.create(ctx);
    nodeToRuleContext.put(statementItem, ctx);
    stack.push(statementItem);
  }

  // Mapping of grammar-rules to CodeItems.

  public void enterOpeningCurly(Opening_curlyContext ctx) {
    replaceTopOfStack(new CompoundStatement(), ctx);
  }

  public void enterClosingCurly(Closing_curlyContext ctx) {
    replaceTopOfStack(new BlockCloser(), ctx);
  }

  public void enterBlockStarter(Block_starterContext ctx) {
    replaceTopOfStack(new BlockStarter(), ctx);
  }

  public void enterExprStatement(Expr_statementContext ctx) {
    replaceTopOfStack(new ExpressionStatement(), ctx);
  }

  public void enterIf(If_statementContext ctx) {
    replaceTopOfStack(new IfStatement(), ctx);
  }

  public void enterFor(For_statementContext ctx) {
    replaceTopOfStack(new ForStatement(), ctx);
  }

  public void enterWhile(While_statementContext ctx) {
    replaceTopOfStack(new WhileStatement(), ctx);
  }

  public void enterDo(Do_statementContext ctx) {
    replaceTopOfStack(new DoStatement(), ctx);
  }

  public void enterElse(Else_statementContext ctx) {
    replaceTopOfStack(new ElseStatement(), ctx);
  }

  public void exitStatement(StatementContext ctx) {
    if (stack.size() == 0) {
      throw new RuntimeException();
    }

    AstNode itemToRemove = stack.peek();
    AstNodeFactory.initializeFromContext(itemToRemove, ctx);

    if (itemToRemove instanceof BlockCloser) {
      closeCompoundStatement();
      return;
    }

    // We keep Block-starters and compound items
    // on the stack. They are removed by following
    // statements.
    if (itemToRemove instanceof BlockStarter
        || itemToRemove instanceof CompoundStatement) {
      return;
    }

    nesting.consolidate();
  }

  private void closeCompoundStatement() {
    stack.pop(); // remove 'CloseBlock'
    CompoundStatement compoundItem = (CompoundStatement) stack.pop();
    nesting.consolidateBlockStarters(compoundItem);
  }

  // Expression handling

  public void enterExpression(ExprContext ctx) {
    Expression expression = new Expression();
    nodeToRuleContext.put(expression, ctx);
    stack.push(expression);
  }

  public void exitExpression(ExprContext ctx) {
    nesting.consolidateSubExpression(ctx);
  }

  public void enterAssignment(Assign_exprContext ctx) {
    AssignmentExpression expr = new AssignmentExpression();
    nodeToRuleContext.put(expr, ctx);
    stack.push(expr);
  }

  public void exitAssignment(Assign_exprContext ctx) {
    nesting.consolidateSubExpression(ctx);
  }

  public void enterConditionalExpr(Conditional_expressionContext ctx) {
    ConditionalExpression expr = new ConditionalExpression();
    nodeToRuleContext.put(expr, ctx);
    stack.push(expr);
  }

  public void exitConditionalExpr(Conditional_expressionContext ctx) {
    introduceCndNodeForCndExpr();
    nesting.consolidateSubExpression(ctx);
  }

  public void enterOrExpression(Or_expressionContext ctx) {
    OrExpression expr = new OrExpression();
    nodeToRuleContext.put(expr, ctx);
    stack.push(expr);
  }

  public void exitrOrExpression(Or_expressionContext ctx) {
    nesting.consolidateSubExpression(ctx);
  }

  public void enterAndExpression(And_expressionContext ctx) {
    AndExpression expr = new AndExpression();
    nodeToRuleContext.put(expr, ctx);
    stack.push(expr);
  }

  public void exitAndExpression(And_expressionContext ctx) {
    nesting.consolidateSubExpression(ctx);
  }

  public void enterInclusiveOrExpression(Inclusive_or_expressionContext ctx) {
    InclusiveOrExpression expr = new InclusiveOrExpression();
    nodeToRuleContext.put(expr, ctx);
    stack.push(expr);
  }

  public void exitInclusiveOrExpression(Inclusive_or_expressionContext ctx) {
    nesting.consolidateSubExpression(ctx);
  }

  public void enterExclusiveOrExpression(Exclusive_or_expressionContext ctx) {
    ExclusiveOrExpression expr = new ExclusiveOrExpression();
    nodeToRuleContext.put(expr, ctx);
    stack.push(expr);
  }

  public void exitExclusiveOrExpression(Exclusive_or_expressionContext ctx) {
    nesting.consolidateSubExpression(ctx);
  }

  public void enterBitAndExpression(Bit_and_expressionContext ctx) {
    BitAndExpression expr = new BitAndExpression();
    nodeToRuleContext.put(expr, ctx);
    stack.push(expr);
  }

  public void enterEqualityExpression(Equality_expressionContext ctx) {
    EqualityExpression expr = new EqualityExpression();
    nodeToRuleContext.put(expr, ctx);
    stack.push(expr);
  }

  public void exitEqualityExpression(Equality_expressionContext ctx) {
    nesting.consolidateSubExpression(ctx);
  }

  public void exitBitAndExpression(Bit_and_expressionContext ctx) {
    nesting.consolidateSubExpression(ctx);
  }

  public void enterRelationalExpression(Relational_expressionContext ctx) {
    RelationalExpression expr = new RelationalExpression();
    nodeToRuleContext.put(expr, ctx);
    stack.push(expr);
  }

  public void exitRelationalExpression(Relational_expressionContext ctx) {
    nesting.consolidateSubExpression(ctx);
  }

  public void enterShiftExpression(Shift_expressionContext ctx) {
    ShiftExpression expr = new ShiftExpression();
    nodeToRuleContext.put(expr, ctx);
    stack.push(expr);
  }

  public void exitShiftExpression(Shift_expressionContext ctx) {
    nesting.consolidateSubExpression(ctx);
  }

  public void enterAdditiveExpression(Additive_expressionContext ctx) {
    AdditiveExpression expr = new AdditiveExpression();
    nodeToRuleContext.put(expr, ctx);
    stack.push(expr);
  }

  public void exitAdditiveExpression(Additive_expressionContext ctx) {
    nesting.consolidateSubExpression(ctx);
  }

  public void enterMultiplicativeExpression(
      Multiplicative_expressionContext ctx) {
    MultiplicativeExpression expr = new MultiplicativeExpression();
    nodeToRuleContext.put(expr, ctx);
    stack.push(expr);
  }

  public void exitMultiplicativeExpression(
      Multiplicative_expressionContext ctx) {
    nesting.consolidateSubExpression(ctx);
  }

  public void enterCastExpression(Cast_expressionContext ctx) {
    CastExpression expr = new CastExpression();
    nodeToRuleContext.put(expr, ctx);
    stack.push(expr);
  }

  public void exitCastExpression(Cast_expressionContext ctx) {
    nesting.consolidateSubExpression(ctx);
  }

  public void enterCast_target(Cast_targetContext ctx) {
    CastTarget expr = new CastTarget();
    nodeToRuleContext.put(expr, ctx);
    stack.push(expr);
  }

  public void exitCast_target(Cast_targetContext ctx) {
    nesting.consolidateSubExpression(ctx);
  }

  public void enterFuncCall(FuncCallContext ctx) {
    CallExpression expr = new CallExpression();
    nodeToRuleContext.put(expr, ctx);
    stack.push(expr);
  }

  public void exitFuncCall(FuncCallContext ctx) {
    introduceCalleeNode();
    nesting.consolidateSubExpression(ctx);
  }

  public void enterSizeof(SizeofContext ctx) {
    Sizeof expr = new Sizeof();
    nodeToRuleContext.put(expr, ctx);
    stack.push(expr);
  }

  public void exitSizeof(SizeofContext ctx) {
    nesting.consolidateSubExpression(ctx);
  }

  private void introduceCalleeNode() {
    CallExpression expr;
    try {
      expr = (CallExpression) stack.peek();
    } catch (EmptyStackException ex) {
      return;
    }

    AstNode child = expr.getChild(0);
    if (child == null) {
      return;
    }

    Callee callee = new Callee();
    callee.addChild(child);
    expr.replaceFirstChild(callee);
  }

  private void introduceCndNodeForCndExpr() {
    ConditionalExpression expr;
    try {
      expr = (ConditionalExpression) stack.peek();
    } catch (EmptyStackException ex) {
      return;
    }

    AstNode child = expr.getChild(0);
    if (child == null) {
      return;
    }
    Condition cnd = new Condition();
    cnd.addChild(child);
    expr.replaceFirstChild(cnd);

  }

  public void enterArgumentList(Function_argument_listContext ctx) {
    ArgumentList expr = new ArgumentList();
    nodeToRuleContext.put(expr, ctx);
    stack.push(expr);
  }

  public void exitArgumentList(Function_argument_listContext ctx) {
    nesting.consolidateSubExpression(ctx);
  }

  public void enterCondition(ConditionContext ctx) {
    Condition expr = new Condition();
    nodeToRuleContext.put(expr, ctx);
    stack.push(expr);
  }

  public void exitCondition(ConditionContext ctx) {
    Condition cond = (Condition) stack.pop();
    AstNodeFactory.initializeFromContext(cond, ctx);
    nesting.addItemToParent(cond);
  }

  public void enterDeclByClass(DeclByClassContext ctx) {
    ClassDefBuilder classDefBuilder = new ClassDefBuilder();
    classDefBuilder.createNew(ctx);
    classDefBuilder.setName(ctx.class_def().class_name());
    replaceTopOfStack(classDefBuilder.getItem(), ctx);
  }

  public void exitDeclByClass() {
    nesting.consolidate();
  }

  public void enterInitDeclSimple(InitDeclSimpleContext ctx) {
    AstNode identifierDecl = buildDeclarator(ctx);
    nodeToRuleContext.put(identifierDecl, ctx);
    stack.push(identifierDecl);
  }

  public void exitInitDeclSimple() {
    IdentifierDecl identifierDecl = (IdentifierDecl) stack.pop();
    AstNode stmt = stack.peek();
    stmt.addChild(identifierDecl);
  }

  public void enterInitDeclWithAssign(InitDeclWithAssignContext ctx) {
    IdentifierDecl identifierDecl = buildDeclarator(ctx);
    nodeToRuleContext.put(identifierDecl, ctx);
    stack.push(identifierDecl);
  }

  public void exitInitDeclWithAssign(InitDeclWithAssignContext ctx) {
    IdentifierDecl identifierDecl = (IdentifierDecl) stack.pop();

    Expression lastChild = (Expression) identifierDecl.popLastChild();
    AssignmentExpression assign = AstNodeFactory.create(ctx);

    // This is a bit of a hack. As we go up,
    // we introduce an artificial assignment-node.

    assign.addChild(new Identifier(identifierDecl.getName()));
    assign.addChild(lastChild);

    identifierDecl.addChild(assign);

    AstNode stmt = stack.peek();
    stmt.addChild(identifierDecl);
  }

  public void enterInitDeclWithCall(InitDeclWithCallContext ctx) {
    AstNode identifierDecl = buildDeclarator(ctx);
    nodeToRuleContext.put(identifierDecl, ctx);
    stack.push(identifierDecl);
  }

  public void exitInitDeclWithCall() {
    IdentifierDecl identifierDecl = (IdentifierDecl) stack.pop();
    AstNode stmt = stack.peek();
    stmt.addChild(identifierDecl);
  }

  private IdentifierDecl buildDeclarator(ParserRuleContext ctx) {
    InitDeclContextWrapper wrappedContext = new InitDeclContextWrapper(ctx);
    ParserRuleContext typeName = getTypeFromParent();
    IdentifierDeclBuilder declBuilder = new IdentifierDeclBuilder();
    declBuilder.createNew(ctx);
    declBuilder.setType(wrappedContext, typeName);
    return declBuilder.getItem();
  }

  private ParserRuleContext getTypeFromParent() {
    AstNode parentItem = stack.peek();
    ParserRuleContext typeName;
    if (parentItem instanceof IdentifierDeclStatement) {
      IdentifierDeclStatement stmt = ((IdentifierDeclStatement) parentItem);
      IdentifierDeclType type = stmt.getType();
      typeName = nodeToRuleContext.get(type);
    } else if (parentItem instanceof ClassDefStatement) {
      Identifier name = ((ClassDefStatement) parentItem).getIdentifier();
      typeName = nodeToRuleContext.get(name);
    } else {
      throw new RuntimeException(
          "No matching declaration statement/class definition for init declarator");
    }
    return typeName;
  }

  public void enterIncDec(Inc_decContext ctx) {
    IncDec expr = new IncDec();
    nodeToRuleContext.put(expr, ctx);
    stack.push(expr);
  }

  public void exitIncDec(Inc_decContext ctx) {
    nesting.consolidateSubExpression(ctx);
  }

  public void enterArrayIndexing(ArrayIndexingContext ctx) {
    ArrayIndexing expr = new ArrayIndexing();
    nodeToRuleContext.put(expr, ctx);
    stack.push(expr);
  }

  public void exitArrayIndexing(ArrayIndexingContext ctx) {
    nesting.consolidateSubExpression(ctx);
  }

  public void enterMemberAccess(MemberAccessContext ctx) {
    MemberAccess expr = new MemberAccess();
    nodeToRuleContext.put(expr, ctx);
    stack.push(expr);
  }

  public void exitMemberAccess(MemberAccessContext ctx) {
    nesting.consolidateSubExpression(ctx);
  }

  public void enterIncDecOp(IncDecOpContext ctx) {
    PostIncDecOperationExpression expr = new PostIncDecOperationExpression();
    nodeToRuleContext.put(expr, ctx);
    stack.push(expr);
  }

  public void exitIncDecOp(IncDecOpContext ctx) {
    nesting.consolidateSubExpression(ctx);
  }

  public void enterPrimary(Primary_expressionContext ctx) {
    PrimaryExpression expr = new PrimaryExpression();
    nodeToRuleContext.put(expr, ctx);
    stack.push(expr);
  }

  public void exitPrimary(Primary_expressionContext ctx) {
    nesting.consolidateSubExpression(ctx);
  }

  public void enterUnaryExpression(Unary_expressionContext ctx) {
    UnaryExpression expr = new UnaryExpression();
    nodeToRuleContext.put(expr, ctx);
    stack.push(expr);
  }

  public void exitUnaryExpression(Unary_expressionContext ctx) {
    nesting.consolidateSubExpression(ctx);
  }

  public void enterConstant(ConstantContext ctx) {
    Constant expr = new Constant();
    nodeToRuleContext.put(expr, ctx);
    stack.push(expr);
  }

  public void exitConstant(ConstantContext ctx) {
    nesting.consolidateSubExpression(ctx);
  }

  public void enterIdentifier(IdentifierContext ctx) {
    Identifier expr = new Identifier();
    nodeToRuleContext.put(expr, ctx);
    stack.push(expr);
  }

  public void exitIdentifier(IdentifierContext ctx) {
    nesting.consolidateSubExpression(ctx);
  }

  public void enterArgument(Function_argumentContext ctx) {
    Argument expr = new Argument();
    nodeToRuleContext.put(expr, ctx);
    stack.push(expr);
  }

  public void exitArgument(Function_argumentContext ctx) {
    nesting.consolidateSubExpression(ctx);
  }

  public void enterInitializerList(Initializer_listContext ctx) {
    InitializerList expr = new InitializerList();
    nodeToRuleContext.put(expr, ctx);
    stack.push(expr);
  }

  public void exitInitializerList(Initializer_listContext ctx) {
    nesting.consolidateSubExpression(ctx);
  }

  public void enterPtrMemberAccess(PtrMemberAccessContext ctx) {
    PtrMemberAccess expr = new PtrMemberAccess();
    nodeToRuleContext.put(expr, ctx);
    stack.push(expr);
  }

  public void exitPtrMemberAccess(PtrMemberAccessContext ctx) {
    nesting.consolidateSubExpression(ctx);
  }

  public void enterInitFor(For_init_statementContext ctx) {
    ForInit expr = new ForInit();
    nodeToRuleContext.put(expr, ctx);
    stack.push(expr);
  }

  public void exitInitFor(For_init_statementContext ctx) {
    AstNode node = stack.pop();
    AstNodeFactory.initializeFromContext(node, ctx);
    ForStatement forStatement = (ForStatement) stack.peek();
    forStatement.addChild(node);
  }

  public void enterSwitchStatement(Switch_statementContext ctx) {
    replaceTopOfStack(new SwitchStatement(), ctx);
  }

  public void enterLabel(LabelContext ctx) {
    replaceTopOfStack(new Label(), ctx);
  }

  public void enterReturnStatement(ReturnStatementContext ctx) {
    replaceTopOfStack(new ReturnStatement(), ctx);
  }

  public void enterBreakStatement(BreakStatementContext ctx) {
    replaceTopOfStack(new BreakStatement(), ctx);
  }

  public void enterContinueStatement(ContinueStatementContext ctx) {
    replaceTopOfStack(new ContinueStatement(), ctx);
  }

  public void enterGotoStatement(GotoStatementContext ctx) {
    replaceTopOfStack(new GotoStatement(), ctx);
  }

  @Override
  public void createNew(ParserRuleContext ctx) {
    item = new CompoundStatement();
    CompoundStatement rootItem = (CompoundStatement) item;
    AstNodeFactory.initializeFromContext(item, ctx);
    nodeToRuleContext.put(rootItem, ctx);
    stack.push(rootItem);
  }

  public void addLocalDecl(IdentifierDecl decl) {
    IdentifierDeclStatement declStmt = (IdentifierDeclStatement) stack
        .peek();
    declStmt.addChild(decl);
  }

  public void enterDeclByType(ParserRuleContext ctx,
      Type_nameContext type_nameContext) {
    IdentifierDeclStatement declStmt = new IdentifierDeclStatement();
    AstNodeFactory.initializeFromContext(declStmt, ctx);

    IdentifierDeclType type = new IdentifierDeclType();
    AstNodeFactory.initializeFromContext(type, type_nameContext);
    nodeToRuleContext.put(type, type_nameContext);
    declStmt.addChild(type);

    if (stack.peek() instanceof Statement) {
      replaceTopOfStack(declStmt, ctx);
    } else {
      nodeToRuleContext.put(declStmt, ctx);
      stack.push(declStmt);
    }
  }

  public void exitDeclByType() {
    nesting.consolidate();
  }

  protected void replaceTopOfStack(AstNode item, ParserRuleContext ctx) {
    AstNode oldNode = stack.pop();
    nodeToRuleContext.put(item, ctx);
    stack.push(item);
  }

  public void enterSizeofExpr(Sizeof_expressionContext ctx) {
    SizeofExpression expr = new SizeofExpression();
    nodeToRuleContext.put(expr, ctx);
    stack.push(expr);
  }

  public void exitSizeofExpr(Sizeof_expressionContext ctx) {
    nesting.consolidateSubExpression(ctx);
  }

  public void enterSizeofOperand2(Sizeof_operand2Context ctx) {
    SizeofOperand expr = new SizeofOperand();
    nodeToRuleContext.put(expr, ctx);
    stack.push(expr);
  }

  public void enterSizeofOperand(Sizeof_operandContext ctx) {
    SizeofOperand expr = new SizeofOperand();
    stack.push(expr);
  }

  public void exitSizeofOperand2(Sizeof_operand2Context ctx) {
    nesting.consolidateSubExpression(ctx);
  }

  public void exitSizeofOperand(Sizeof_operandContext ctx) {
    nesting.consolidateSubExpression(ctx);
  }

  public void enterUnaryOpAndCastExpr(Unary_op_and_cast_exprContext ctx) {
    UnaryOperationExpression expr = new UnaryOperationExpression();
    nodeToRuleContext.put(expr, ctx);
    stack.push(expr);
  }

  public void exitUnaryOpAndCastExpr(Unary_op_and_cast_exprContext ctx) {
    nesting.consolidateSubExpression(ctx);
  }

  public void enterUnaryOperator(Unary_operatorContext ctx) {
    UnaryOperator expr = new UnaryOperator();
    nodeToRuleContext.put(expr, ctx);
    stack.push(expr);
  }

  public void exitUnaryOperator(Unary_operatorContext ctx) {
    nesting.consolidateSubExpression(ctx);
  }

  public void enterTryStatement(Try_statementContext ctx) {
    replaceTopOfStack(new TryStatement(), ctx);
  }

  public void enterCatchStatement(Catch_statementContext ctx) {
    replaceTopOfStack(new CatchStatement(), ctx);
  }

  public void enterThrowStatement(ThrowStatementContext ctx) {
    replaceTopOfStack(new ThrowStatement(), ctx);
  }

    public void enterNewExpr(FunctionParser.New_expressionContext ctx) {
      NewExpression expr = new NewExpression();
      expr.setTargetClass(ctx.type_name());
      // TODO: Set the arg list (e.g. class ctor params & array size)
      expr.setArgumentList(new ArgumentList());
      nodeToRuleContext.put(expr, ctx);
      stack.push(expr);
  }

  public void exitNewExpr(FunctionParser.New_expressionContext ctx) {
    nesting.consolidateSubExpression(ctx);
  }

  public void enterDeleteExpr(FunctionParser.Delete_expressionContext ctx) {
    DeleteExpression expr = new DeleteExpression();
    expr.setTarget(ctx.identifier());
    expr.setArgumentList(new ArgumentList());
    nodeToRuleContext.put(expr, ctx);
    stack.push(expr);
  }

  public void exitDeleteExpr(FunctionParser.Delete_expressionContext ctx) {
    nesting.consolidateSubExpression(ctx);
  }
}
