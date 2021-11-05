package io.shiftleft.fuzzyc2cpg.parser.functions.builder;

import io.shiftleft.fuzzyc2cpg.ast.AstNode;
import io.shiftleft.fuzzyc2cpg.ast.langc.statements.blockstarters.IfStatement;
import io.shiftleft.fuzzyc2cpg.ast.statements.blockstarters.DoStatement;
import io.shiftleft.fuzzyc2cpg.ast.statements.blockstarters.TryStatement;
import java.util.Stack;

public class ContentBuilderStack {

  private Stack<AstNode> itemStack = new Stack<AstNode>();
  private ShadowStack shadowStack = new ShadowStack(itemStack);

  public void push(AstNode statementItem) {
    shadowStack.push(statementItem);
    itemStack.push(statementItem);
  }

  public AstNode pop() {
    shadowStack.pop();
    return itemStack.pop();
  }

  public int size() {
    return itemStack.size();
  }

  public AstNode peek() {
    return itemStack.peek();
  }

  public IfStatement getIfInElseCase() {
    return shadowStack.getIfInElseCase();
  }

  public IfStatement getIf() {
    return shadowStack.getIf();
  }

  public DoStatement getDo() {
    return shadowStack.getDo();
  }

  public TryStatement getTry() {
    return shadowStack.getTry();
  }
}
