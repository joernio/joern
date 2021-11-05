package io.shiftleft.fuzzyc2cpg.parser.functions.builder;

import io.shiftleft.fuzzyc2cpg.ast.AstNode;
import io.shiftleft.fuzzyc2cpg.ast.langc.statements.blockstarters.IfStatement;
import io.shiftleft.fuzzyc2cpg.ast.logical.statements.CompoundStatement;
import io.shiftleft.fuzzyc2cpg.ast.statements.blockstarters.DoStatement;
import io.shiftleft.fuzzyc2cpg.ast.statements.blockstarters.TryStatement;
import java.util.EmptyStackException;
import java.util.Stack;

public class ShadowStack {

  private Stack<StackItem> stack = new Stack<StackItem>();
  private Stack<AstNode> itemStack;

  public ShadowStack(Stack<AstNode> aItemStack) {
    itemStack = aItemStack;
  }

  public void push(AstNode statementItem) {
    if (statementItem instanceof IfStatement
        || statementItem instanceof DoStatement
        || statementItem instanceof TryStatement) {
      AstNode parentCompound = parentCompoundFromItemStack(itemStack);

      stack.push(new StackItem(statementItem, parentCompound));
    }
  }

  public void pop() {
    AstNode topOfItemStack = itemStack.peek();

    while (stack.size() > 0
        && stack.peek().parentCompound == topOfItemStack) {
      stack.pop();
    }
  }

  public IfStatement getIfInElseCase() {
    if (stack.size() < 2) {
      return null;
    }

    StackItem topItem = stack.pop();
    StackItem returnItem = stack.pop();
    stack.push(topItem);
    return (IfStatement) returnItem.ifOrDoOrTry;
  }

  public IfStatement getIf() {
    IfStatement retval;
    StackItem item = null;

    try {
      item = stack.pop();
      retval = (IfStatement) item.ifOrDoOrTry;
    } catch (EmptyStackException ex) {
      return null;
    } catch (ClassCastException ex) {
      stack.push(item);
      return null;
    }

    return retval;
  }

  public DoStatement getDo() {
    DoStatement retval;
    StackItem item = null;

    try {
      item = stack.pop();
      retval = (DoStatement) item.ifOrDoOrTry;

      if (itemStack.contains(retval)) {
        stack.push(item);
        return null;
      }

    } catch (EmptyStackException ex) {
      return null;
    } catch (ClassCastException ex) {
      stack.push(item);
      return null;
    }

    return retval;
  }

  private AstNode parentCompoundFromItemStack(Stack<AstNode> itemStack) {
    // Watchout: we are assuming that this function is never
    // called when 0 compound statements are on the stack.
    // If this ever happens, null is returned.

    AstNode parentCompound = null;
    // walk stack from top to bottom
    for (int i = itemStack.size() - 1; i >= 0; i--) {
      if (itemStack.get(i) instanceof CompoundStatement) {
        parentCompound = itemStack.get(i);
        break;
      }
    }
    return parentCompound;
  }

  public TryStatement getTry() {
    TryStatement retval;
    StackItem item = null;

    try {
      // keep try statement on stack for further catch expressions
      item = stack.peek();
      retval = (TryStatement) item.ifOrDoOrTry;

      if (itemStack.contains(retval)) {
        stack.push(item);
        return null;
      }

    } catch (EmptyStackException ex) {
      return null;
    } catch (ClassCastException ex) {
      stack.push(item);
      return null;
    }

    return retval;
  }

  private class StackItem {

    public AstNode parentCompound;
    public AstNode ifOrDoOrTry;

    public StackItem(AstNode item, AstNode parent) {
      ifOrDoOrTry = item;
      parentCompound = parent;
    }

  }

}
