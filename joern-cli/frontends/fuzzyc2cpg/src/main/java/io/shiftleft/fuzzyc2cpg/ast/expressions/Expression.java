package io.shiftleft.fuzzyc2cpg.ast.expressions;

import io.shiftleft.fuzzyc2cpg.ast.AstNode;
import io.shiftleft.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

public class Expression extends AstNode {

  private String operator = "";

  public Expression() {
  }

  public Expression(Expression other) {
    super(other);
    setOperator(other.operator);
  }

  public void replaceFirstChild(AstNode node) {
    children.set(0, node);
  }

  public String getOperator() {
    return operator;
  }

  public void setOperator(String text) {
    operator = text;
  }

  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
