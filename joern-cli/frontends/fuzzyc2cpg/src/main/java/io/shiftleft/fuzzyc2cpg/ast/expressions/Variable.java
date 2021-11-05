package io.shiftleft.fuzzyc2cpg.ast.expressions;

import io.shiftleft.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

public class Variable extends Expression {

  private Expression name = null;

  public Expression getNameExpression() {
    return this.name;
  }

  public void setNameExpression(Expression name) {
    this.name = name;
    super.addChild(name);
  }

  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
